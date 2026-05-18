"""
Databricks marginal AquaCrop simulations — DBFS version.
Reads ERA5 climate input from DBFS, writes output CSVs to DBFS.

SETUP:
  1. Upload 3 ERA5 files to DBFS (run once in terminal):
       databricks fs cp "/path/to/Data/ERA5/era5_daily_et0.csv"      dbfs:/FileStore/WEP/ERA5/era5_daily_et0.csv
       databricks fs cp "/path/to/Data/ERA5/era5_soil_types.csv"     dbfs:/FileStore/WEP/ERA5/era5_soil_types.csv
       databricks fs cp "/path/to/Data/ERA5/era5_planting_dates.csv" dbfs:/FileStore/WEP/ERA5/era5_planting_dates.csv
  2. Install aquacrop: add "%pip install aquacrop" as first cell, run once
  3. Run this script via VS Code Databricks extension → "Run on Cluster"

DOWNLOAD OUTPUTS AFTER RUN:
  databricks fs cp -r "dbfs:/FileStore/WEP/Data Main Analysis/" "./Data Main Analysis/"
"""

# ── INSTALL (first run only — uncomment, run, then comment out again) ─────────
# %pip install aquacrop

# ── DBFS PATHS ────────────────────────────────────────────────────────────────
DBFS_INPUT_CSV   = "/dbfs/FileStore/WEP/era5_daily_et0.csv"
DBFS_SOIL_CSV    = "/dbfs/FileStore/WEP/era5_soil_types.csv"
DBFS_PLANT_CSV   = "/dbfs/FileStore/WEP/era5_planting_dates.csv"
DBFS_WEATHER_DIR = "/dbfs/FileStore/WEP/weather"
DBFS_OUTPUT_DIR  = "/dbfs/FileStore/WEP/Data Main Analysis"

# ── TUNING ────────────────────────────────────────────────────────────────────
N_STARTS    = 4           # random starting points (agent-validated: 3 too thin, 4 safe for 4D Nelder-Mead)
MAX_FUN     = 60          # Nelder-Mead max evaluations — ~35% faster than 75, acceptable accuracy
BATCH_SIZE  = 16          # sites per Spark batch — 16×10=160 tasks = exactly 10 rounds of 16 cores

CROP_FILTER = None        # None = run all crops sequentially on single cluster

# ── IMPORTS ───────────────────────────────────────────────────────────────────
import os, gc, math, signal
import pandas as pd
import numpy as np
from scipy.optimize import fmin
from aquacrop import AquaCropModel, Soil, Crop, InitialWaterContent, IrrigationManagement
from aquacrop.utils import prepare_weather

def _log(msg):
    print(msg, flush=True)

TASK_TIMEOUT = 180   # seconds — hung AquaCrop task killed after 3 min

def _timeout_handler(signum, frame):
    raise TimeoutError("AquaCrop task timeout")

os.makedirs(DBFS_WEATHER_DIR, exist_ok=True)
os.makedirs(DBFS_OUTPUT_DIR,  exist_ok=True)

# ── CROP PARAMS ───────────────────────────────────────────────────────────────

YEARS     = list(range(2018, 2024))
SOIL_TYPE = "LoamySand"

WHEAT = dict(
    crop_type='Wheat', planting_date='05/01', harvest_date='10/30',
    CropType=3, Tbase=5, Tupp=35, Zmax=0.7, WP=10,
    Tmin_up=8, Tmax_lo=40, exc=50, CGC=0.16764, CCx=0.95,
    CDC=0.13653, Kcb=1.10, fshape_r=15, SxTopQ=0.020,
    SxBotQ=0.005, p_up4=0.8, p_up2=0.55, fshape_w1=4, PlantPop=2000000,
)
CANOLA = dict(
    crop_type='Sunflower', planting_date='05/01', harvest_date='10/30',
    CropType=3, Tbase=5, Tupp=35, Zmax=0.7, WP=14,
    Tmin_up=8, Tmax_lo=40, exc=50, CGC=0.16764, CCx=0.95,
    CDC=0.13653, SeedSize=5, Kcb=1.10, fshape_r=15,
    SxTopQ=0.020, SxBotQ=0.005, p_up4=0.8, p_up2=0.55, fshape_w1=4,
)
POTATO = dict(
    crop_type='Potato', planting_date='05/01', harvest_date='11/30',
    WP=15, HI0=0.60, CCx=0.85, CGC=0.14336, CDC=0.08, Zmax=0.50,
    SeedSize=10, p_up1=0.25, p_lo1=0.55, p_up2=0.50, p_up3=0.85,
    p_up4=0.90, SxTopQ=0.048, SxBotQ=0.012, Emergence=22,
    MaxRooting=70, Senescence=110, Maturity=135, HIstart=75,
    dHI_pre=0.10, fshape_w2=8, dHI0=0.10,
)
MAX_IRR_LEVELS = list(range(10, 210, 20))          # [10,30,...,190] — 10 points
MAX_IRR_POTATO = list(range(10, 270, 40))          # [10,50,...,250] — 7 points (was 13)

_ALL_CONFIGS = [
    ('wheat',  WHEAT,  MAX_IRR_LEVELS),
    ('canola', CANOLA, MAX_IRR_LEVELS),
    ('potato', POTATO, MAX_IRR_POTATO),
]
CROP_CONFIGS = [c for c in _ALL_CONFIGS if CROP_FILTER is None or c[0] == CROP_FILTER]
print(f"Running crops: {[c[0] for c in CROP_CONFIGS]}")

# ── LOAD CLIMATE + WRITE WEATHER FILES ────────────────────────────────────────

print("Loading climate data from DBFS...")
climate = pd.read_csv(DBFS_INPUT_CSV, on_bad_lines='skip')
climate['Date'] = pd.to_datetime(climate['Date'], errors='coerce')
climate = climate.dropna(subset=['Date'])
climate['Day']   = climate['Date'].dt.day
climate['Month'] = climate['Date'].dt.month
climate['Year']  = climate['Date'].dt.year
# ERA5 file already contains ReferenceET computed with actual ERA5 wind speed (u2_ms).
# Do NOT overwrite with compute_et0() which uses hardcoded u=1.2.
# climate['ReferenceET'] = compute_et0(climate)   # disabled

col_map = {'MinTemp':'Tmin(c)', 'MaxTemp':'Tmax(c)',
           'Precipitation':'Prcp(mm)', 'ReferenceET':'Et0(mm)'}

print("Writing per-site weather files to DBFS...")
for site in climate['site'].unique():
    df_s = (climate[climate['site'] == site]
            [['Day','Month','Year','MinTemp','MaxTemp','Precipitation','ReferenceET']]
            .rename(columns=col_map))
    df_s.to_csv(os.path.join(DBFS_WEATHER_DIR, f"site_{site}_weather_data.txt"),
                sep='\t', index=False)

def _parse_site_id(f):
    try:
        return int(f.split('_')[1])
    except (ValueError, IndexError):
        return None

site_ids = sorted(
    _id for f in os.listdir(DBFS_WEATHER_DIR)
    if f.startswith('site_') and f.endswith('_weather_data.txt')
    for _id in [_parse_site_id(f)] if _id is not None
)
print(f"Found {len(site_ids)} sites")

# ── PER-SITE SOIL TYPES ───────────────────────────────────────────────────────
_SOIL_MAP = {
    'SiltyClayLoam': 'SiltClayLoam',
    'SiltyClay':     'SiltClay',
    'Silt':          'SiltLoam',
    'Sandy':         'LoamySand',
}
if os.path.exists(DBFS_SOIL_CSV):
    _sdf = pd.read_csv(DBFS_SOIL_CSV)[['grid_id', 'soil_type']]
    site_soil = dict(zip(_sdf['grid_id'], _sdf['soil_type']))
    print(f"Loaded per-site soil types ({len(site_soil)} sites)")
else:
    site_soil = {}
    print(f"Soil CSV not found — using uniform {SOIL_TYPE}")

# ── PER-SITE PLANTING DATES ───────────────────────────────────────────────────
if os.path.exists(DBFS_PLANT_CSV):
    _pdf = pd.read_csv(DBFS_PLANT_CSV)
    _pdf['wheat_plant']  = pd.to_datetime(_pdf['wheat_plant']).dt.strftime('%m/%d')
    _pdf['canola_plant'] = pd.to_datetime(_pdf['canola_plant']).dt.strftime('%m/%d')
    _pdf['potato_plant'] = pd.to_datetime(_pdf['potato_plant']).dt.strftime('%m/%d')
    plant_dates = {(r.site, r.year): {'wheat':  r.wheat_plant,
                                      'canola': r.canola_plant,
                                      'potato': r.potato_plant}
                   for _, r in _pdf.iterrows()}
    print(f"Loaded per-site planting dates ({len(plant_dates)} site-year combinations)")
else:
    plant_dates = {}
    print("Planting dates CSV not found — using fixed dates from crop params")

# ── MODEL HELPERS ─────────────────────────────────────────────────────────────

def make_crop(params):
    ctype = params['crop_type']
    return Crop(ctype, **{k: v for k, v in params.items() if k != 'crop_type'})

def evaluate(smts, max_irr, year, wdf, crop_params, soil_name, test=False):
    p, h = crop_params['planting_date'], crop_params['harvest_date']
    model = AquaCropModel(
        f"{year}/{p}", f"{year}/{h}", wdf,
        Soil(soil_name), make_crop(crop_params),
        irrigation_management=IrrigationManagement(
            irrigation_method=1, SMT=smts, MaxIrrSeason=max_irr),
        initial_water_content=InitialWaterContent(wc_type='Pct', value=[70]),
    )
    model.run_model(till_termination=True)
    out = model.get_simulation_results()
    yield_col = ('Fresh yield (tonne/ha)' if crop_params['crop_type'] == 'Potato'
                 else 'Dry yield (tonne/ha)')
    if out is None or out.empty or yield_col not in out.columns:
        return (0.0, 0.0) if test else 0.0
    yld  = out[yield_col].mean()
    tirr = out['Seasonal irrigation (mm)'].mean()
    return (yld, tirr) if test else -yld

def process_task(args):
    site_id, max_irr, year, crop_params, crop_name, weather_dir = args
    signal.signal(signal.SIGALRM, _timeout_handler)
    signal.alarm(TASK_TIMEOUT)
    try:
        wdf = prepare_weather(os.path.join(weather_dir, f"site_{site_id}_weather_data.txt"))
        raw_soil  = site_soil.get(site_id, SOIL_TYPE)
        soil_name = _SOIL_MAP.get(raw_soil, raw_soil)
        site_dates  = plant_dates.get((site_id, year), {})
        p_date      = site_dates.get(crop_name, crop_params['planting_date'])
        params_local = {**crop_params, 'planting_date': p_date}
        x0list = np.random.rand(N_STARTS, 4) * 100
        losses = [evaluate(x, max_irr, year, wdf, params_local, soil_name) for x in x0list]
        x0   = x0list[np.argmin(losses)]
        smts = fmin(evaluate, x0, args=(max_irr, year, wdf, params_local, soil_name),
                    disp=False, maxfun=MAX_FUN).reshape(4)
        yld, tirr = evaluate(smts, max_irr, year, wdf, params_local, soil_name, test=True)
        signal.alarm(0)
        return {'Site_ID': site_id, 'Max_Irrigation_mm': max_irr,
                'Yield_tonne_per_ha': yld, 'Total_Irrigation_mm': tirr}
    except (Exception, TimeoutError) as e:
        signal.alarm(0)
        print(f"    site={site_id} max_irr={max_irr} year={year}: {e}")
        return None

# ── SPARK RUN — sequential crops, FIFO scheduler ─────────────────────────────
# Sequential crops: FIFO gives all 16 cores to one crop at a time (fastest on single cluster).
# BATCH_SIZE=16 → 16×10=160 tasks → exactly 10 rounds of 16 parallel tasks, zero waste.
# Checkpoint per batch; on restart, completed batches are skipped automatically.

import glob as _glob

CKPT_DIR = "/dbfs/FileStore/WEP/checkpoints"   # no-space path — survives cluster restart reliably
os.makedirs(CKPT_DIR, exist_ok=True)
print(f"Spark scheduler mode: {sc.getConf().get('spark.scheduler.mode', 'FIFO (default)')}", flush=True)

n_sites         = len(site_ids)
n_batches_total = math.ceil(n_sites / BATCH_SIZE)

def run_crop(crop_name, crop_params, irr_levels):
    site_batches = [site_ids[i:i+BATCH_SIZE] for i in range(0, n_sites, BATCH_SIZE)]

    for year in YEARS:
        year_rows      = []
        n_batches_done = 0

        for batch_idx, batch_sites in enumerate(site_batches):
            ckpt = os.path.join(CKPT_DIR, f"{crop_name}_{year}_batch{batch_idx:04d}.csv")

            if os.path.exists(ckpt):
                _log(f"[SKIP]    {crop_name} year={year} "
                     f"batch={batch_idx+1}/{n_batches_total} "
                     f"(sites {batch_sites[0]}–{batch_sites[-1]})")
                year_rows.append(pd.read_csv(ckpt))
                n_batches_done += 1
                continue

            n_tasks = len(batch_sites) * len(irr_levels)
            _log(f"[RUNNING] {crop_name} year={year} "
                 f"batch={batch_idx+1}/{n_batches_total} "
                 f"sites {batch_sites[0]}–{batch_sites[-1]} "
                 f"— {n_tasks} tasks ({len(batch_sites)} sites × {len(irr_levels)} irr levels)")

            tasks = [
                (sid, mx, year, crop_params, crop_name, DBFS_WEATHER_DIR)
                for sid in batch_sites
                for mx  in irr_levels
            ]

            results = (sc.parallelize(tasks, numSlices=len(tasks))
                         .map(process_task)
                         .collect())

            rows = [r for r in results if r is not None]
            if not rows:
                _log(f"[WARN]    {crop_name} year={year} batch={batch_idx+1} — all tasks failed, skipping")
            if rows:
                df_batch = pd.DataFrame(rows)
                df_batch.to_csv(ckpt, index=False)
                year_rows.append(df_batch)
                n_batches_done += 1
                _log(f"[SAVED]   {crop_name} year={year} "
                     f"batch={batch_idx+1} → {len(df_batch)} rows → {ckpt}")

            gc.collect()

        if year_rows:
            df_year  = pd.concat(year_rows, ignore_index=True)
            out_file = os.path.join(
                DBFS_OUTPUT_DIR,
                f"merged_simulation_results_{crop_name}_marginal_{year}_irrigation.csv"
            )
            df_year.to_csv(out_file, index=False)
            _log(f"[MERGED]  {crop_name} year={year} "
                 f"→ {len(df_year)} rows ({n_batches_done}/{n_batches_total} batches) → {out_file}")

    _log(f"[DONE]    {crop_name} — all years complete")

# Sequential: FIFO scheduler gives all 16 cores to each crop in turn (faster than threading)
for name, params, levels in CROP_CONFIGS:
    _log(f"[START] {name}")
    run_crop(name, params, levels)

print("All marginal simulations complete.")
print(f"Outputs in DBFS: {DBFS_OUTPUT_DIR}")
print("Download with: databricks fs cp -r 'dbfs:/FileStore/WEP/Data Main Analysis/' './Data Main Analysis/'")
