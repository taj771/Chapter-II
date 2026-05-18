"""
CanDCS-U6 marginal AquaCrop simulations on Databricks (Spark).
Runs OriPrcp or RedPrcp scenario — set SCENARIO below.

SETUP (run once in terminal):
  # Upload climate CSV to DBFS:
  databricks fs cp \
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/Chapter-II/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP245_ET_candscu6_CanESM5.csv" \
    dbfs:/FileStore/WEP/CMIP245_ET_candscu6_CanESM5.csv

  databricks fs cp \
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/Chapter-II/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP245_ET_candscu6_CanESM5_20redPrcp.csv" \
    dbfs:/FileStore/WEP/CMIP245_ET_candscu6_CanESM5_20redPrcp.csv

  # Install aquacrop (first cell in notebook, run once):
  # %pip install aquacrop

DOWNLOAD OUTPUTS:
  databricks fs cp -r dbfs:/FileStore/WEP/CanDCSU6/ ./CanDCSU6_results/
"""

# ── CONFIGURE THIS BEFORE RUNNING ─────────────────────────────────────────────

import sys as _sys
try:
    SCENARIO = dbutils.widgets.get("scenario")  # notebook widget
except Exception:
    SCENARIO = _sys.argv[1] if len(_sys.argv) > 1 else "OriPrcp"

assert SCENARIO in ("OriPrcp", "RedPrcp"), f"Invalid scenario: {SCENARIO}"

# ── TUNING ────────────────────────────────────────────────────────────────────

N_STARTS   = 2    # random starting points for Nelder-Mead
MAX_FUN    = 40   # max function evaluations per optimisation
BATCH_SIZE = 16   # sites per Spark batch

# ── DERIVED PATHS ─────────────────────────────────────────────────────────────

_CSV_MAP = {
    "OriPrcp": "/dbfs/FileStore/WEP/CMIP245_ET_candscu6_CanESM5.csv",
    "RedPrcp": "/dbfs/FileStore/WEP/CMIP245_ET_candscu6_CanESM5_20redPrcp.csv",
}
DBFS_INPUT_CSV   = _CSV_MAP[SCENARIO]
DBFS_WEATHER_DIR = f"/dbfs/FileStore/WEP/CanDCSU6_weather_{SCENARIO}"
DBFS_OUTPUT_DIR  = f"/dbfs/FileStore/WEP/CanDCSU6/{SCENARIO}"
CKPT_DIR         = f"/dbfs/FileStore/WEP/checkpoints/CanDCSU6_{SCENARIO}"

# ── IMPORTS ───────────────────────────────────────────────────────────────────

import os, gc, math, signal
import pandas as pd
import numpy as np
from scipy.optimize import fmin
from aquacrop import AquaCropModel, Soil, Crop, InitialWaterContent, IrrigationManagement
from aquacrop.utils import prepare_weather

for d in [DBFS_WEATHER_DIR, DBFS_OUTPUT_DIR, CKPT_DIR]:
    os.makedirs(d, exist_ok=True)

def _log(msg): print(msg, flush=True)

TASK_TIMEOUT = 180

def _timeout_handler(signum, frame):
    raise TimeoutError("AquaCrop timeout")

# ── CROP CONFIG ───────────────────────────────────────────────────────────────

YEARS     = [2031, 2044, 2050]  # P5/P50/P100 GS-precip quantiles: driest/average/wettest
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

CROP_CONFIGS = [
    # (name, params, irr_levels, yield_col, out_prefix)
    # Irrigation brackets match marginal script: step 20 wheat/canola, step 40 potato
    ('wheat',  WHEAT,  [0] + list(range(10, 160, 20)), 'Dry yield (tonne/ha)',
     f'WheatCanDCSU6_{SCENARIO}'),
    ('canola', CANOLA, [0] + list(range(10, 160, 20)), 'Dry yield (tonne/ha)',
     f'CanolaCanDCSU6_{SCENARIO}'),
    ('potato', POTATO, [0] + list(range(10, 210, 40)), 'Fresh yield (tonne/ha)',
     f'PotatoCanDCSU6_{SCENARIO}'),
]

# ── LOAD CLIMATE + WRITE WEATHER FILES ────────────────────────────────────────

_log(f"Loading {SCENARIO} climate data from DBFS...")
climate = pd.read_csv(DBFS_INPUT_CSV, parse_dates=["Date"])
climate = climate.dropna(subset=["Date"])

assert "ReferenceET" in climate.columns, \
    "Missing ReferenceET — upload CMIP245_ET_candscu6_CanESM5*.csv to DBFS"

_log(f"  Sites: {climate['site'].nunique()} | "
     f"Years: {climate['Year'].min()}-{climate['Year'].max()}")
gs = climate[climate["Month"].between(5, 8)]
_log(f"  GS ET0: {gs['ReferenceET'].mean():.2f} mm/day | "
     f"GS Prcp: {gs['Precipitation'].mean():.2f} mm/day")

col_map = {"MinTemp": "Tmin(c)", "MaxTemp": "Tmax(c)",
           "Precipitation": "Prcp(mm)", "ReferenceET": "Et0(mm)"}

_log("Writing per-site weather files to DBFS...")
for site in sorted(climate["site"].unique()):
    df_s = (climate[climate["site"] == site]
            [["Day", "Month", "Year", "MinTemp", "MaxTemp", "Precipitation", "ReferenceET"]]
            .rename(columns=col_map))
    df_s.to_csv(os.path.join(DBFS_WEATHER_DIR, f"site_{site}_weather.txt"),
                sep="\t", index=False)

site_ids = sorted(climate["site"].unique())
_log(f"{len(site_ids)} sites written")

# ── MODEL HELPERS ─────────────────────────────────────────────────────────────

def make_crop(params):
    return Crop(params['crop_type'], **{k: v for k, v in params.items() if k != 'crop_type'})

def evaluate(smts, max_irr, year, wdf, crop_params, yield_col, test=False):
    model = AquaCropModel(
        f"{year}/{crop_params['planting_date']}", f"{year}/{crop_params['harvest_date']}",
        wdf, Soil(SOIL_TYPE), make_crop(crop_params),
        irrigation_management=IrrigationManagement(
            irrigation_method=1, SMT=smts, MaxIrrSeason=max_irr),
        initial_water_content=InitialWaterContent(wc_type='Pct', value=[70]),
    )
    model.run_model(till_termination=True)
    out  = model.get_simulation_results()
    if out is None or out.empty: return (0.0, 0.0) if test else 0.0
    yld  = out[yield_col].mean()
    tirr = out['Seasonal irrigation (mm)'].mean()
    return (yld, tirr) if test else -yld

def process_task(args):
    site_id, max_irr, year, crop_params, yield_col, weather_dir = args
    signal.signal(signal.SIGALRM, _timeout_handler)
    signal.alarm(TASK_TIMEOUT)
    try:
        wdf    = prepare_weather(os.path.join(weather_dir, f"site_{site_id}_weather.txt"))
        x0list = np.random.rand(N_STARTS, 4) * 100
        losses = [evaluate(x, max_irr, year, wdf, crop_params, yield_col) for x in x0list]
        x0     = x0list[np.argmin(losses)]
        smts   = fmin(evaluate, x0, args=(max_irr, year, wdf, crop_params, yield_col),
                      disp=False, maxfun=MAX_FUN).reshape(4)
        yld, tirr = evaluate(smts, max_irr, year, wdf, crop_params, yield_col, test=True)
        signal.alarm(0)
        return {'Site_ID': site_id, 'Max_Irrigation_mm': max_irr,
                'Yield_tonne_per_ha': yld, 'Total_Irrigation_mm': tirr}
    except (Exception, TimeoutError) as e:
        signal.alarm(0)
        _log(f"    site={site_id} max_irr={max_irr} year={year}: {e}")
        return None

# ── SPARK RUN ─────────────────────────────────────────────────────────────────

import glob as _glob

n_sites         = len(site_ids)
n_batches_total = math.ceil(n_sites / BATCH_SIZE)
_log(f"Spark mode: {sc.getConf().get('spark.scheduler.mode', 'FIFO')}")

def run_crop(crop_name, crop_params, irr_levels, yield_col, out_prefix):
    site_batches = [site_ids[i:i+BATCH_SIZE] for i in range(0, n_sites, BATCH_SIZE)]

    for year in YEARS:
        out_file       = os.path.join(DBFS_OUTPUT_DIR, f"{out_prefix}{year}.csv")
        year_rows      = []
        n_batches_done = 0

        for batch_idx, batch_sites in enumerate(site_batches):
            ckpt = os.path.join(CKPT_DIR, f"{crop_name}_{SCENARIO}_{year}_batch{batch_idx:04d}.csv")

            if os.path.exists(ckpt):
                _log(f"[SKIP]    {crop_name} {SCENARIO} year={year} "
                     f"batch={batch_idx+1}/{n_batches_total} "
                     f"(sites {batch_sites[0]}–{batch_sites[-1]})")
                year_rows.append(pd.read_csv(ckpt))
                n_batches_done += 1
                continue

            n_tasks = len(batch_sites) * len(irr_levels)
            _log(f"[RUNNING] {crop_name} {SCENARIO} year={year} "
                 f"batch={batch_idx+1}/{n_batches_total} "
                 f"sites {batch_sites[0]}–{batch_sites[-1]} "
                 f"— {n_tasks} tasks ({len(batch_sites)} sites × {len(irr_levels)} irr levels)")

            tasks = [(sid, mx, year, crop_params, yield_col, DBFS_WEATHER_DIR)
                     for sid in batch_sites for mx in irr_levels]

            results = (sc.parallelize(tasks, numSlices=len(tasks))
                         .map(process_task)
                         .collect())
            rows = [r for r in results if r is not None]

            if not rows:
                _log(f"[WARN]    {crop_name} year={year} batch={batch_idx+1} — all tasks failed, skipping")
            if rows:
                df_b = pd.DataFrame(rows)
                df_b.to_csv(ckpt, index=False)
                year_rows.append(df_b)
                n_batches_done += 1
                _log(f"[SAVED]   {crop_name} {SCENARIO} year={year} "
                     f"batch={batch_idx+1} → {len(df_b)} rows → {os.path.basename(ckpt)}")

            gc.collect()

        if year_rows:
            df_yr = pd.concat(year_rows, ignore_index=True)
            df_yr.to_csv(out_file, index=False)
            _log(f"[MERGED]  {crop_name} {SCENARIO} year={year} "
                 f"→ {len(df_yr)} rows ({n_batches_done}/{n_batches_total} batches) → {out_file}")

    _log(f"[DONE]    {crop_name} {SCENARIO} — all years complete")

for crop_name, crop_params, irr_levels, yield_col, out_prefix in CROP_CONFIGS:
    run_crop(crop_name, crop_params, irr_levels, yield_col, out_prefix)

_log(f"\nAll CanDCS-U6 {SCENARIO} simulations complete.")
_log(f"Download: databricks fs cp -r dbfs:/FileStore/WEP/CanDCSU6/ ./CanDCSU6_results/")
