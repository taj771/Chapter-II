"""
Potato extension: simulate irrigation levels 150-300mm for years 2031, 2044, 2050.
Purpose:
  - 150mm needed for rigid-allocation baseline (current files only have 0,10,50,90,130,170)
  - 200-300mm needed for flexible three-crop allocation search space

Extends existing CanDCS-U6 potato files with 4 new irrigation levels:
  150, 200, 250, 300 mm
for both OriPrcp and RedPrcp scenarios, all three representative years.

Output: merged CSV appended to existing potato files in DBFS.

SETUP: weather files already on DBFS from previous runs.
  dbfs:/FileStore/WEP/CanDCSU6_weather_OriPrcp/
  dbfs:/FileStore/WEP/CanDCSU6_weather_RedPrcp/
"""

import sys as _sys
try:
    SCENARIO = dbutils.widgets.get("scenario")
except Exception:
    SCENARIO = _sys.argv[1] if len(_sys.argv) > 1 else "OriPrcp"

assert SCENARIO in ("OriPrcp", "RedPrcp"), f"Invalid scenario: {SCENARIO}"

# ── TUNING ────────────────────────────────────────────────────────────────────

N_STARTS   = 3
MAX_FUN    = 60
BATCH_SIZE = 16

# ── PATHS ─────────────────────────────────────────────────────────────────────

DBFS_WEATHER_DIR = f"/dbfs/FileStore/WEP/CanDCSU6_weather_{SCENARIO}"
DBFS_OUTPUT_DIR  = f"/dbfs/FileStore/WEP/CanDCSU6/{SCENARIO}"
CKPT_BASE        = f"/dbfs/FileStore/WEP/checkpoints/CanDCSU6_{SCENARIO}_potato_ext300"

# ── IMPORTS ───────────────────────────────────────────────────────────────────

import os, gc, math, signal
import pandas as pd
import numpy as np
from scipy.optimize import fmin
from aquacrop import AquaCropModel, Soil, Crop, InitialWaterContent, IrrigationManagement
from aquacrop.utils import prepare_weather

os.makedirs(DBFS_OUTPUT_DIR, exist_ok=True)
os.makedirs(CKPT_BASE, exist_ok=True)

def _log(msg): print(msg, flush=True)

TASK_TIMEOUT = 180

def _timeout_handler(signum, frame):
    raise TimeoutError("AquaCrop timeout")

# ── CROP CONFIG ───────────────────────────────────────────────────────────────

SOIL_TYPE = "LoamySand"

POTATO = dict(
    crop_type='Potato',
    planting_date='05/01',
    harvest_date='11/30',
    WP=15,
    HI0=0.60,
    CCx=0.85,
    CGC=0.14336,
    CDC=0.08,
    Zmax=0.50,
    SeedSize=10,
    p_up1=0.25,
    p_lo1=0.55,
    p_up2=0.50,
    p_up3=0.85,
    p_up4=0.90,
    SxTopQ=0.048,
    SxBotQ=0.012,
    Emergence=22,
    MaxRooting=70,
    Senescence=110,
    Maturity=135,
)

YEARS          = [2031, 2044, 2050]
EXT_IRR_LEVELS = [150, 200, 250, 300]

_log(f"Potato extension: {SCENARIO}, years={YEARS}, levels={EXT_IRR_LEVELS}")

# ── MODEL HELPERS ─────────────────────────────────────────────────────────────

def make_crop(params):
    return Crop(params['crop_type'], **{k: v for k, v in params.items() if k != 'crop_type'})

def evaluate(smts, max_irr, wdf, year, planting_date, harvest_date, test=False):
    model = AquaCropModel(
        f"{year}/{planting_date}",
        f"{year}/{harvest_date}",
        wdf, Soil(SOIL_TYPE), make_crop(POTATO),
        irrigation_management=IrrigationManagement(
            irrigation_method=1, SMT=smts, MaxIrrSeason=max_irr),
        initial_water_content=InitialWaterContent(wc_type='Pct', value=[70]),
    )
    model.run_model(till_termination=True)
    out = model.get_simulation_results()
    if out is None or out.empty: return (0.0, 0.0) if test else 0.0
    yld  = out['Dry yield (tonne/ha)'].mean()
    tirr = out['Seasonal irrigation (mm)'].mean()
    return (yld, tirr) if test else -yld

def process_task(args):
    site_id, max_irr, year = args
    signal.signal(signal.SIGALRM, _timeout_handler)
    signal.alarm(TASK_TIMEOUT)
    try:
        wdf    = prepare_weather(os.path.join(DBFS_WEATHER_DIR, f"site_{site_id}_weather.txt"))
        x0list = np.random.rand(N_STARTS, 4) * 100
        losses = [evaluate(x, max_irr, wdf, year, POTATO['planting_date'], POTATO['harvest_date'])
                  for x in x0list]
        x0     = x0list[np.argmin(losses)]
        smts   = fmin(evaluate, x0,
                      args=(max_irr, wdf, year, POTATO['planting_date'], POTATO['harvest_date']),
                      disp=False, maxfun=MAX_FUN).reshape(4)
        yld, tirr = evaluate(smts, max_irr, wdf, year,
                             POTATO['planting_date'], POTATO['harvest_date'], test=True)
        signal.alarm(0)
        return {'Site_ID': site_id, 'Max_Irrigation_mm': max_irr,
                'Yield_tonne_per_ha': yld, 'Total_Irrigation_mm': tirr, 'Year': year}
    except (Exception, TimeoutError) as e:
        signal.alarm(0)
        _log(f"    site={site_id} max_irr={max_irr} year={year}: {e}")
        return None

# ── RUN ALL YEARS ─────────────────────────────────────────────────────────────

for YEAR in YEARS:
    _log(f"\n{'='*60}")
    _log(f"Year {YEAR}")

    existing_file = os.path.join(DBFS_OUTPUT_DIR, f"PotatoCanDCSU6_{SCENARIO}{YEAR}.csv")
    ckpt_dir      = os.path.join(CKPT_BASE, str(YEAR))
    os.makedirs(ckpt_dir, exist_ok=True)

    if os.path.exists(existing_file):
        existing  = pd.read_csv(existing_file)
        site_ids  = sorted(existing["Site_ID"].unique())
        _log(f"Existing: {len(existing)} rows, {len(site_ids)} sites")
    else:
        import glob
        weather_files = glob.glob(os.path.join(DBFS_WEATHER_DIR, "site_*_weather.txt"))
        site_ids = sorted([int(f.split("site_")[1].split("_")[0]) for f in weather_files])
        existing = None
        _log(f"No existing file — using {len(site_ids)} sites from weather dir")

    n_sites         = len(site_ids)
    n_batches_total = math.ceil(n_sites / BATCH_SIZE)
    site_batches    = [site_ids[i:i+BATCH_SIZE] for i in range(0, n_sites, BATCH_SIZE)]

    _log(f"Sites: {n_sites} | Batches: {n_batches_total} | New irr levels: {EXT_IRR_LEVELS}")

    ext_rows = []

    for batch_idx, batch_sites in enumerate(site_batches):
        ckpt = os.path.join(ckpt_dir, f"potato_{SCENARIO}_{YEAR}_ext_batch{batch_idx:04d}.csv")

        if os.path.exists(ckpt):
            _log(f"[SKIP] year={YEAR} batch={batch_idx+1}/{n_batches_total}")
            ext_rows.append(pd.read_csv(ckpt))
            continue

        n_tasks = len(batch_sites) * len(EXT_IRR_LEVELS)
        _log(f"[RUNNING] year={YEAR} batch={batch_idx+1}/{n_batches_total} "
             f"sites {batch_sites[0]}-{batch_sites[-1]} — {n_tasks} tasks")

        tasks   = [(sid, mx, YEAR) for sid in batch_sites for mx in EXT_IRR_LEVELS]
        results = (sc.parallelize(tasks, numSlices=len(tasks))
                     .map(process_task)
                     .collect())
        rows = [r for r in results if r is not None]

        if rows:
            df_b = pd.DataFrame(rows)
            df_b.to_csv(ckpt, index=False)
            ext_rows.append(df_b)
            _log(f"[SAVED] year={YEAR} batch={batch_idx+1} → {len(df_b)} rows")

        gc.collect()

    # ── merge with existing and save ──────────────────────────────────────────

    if ext_rows:
        df_ext = pd.concat(ext_rows, ignore_index=True)
        df_ext = df_ext.drop(columns=['Year'], errors='ignore')
        _log(f"\nExtension rows for {YEAR}: {len(df_ext)}")
        _log(f"New levels: {sorted(df_ext['Max_Irrigation_mm'].unique())}")

        if existing is not None:
            existing_clean = existing[~existing['Max_Irrigation_mm'].isin(EXT_IRR_LEVELS)]
            merged = pd.concat([existing_clean, df_ext], ignore_index=True)
            merged = merged.sort_values(['Site_ID', 'Max_Irrigation_mm']).reset_index(drop=True)
        else:
            merged = df_ext

        out_file = existing_file if existing is not None else \
                   os.path.join(DBFS_OUTPUT_DIR, f"PotatoCanDCSU6_{SCENARIO}{YEAR}.csv")
        merged.to_csv(out_file, index=False)
        _log(f"[MERGED] {YEAR}: {len(merged)} total rows → {out_file}")
        _log(f"Levels now: {sorted(merged['Max_Irrigation_mm'].unique())}")
    else:
        _log(f"No extension rows for {YEAR}.")

_log(f"\nPotato extension {SCENARIO} complete.")
