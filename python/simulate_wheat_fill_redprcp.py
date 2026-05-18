"""
RedPrcp wheat gap fill: simulate 170mm and 190mm for years 2031, 2044, 2050.
Purpose: original RedPrcp wheat only went to 150mm; OriPrcp went to 190mm.
2044 most critical: wheat still strongly responsive above 150mm (+48% yield).
"""

import sys as _sys
try:
    SCENARIO = dbutils.widgets.get("scenario")
except Exception:
    SCENARIO = "RedPrcp"

assert SCENARIO == "RedPrcp", "This script is RedPrcp only"

N_STARTS   = 3
MAX_FUN    = 60
BATCH_SIZE = 16

DBFS_WEATHER_DIR = f"/dbfs/FileStore/WEP/CanDCSU6_weather_{SCENARIO}"
DBFS_OUTPUT_DIR  = f"/dbfs/FileStore/WEP/CanDCSU6/{SCENARIO}"
CKPT_BASE        = f"/dbfs/FileStore/WEP/checkpoints/CanDCSU6_{SCENARIO}_wheat_fill170190"

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

SOIL_TYPE = "LoamySand"

WHEAT = dict(
    crop_type='Wheat',
    planting_date='05/01',
    harvest_date='09/30',
    WP=15,
    HI0=0.48,
    CCx=0.90,
    CGC=0.01663,
    CDC=0.01118,
    Zmax=1.5,
    SeedSize=0.0026,
    PlantPop=4500000,
    p_up1=0.20,
    p_lo1=0.65,
    p_up2=0.60,
    p_up3=0.75,
    p_up4=0.85,
    SxTopQ=0.048,
    SxBotQ=0.012,
    Emergence=7,
    MaxRooting=56,
    Senescence=107,
    Maturity=132,
)

YEARS       = [2031, 2044, 2050]
FILL_LEVELS = [170, 190]

_log(f"RedPrcp wheat fill: years={YEARS}, levels={FILL_LEVELS}")

def make_crop(params):
    return Crop(params['crop_type'], **{k: v for k, v in params.items() if k != 'crop_type'})

def evaluate(smts, max_irr, wdf, year, test=False):
    model = AquaCropModel(
        f"{year}/{WHEAT['planting_date']}", f"{year}/{WHEAT['harvest_date']}",
        wdf, Soil(SOIL_TYPE), make_crop(WHEAT),
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
        losses = [evaluate(x, max_irr, wdf, year) for x in x0list]
        x0     = x0list[np.argmin(losses)]
        smts   = fmin(evaluate, x0, args=(max_irr, wdf, year),
                      disp=False, maxfun=MAX_FUN).reshape(4)
        yld, tirr = evaluate(smts, max_irr, wdf, year, test=True)
        signal.alarm(0)
        return {'Site_ID': site_id, 'Max_Irrigation_mm': max_irr,
                'Yield_tonne_per_ha': yld, 'Total_Irrigation_mm': tirr}
    except (Exception, TimeoutError) as e:
        signal.alarm(0)
        _log(f"    site={site_id} max_irr={max_irr} year={year}: {e}")
        return None

for YEAR in YEARS:
    _log(f"\n{'='*60}\nYear {YEAR}")

    existing_file = os.path.join(DBFS_OUTPUT_DIR, f"WheatCanDCSU6_{SCENARIO}{YEAR}.csv")
    ckpt_dir      = os.path.join(CKPT_BASE, str(YEAR))
    os.makedirs(ckpt_dir, exist_ok=True)

    existing = pd.read_csv(existing_file)
    site_ids = sorted(existing["Site_ID"].unique())
    _log(f"Existing: {len(existing)} rows, {len(site_ids)} sites")
    _log(f"Existing levels: {sorted(existing['Max_Irrigation_mm'].unique())}")

    n_batches_total = math.ceil(len(site_ids) / BATCH_SIZE)
    site_batches    = [site_ids[i:i+BATCH_SIZE] for i in range(0, len(site_ids), BATCH_SIZE)]

    fill_rows = []

    for batch_idx, batch_sites in enumerate(site_batches):
        ckpt = os.path.join(ckpt_dir, f"wheat_fill_{YEAR}_batch{batch_idx:04d}.csv")

        if os.path.exists(ckpt):
            _log(f"[SKIP] year={YEAR} batch={batch_idx+1}/{n_batches_total}")
            fill_rows.append(pd.read_csv(ckpt))
            continue

        n_tasks = len(batch_sites) * len(FILL_LEVELS)
        _log(f"[RUNNING] year={YEAR} batch={batch_idx+1}/{n_batches_total} — {n_tasks} tasks")

        tasks   = [(sid, mx, YEAR) for sid in batch_sites for mx in FILL_LEVELS]
        results = (sc.parallelize(tasks, numSlices=len(tasks))
                     .map(process_task)
                     .collect())
        rows = [r for r in results if r is not None]

        if rows:
            df_b = pd.DataFrame(rows)
            df_b.to_csv(ckpt, index=False)
            fill_rows.append(df_b)
            _log(f"[SAVED] year={YEAR} batch={batch_idx+1} → {len(df_b)} rows")

        gc.collect()

    if fill_rows:
        df_fill = pd.concat(fill_rows, ignore_index=True)
        existing_clean = existing[~existing['Max_Irrigation_mm'].isin(FILL_LEVELS)]
        merged = pd.concat([existing_clean, df_fill], ignore_index=True)
        merged = merged.sort_values(['Site_ID', 'Max_Irrigation_mm']).reset_index(drop=True)
        merged.to_csv(existing_file, index=False)
        _log(f"[MERGED] {YEAR}: {len(merged)} total rows")
        _log(f"Levels now: {sorted(merged['Max_Irrigation_mm'].unique())}")
    else:
        _log(f"No fill rows for {YEAR}.")

_log(f"\nRedPrcp wheat fill complete.")
