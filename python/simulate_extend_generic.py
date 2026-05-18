"""
Generic AquaCrop extension script for CanDCS-U6 marginal simulations.

Usage (Databricks widgets):
  scenario : OriPrcp | RedPrcp
  crop     : wheat | canola | potato
  years    : comma-separated, e.g. "2031,2044,2050"
  levels   : comma-separated mm, e.g. "170,190,210,250,300"

Extends existing per-year CSVs with new irrigation levels, merges back.
Checkpoints per year per batch for fault tolerance.
"""

import sys as _sys

def _get(key, default):
    try:    return dbutils.widgets.get(key)
    except: pass
    for arg in _sys.argv[1:]:
        if arg.startswith(key + "="):
            return arg.split("=", 1)[1]
    return default

SCENARIO = _get("scenario", "OriPrcp")
CROP     = _get("crop",     "canola").lower()
YEARS    = [int(y) for y in _get("years",  "2031").split(",")]
LEVELS   = [int(x) for x in _get("levels", "210,250,300").split(",")]

assert SCENARIO in ("OriPrcp", "RedPrcp")
assert CROP in ("wheat", "canola", "potato")

# ── TUNING ────────────────────────────────────────────────────────────────────

N_STARTS   = 3
MAX_FUN    = 60
BATCH_SIZE = 16
TASK_TIMEOUT = 180

# ── PATHS ─────────────────────────────────────────────────────────────────────

DBFS_WEATHER_DIR = f"/dbfs/FileStore/WEP/CanDCSU6_weather_{SCENARIO}"
DBFS_OUTPUT_DIR  = f"/dbfs/FileStore/WEP/CanDCSU6/{SCENARIO}"
CKPT_BASE        = f"/dbfs/FileStore/WEP/checkpoints/CanDCSU6_{SCENARIO}_{CROP}_ext"

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

SOIL_TYPE = "LoamySand"

# ── CROP CONFIGS ──────────────────────────────────────────────────────────────

CROP_CONFIGS = {
    "wheat": dict(
        crop_type='Wheat', planting_date='05/01', harvest_date='09/30',
        WP=15, HI0=0.48, CCx=0.90, CGC=0.01663, CDC=0.01118, Zmax=1.5,
        SeedSize=0.0026, PlantPop=4500000,
        p_up1=0.20, p_lo1=0.65, p_up2=0.60, p_up3=0.75, p_up4=0.85,
        SxTopQ=0.048, SxBotQ=0.012,
        Emergence=7, MaxRooting=56, Senescence=107, Maturity=132,
    ),
    "canola": dict(
        crop_type='Sunflower', planting_date='05/01', harvest_date='10/30',
        CropType=3, Tbase=5, Tupp=35, Zmax=0.7, WP=14,
        Tmin_up=8, Tmax_lo=40, exc=50, CGC=0.16764, CCx=0.95,
        CDC=0.13653, SeedSize=5, Kcb=1.10, fshape_r=15,
        SxTopQ=0.020, SxBotQ=0.005, p_up4=0.8, p_up2=0.55, fshape_w1=4,
    ),
    "potato": dict(
        crop_type='Potato', planting_date='05/01', harvest_date='11/30',
        WP=15, HI0=0.60, CCx=0.85, CGC=0.14336, CDC=0.08, Zmax=0.50,
        SeedSize=10,
        p_up1=0.25, p_lo1=0.55, p_up2=0.50, p_up3=0.85, p_up4=0.90,
        SxTopQ=0.048, SxBotQ=0.012,
        Emergence=22, MaxRooting=70, Senescence=110, Maturity=135,
    ),
}

FNAME_PREFIX = {"wheat": "Wheat", "canola": "Canola", "potato": "Potato"}

cfg = CROP_CONFIGS[CROP]
PLANTING = cfg['planting_date']
HARVEST  = cfg['harvest_date']

_log(f"Generic extension: crop={CROP} scenario={SCENARIO} years={YEARS} levels={LEVELS}")

# ── MODEL HELPERS ─────────────────────────────────────────────────────────────

def make_crop(params):
    return Crop(params['crop_type'], **{k: v for k, v in params.items() if k != 'crop_type'})

def evaluate(smts, max_irr, wdf, year, test=False):
    model = AquaCropModel(
        f"{year}/{PLANTING}", f"{year}/{HARVEST}",
        wdf, Soil(SOIL_TYPE), make_crop(cfg),
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

def _timeout_handler(signum, frame):
    raise TimeoutError("AquaCrop timeout")

# ── RUN ALL YEARS ─────────────────────────────────────────────────────────────

for YEAR in YEARS:
    _log(f"\n{'='*60}\n{CROP.title()} {SCENARIO} year={YEAR} levels={LEVELS}")

    prefix        = FNAME_PREFIX[CROP]
    existing_file = os.path.join(DBFS_OUTPUT_DIR,
                                 f"{prefix}CanDCSU6_{SCENARIO}{YEAR}.csv")
    ckpt_dir      = os.path.join(CKPT_BASE, str(YEAR))
    os.makedirs(ckpt_dir, exist_ok=True)

    if os.path.exists(existing_file):
        existing = pd.read_csv(existing_file)
        site_ids = sorted(existing["Site_ID"].unique())
        _log(f"Existing: {len(existing)} rows, {len(site_ids)} sites")
        _log(f"Existing levels: {sorted(existing['Max_Irrigation_mm'].unique())}")
    else:
        import glob
        wfiles = glob.glob(os.path.join(DBFS_WEATHER_DIR, "site_*_weather.txt"))
        site_ids = sorted([int(f.split("site_")[1].split("_")[0]) for f in wfiles])
        existing = None
        _log(f"No existing file — {len(site_ids)} sites from weather dir")

    n_batches_total = math.ceil(len(site_ids) / BATCH_SIZE)
    site_batches    = [site_ids[i:i+BATCH_SIZE] for i in range(0, len(site_ids), BATCH_SIZE)]

    ext_rows = []

    for batch_idx, batch_sites in enumerate(site_batches):
        lvl_str = "_".join(map(str, LEVELS))
        ckpt = os.path.join(ckpt_dir,
               f"{CROP}_{SCENARIO}_{YEAR}_ext{lvl_str}_batch{batch_idx:04d}.csv")

        if os.path.exists(ckpt):
            _log(f"[SKIP] year={YEAR} batch={batch_idx+1}/{n_batches_total}")
            ext_rows.append(pd.read_csv(ckpt))
            continue

        n_tasks = len(batch_sites) * len(LEVELS)
        _log(f"[RUNNING] year={YEAR} batch={batch_idx+1}/{n_batches_total} "
             f"sites {batch_sites[0]}-{batch_sites[-1]} — {n_tasks} tasks")

        tasks   = [(sid, mx, YEAR) for sid in batch_sites for mx in LEVELS]
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

    if ext_rows:
        df_ext = pd.concat(ext_rows, ignore_index=True)
        _log(f"Extension rows year={YEAR}: {len(df_ext)}")

        if existing is not None:
            clean = existing[~existing['Max_Irrigation_mm'].isin(LEVELS)]
            merged = pd.concat([clean, df_ext], ignore_index=True)
        else:
            merged = df_ext

        merged = merged.sort_values(['Site_ID', 'Max_Irrigation_mm']).reset_index(drop=True)
        merged.to_csv(existing_file, index=False)
        _log(f"[MERGED] year={YEAR}: {len(merged)} rows → {existing_file}")
        _log(f"Levels now: {sorted(merged['Max_Irrigation_mm'].unique())}")
    else:
        _log(f"No extension rows for year={YEAR}.")

_log(f"\n{CROP.title()} {SCENARIO} extension complete.")
