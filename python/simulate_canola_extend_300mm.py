"""
Canola extension: simulate irrigation levels 210-300mm for year 2031 only.
Purpose: close data gap where canola yield still rising at 190mm in driest year.

Extends existing CanDCSU6 canola files with 5 new irrigation levels:
  210, 230, 250, 270, 300 mm
for both OriPrcp and RedPrcp scenarios, year=2031 only.

Output: merged CSV appended to existing canola files in DBFS.

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
CKPT_DIR         = f"/dbfs/FileStore/WEP/checkpoints/CanDCSU6_{SCENARIO}_ext300"

# ── IMPORTS ───────────────────────────────────────────────────────────────────

import os, gc, math, signal
import pandas as pd
import numpy as np
from scipy.optimize import fmin
from aquacrop import AquaCropModel, Soil, Crop, InitialWaterContent, IrrigationManagement
from aquacrop.utils import prepare_weather

for d in [DBFS_OUTPUT_DIR, CKPT_DIR]:
    os.makedirs(d, exist_ok=True)

def _log(msg): print(msg, flush=True)

TASK_TIMEOUT = 180

def _timeout_handler(signum, frame):
    raise TimeoutError("AquaCrop timeout")

# ── CROP CONFIG ───────────────────────────────────────────────────────────────

YEAR = 2031
SOIL_TYPE = "LoamySand"

CANOLA = dict(
    crop_type='Sunflower', planting_date='05/01', harvest_date='10/30',
    CropType=3, Tbase=5, Tupp=35, Zmax=0.7, WP=14,
    Tmin_up=8, Tmax_lo=40, exc=50, CGC=0.16764, CCx=0.95,
    CDC=0.13653, SeedSize=5, Kcb=1.10, fshape_r=15,
    SxTopQ=0.020, SxBotQ=0.005, p_up4=0.8, p_up2=0.55, fshape_w1=4,
)

# Extension levels only — existing data already covers 0-190mm
EXT_IRR_LEVELS = [210, 230, 250, 270, 300]

_log(f"Canola extension: {SCENARIO}, year={YEAR}, levels={EXT_IRR_LEVELS}")

# ── LOAD EXISTING CANOLA FILE (to get site list) ──────────────────────────────

existing_file = os.path.join(DBFS_OUTPUT_DIR, f"CanolaCanDCSU6_{SCENARIO}{YEAR}.csv")
if os.path.exists(existing_file):
    existing = pd.read_csv(existing_file)
    site_ids = sorted(existing["Site_ID"].unique())
    _log(f"Existing canola file: {len(existing)} rows, {len(site_ids)} sites")
else:
    # Fall back: scan weather dir for available sites
    import glob
    weather_files = glob.glob(os.path.join(DBFS_WEATHER_DIR, "site_*_weather.txt"))
    site_ids = sorted([int(f.split("site_")[1].split("_")[0]) for f in weather_files])
    existing = None
    _log(f"No existing canola file — using {len(site_ids)} sites from weather dir")

# ── MODEL HELPERS ─────────────────────────────────────────────────────────────

def make_crop(params):
    return Crop(params['crop_type'], **{k: v for k, v in params.items() if k != 'crop_type'})

def evaluate(smts, max_irr, wdf, test=False):
    model = AquaCropModel(
        f"{YEAR}/{CANOLA['planting_date']}", f"{YEAR}/{CANOLA['harvest_date']}",
        wdf, Soil(SOIL_TYPE), make_crop(CANOLA),
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
    site_id, max_irr = args
    signal.signal(signal.SIGALRM, _timeout_handler)
    signal.alarm(TASK_TIMEOUT)
    try:
        wdf    = prepare_weather(os.path.join(DBFS_WEATHER_DIR, f"site_{site_id}_weather.txt"))
        x0list = np.random.rand(N_STARTS, 4) * 100
        losses = [evaluate(x, max_irr, wdf) for x in x0list]
        x0     = x0list[np.argmin(losses)]
        smts   = fmin(evaluate, x0, args=(max_irr, wdf),
                      disp=False, maxfun=MAX_FUN).reshape(4)
        yld, tirr = evaluate(smts, max_irr, wdf, test=True)
        signal.alarm(0)
        return {'Site_ID': site_id, 'Max_Irrigation_mm': max_irr,
                'Yield_tonne_per_ha': yld, 'Total_Irrigation_mm': tirr}
    except (Exception, TimeoutError) as e:
        signal.alarm(0)
        _log(f"    site={site_id} max_irr={max_irr}: {e}")
        return None

# ── SPARK RUN ─────────────────────────────────────────────────────────────────

n_sites         = len(site_ids)
n_batches_total = math.ceil(n_sites / BATCH_SIZE)
site_batches    = [site_ids[i:i+BATCH_SIZE] for i in range(0, n_sites, BATCH_SIZE)]

_log(f"Sites: {n_sites} | Batches: {n_batches_total} | New irr levels: {EXT_IRR_LEVELS}")

ext_rows = []

for batch_idx, batch_sites in enumerate(site_batches):
    ckpt = os.path.join(CKPT_DIR, f"canola_{SCENARIO}_{YEAR}_ext_batch{batch_idx:04d}.csv")

    if os.path.exists(ckpt):
        _log(f"[SKIP] batch={batch_idx+1}/{n_batches_total}")
        ext_rows.append(pd.read_csv(ckpt))
        continue

    n_tasks = len(batch_sites) * len(EXT_IRR_LEVELS)
    _log(f"[RUNNING] batch={batch_idx+1}/{n_batches_total} "
         f"sites {batch_sites[0]}-{batch_sites[-1]} — {n_tasks} tasks")

    tasks = [(sid, mx) for sid in batch_sites for mx in EXT_IRR_LEVELS]
    results = (sc.parallelize(tasks, numSlices=len(tasks))
                 .map(process_task)
                 .collect())
    rows = [r for r in results if r is not None]

    if rows:
        df_b = pd.DataFrame(rows)
        df_b.to_csv(ckpt, index=False)
        ext_rows.append(df_b)
        _log(f"[SAVED] batch={batch_idx+1} → {len(df_b)} rows")

    gc.collect()

# ── MERGE WITH EXISTING AND SAVE ──────────────────────────────────────────────

if ext_rows:
    df_ext = pd.concat(ext_rows, ignore_index=True)
    _log(f"\nExtension rows: {len(df_ext)}")
    _log(f"New irrigation levels found: {sorted(df_ext['Max_Irrigation_mm'].unique())}")

    if existing is not None:
        # Remove any existing rows at extension levels (avoid duplicates)
        existing_clean = existing[~existing['Max_Irrigation_mm'].isin(EXT_IRR_LEVELS)]
        merged = pd.concat([existing_clean, df_ext], ignore_index=True)
        merged = merged.sort_values(['Site_ID', 'Max_Irrigation_mm']).reset_index(drop=True)
    else:
        merged = df_ext

    out_file = existing_file if existing is not None else \
               os.path.join(DBFS_OUTPUT_DIR, f"CanolaCanDCSU6_{SCENARIO}{YEAR}.csv")
    merged.to_csv(out_file, index=False)
    _log(f"\n[MERGED] {len(merged)} total rows → {out_file}")
    _log(f"Irrigation levels now: {sorted(merged['Max_Irrigation_mm'].unique())}")
else:
    _log("No extension rows produced.")

_log(f"\nCanola extension {SCENARIO} complete.")
_log(f"Download: databricks fs cp {out_file.replace('/dbfs','dbfs:')} ./")
