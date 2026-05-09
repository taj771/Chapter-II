"""
Databricks marginal AquaCrop simulations — DBFS version.
Reads input from DBFS, writes output CSVs to DBFS.

SETUP:
  1. Upload daymet_data_with_et0.csv to DBFS (see instructions below)
  2. Install aquacrop: add "%pip install aquacrop" as first cell, run once
  3. Run this script via VS Code Databricks extension → "Run on Cluster"

UPLOAD INPUT FILE (run once in terminal or Databricks CLI):
  databricks fs cp daymet_data_with_et0.csv dbfs:/aquacrop/input/daymet_data_with_et0.csv

DOWNLOAD OUTPUTS AFTER RUN:
  databricks fs cp -r dbfs:/aquacrop/output/ "./Data Main Analysis/"
"""

# ── INSTALL (first run only — uncomment, run, then comment out again) ─────────
# %pip install aquacrop

# ── DBFS PATHS ────────────────────────────────────────────────────────────────
DBFS_INPUT_CSV  = "/dbfs/aquacrop/input/daymet_data_with_et0.csv"
DBFS_WEATHER_DIR = "/dbfs/aquacrop/weather"
DBFS_OUTPUT_DIR  = "/dbfs/aquacrop/output"

# ── IMPORTS ───────────────────────────────────────────────────────────────────
import os, gc
import pandas as pd
import numpy as np
from scipy.optimize import fmin
from aquacrop import AquaCropModel, Soil, Crop, InitialWaterContent, IrrigationManagement
from aquacrop.utils import prepare_weather

os.makedirs(DBFS_WEATHER_DIR, exist_ok=True)
os.makedirs(DBFS_OUTPUT_DIR,  exist_ok=True)

# ── CROP PARAMS ───────────────────────────────────────────────────────────────

YEARS     = list(range(2018, 2024))
SOIL_TYPE = "LoamySand"

WHEAT = dict(
    crop_type='Wheat', planting_date='05/01', harvest_date='10/30',
    CropType=3, Tbase=5, Tupp=35, Zmax=0.7, WP=16,
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
MAX_IRR_LEVELS = list(range(10, 210, 20))
MAX_IRR_POTATO = list(range(10, 270, 20))

CROP_CONFIGS = [
    ('wheat',  WHEAT,  MAX_IRR_LEVELS),
    ('canola', CANOLA, MAX_IRR_LEVELS),
    ('potato', POTATO, MAX_IRR_POTATO),
]

# ── LOAD CLIMATE + WRITE WEATHER FILES ────────────────────────────────────────

def compute_et0(df):
    T_mean = (df['MaxTemp'] + df['MinTemp']) / 2
    e_s    = 0.6108 * np.exp(17.27 * T_mean / (T_mean + 237.3))
    delta  = 4098 * e_s / (T_mean + 237.3) ** 2
    gamma, u = 0.066, 1.2
    return (0.408 * delta * df['R_n'] +
            gamma * 900 / (T_mean + 273) * u * (e_s - df['e_a'])) / \
           (delta + gamma * (1 + 0.34 * u))

print("Loading climate data from DBFS...")
climate = pd.read_csv(DBFS_INPUT_CSV, on_bad_lines='skip')
climate['Date'] = pd.to_datetime(climate['Date'], errors='coerce')
climate = climate.dropna(subset=['Date'])
climate['Day']   = climate['Date'].dt.day
climate['Month'] = climate['Date'].dt.month
climate['Year']  = climate['Date'].dt.year
climate['ReferenceET'] = compute_et0(climate)

col_map = {'MinTemp':'Tmin(c)', 'MaxTemp':'Tmax(c)',
           'Precipitation':'Prcp(mm)', 'ReferenceET':'Et0(mm)'}

print("Writing per-site weather files to DBFS...")
for site in climate['site'].unique():
    df_s = (climate[climate['site'] == site]
            [['Day','Month','Year','MinTemp','MaxTemp','Precipitation','ReferenceET']]
            .rename(columns=col_map))
    df_s.to_csv(os.path.join(DBFS_WEATHER_DIR, f"site_{site}_weather_data.txt"),
                sep='\t', index=False)

site_ids = sorted(
    int(f.split('_')[1])
    for f in os.listdir(DBFS_WEATHER_DIR)
    if f.startswith('site_') and f.endswith('_weather_data.txt')
)
print(f"Found {len(site_ids)} sites")

# ── MODEL HELPERS ─────────────────────────────────────────────────────────────

def make_crop(params):
    ctype = params['crop_type']
    return Crop(ctype, **{k: v for k, v in params.items() if k != 'crop_type'})

def evaluate(smts, max_irr, year, wdf, crop_params, test=False):
    p, h = crop_params['planting_date'], crop_params['harvest_date']
    model = AquaCropModel(
        f"{year}/{p}", f"{year}/{h}", wdf,
        Soil(SOIL_TYPE), make_crop(crop_params),
        irrigation_management=IrrigationManagement(
            irrigation_method=1, SMT=smts, MaxIrrSeason=max_irr),
        initial_water_content=InitialWaterContent(wc_type='Pct', value=[70]),
    )
    model.run_model(till_termination=True)
    out = model.get_simulation_results()
    yield_col = ('Fresh yield (tonne/ha)' if crop_params['crop_type'] == 'Potato'
                 else 'Dry yield (tonne/ha)')
    yld  = out[yield_col].mean()
    tirr = out['Seasonal irrigation (mm)'].mean()
    return (yld, tirr) if test else -yld

def process_task(args):
    site_id, max_irr, year, crop_params, weather_dir = args
    try:
        wdf  = prepare_weather(os.path.join(weather_dir, f"site_{site_id}_weather_data.txt"))
        x0list = np.random.rand(10, 4) * 100
        losses = [evaluate(x, max_irr, year, wdf, crop_params) for x in x0list]
        x0   = x0list[np.argmin(losses)]
        smts = fmin(evaluate, x0, args=(max_irr, year, wdf, crop_params),
                    disp=False, maxfun=150).squeeze()
        yld, tirr = evaluate(smts, max_irr, year, wdf, crop_params, test=True)
        return {'Site_ID': site_id, 'Max_Irrigation_mm': max_irr,
                'Yield_tonne_per_ha': yld, 'Total_Irrigation_mm': tirr}
    except Exception as e:
        print(f"    site={site_id} max_irr={max_irr} year={year}: {e}")
        return None

# ── SPARK PARALLEL RUN ────────────────────────────────────────────────────────

for crop_name, crop_params, irr_levels in CROP_CONFIGS:
    for year in YEARS:
        print(f"[{crop_name}] year={year} — {len(site_ids)} sites × {len(irr_levels)} levels")

        tasks = [
            (sid, mx, year, crop_params, DBFS_WEATHER_DIR)
            for mx in irr_levels
            for sid in site_ids
        ]

        results = sc.parallelize(tasks, numSlices=len(tasks)) \
                    .map(process_task) \
                    .collect()

        rows = [r for r in results if r is not None]
        if rows:
            df = pd.DataFrame(rows)
            out_file = os.path.join(
                DBFS_OUTPUT_DIR,
                f"merged_simulation_results_{crop_name}_marginal_{year}_irrigation.csv"
            )
            df.to_csv(out_file, index=False)
            print(f"  → {len(df)} rows → {out_file}")

        gc.collect()

print("All marginal simulations complete.")
print(f"Outputs in DBFS: {DBFS_OUTPUT_DIR}")
print("Download with: databricks fs cp -r dbfs:/aquacrop/output/ './Data Main Analysis/'")
