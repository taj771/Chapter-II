"""
CMIP245 all-crop (SSP2-4.5) marginal simulations — 20% reduced precipitation.
Replaces: 12. Climate_chnage_CMIP245_PrcpRed.py

Primary output for R analysis (code_part7_option2.R reads these files).

Outputs (one CSV per crop per year, 2030-2050):
  {ONEDRIVE}/WheatCMIP245/RedPrcp/WheatCMIP245_RedPrcp{year}.csv
  {ONEDRIVE}/CanolaCMIP245/RedPrcp/CanolaCMIP245_RedPrcp{year}.csv
  {ONEDRIVE}/PotatoCMIP245/RedPrcp/PotataoCMIP245_RedPrcp{year}.csv
  Columns: Site_ID, Max_Irrigation_mm, Yield_tonne_per_ha, Total_Irrigation_mm

Input: CMIP245_20redPrcp_ET.csv  (20% precipitation reduction scenario)
"""
import os, gc, sys
import pandas as pd
import numpy as np
from joblib import Parallel, delayed
from scipy.optimize import fmin
from aquacrop import AquaCropModel, Soil, Crop, InitialWaterContent, IrrigationManagement
from aquacrop.utils import prepare_weather

sys.path.insert(0, os.path.dirname(__file__))
from config import WHEAT, CANOLA, POTATO, SOIL_TYPE

# ── paths ─────────────────────────────────────────────────────────────────────
ONEDRIVE_BASE = (
    "/Users/tharakajayalath/Library/CloudStorage"
    "/OneDrive-UniversityofSaskatchewan"
    "/Chapter II-IrrigationValue/Chapter-II/AquaCropOPSyData"
)
CLIMATE_CSV = os.path.join(
    ONEDRIVE_BASE,
    "ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP245_20redPrcp_ET.csv"
)
WEATHER_DIR = os.path.join(ONEDRIVE_BASE, "ClimateData/CMIP_Weather_245_RedPrcp")

# ── settings ──────────────────────────────────────────────────────────────────
YEARS = list(range(2030, 2051))

CROP_CONFIGS = [
    # (name, params, irr_levels, yield_col, out_subdir, file_prefix)
    ('wheat',  WHEAT,  [0] + list(range(10, 210, 10)), 'Dry yield (tonne/ha)',
     'WheatCMIP245/RedPrcp',  'WheatCMIP245_RedPrcp'),
    ('canola', CANOLA, [0] + list(range(10, 210, 10)), 'Dry yield (tonne/ha)',
     'CanolaCMIP245/RedPrcp', 'CanolaCMIP245_RedPrcp'),
    ('potato', POTATO, [0] + list(range(10, 270, 10)), 'Fresh yield (tonne/ha)',
     'PotatoCMIP245/RedPrcp', 'PotataoCMIP245_RedPrcp'),
]

# ── helpers ───────────────────────────────────────────────────────────────────

def compute_et0(df):
    T_mean = (df['MaxTemp'] + df['MinTemp']) / 2
    e_s    = 0.6108 * np.exp(17.27 * T_mean / (T_mean + 237.3))
    delta  = 4098 * e_s / (T_mean + 237.3) ** 2
    gamma, u = 0.066, 2.0
    return (0.408 * delta * df['R_n'] +
            gamma * 900 / (T_mean + 273) * u * (e_s - df['e_a'])) / \
           (delta + gamma * (1 + 0.34 * u))

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
    yld  = out[yield_col].mean()
    tirr = out['Seasonal irrigation (mm)'].mean()
    return (yld, tirr) if test else -yld

def process_site(site_id, max_irr, year, crop_params, yield_col):
    try:
        wdf    = prepare_weather(os.path.join(WEATHER_DIR, f"site_{site_id}_weather.txt"))
        x0list = np.random.rand(10, 4) * 100
        losses = [evaluate(x, max_irr, year, wdf, crop_params, yield_col) for x in x0list]
        x0     = x0list[np.argmin(losses)]
        smts   = fmin(evaluate, x0, args=(max_irr, year, wdf, crop_params, yield_col),
                      disp=False, maxfun=150).squeeze()
        yld, tirr = evaluate(smts, max_irr, year, wdf, crop_params, yield_col, test=True)
        return {'Site_ID': site_id, 'Max_Irrigation_mm': max_irr,
                'Yield_tonne_per_ha': yld, 'Total_Irrigation_mm': tirr}
    except Exception as e:
        print(f"  site={site_id} max_irr={max_irr} year={year}: {e}")
        return None

# ── load climate and write per-site weather files ─────────────────────────────

print("Loading CMIP245 climate data (20% reduced precipitation)...")
climate = pd.read_csv(CLIMATE_CSV, on_bad_lines='skip')
climate['Date'] = pd.to_datetime(climate['Date'], errors='coerce')
climate = climate.dropna(subset=['Date'])
climate['Day']   = climate['Date'].dt.day
climate['Month'] = climate['Date'].dt.month
climate['Year']  = climate['Date'].dt.year
climate['ReferenceET'] = compute_et0(climate)

os.makedirs(WEATHER_DIR, exist_ok=True)

col_map = {'MinTemp': 'Tmin(c)', 'MaxTemp': 'Tmax(c)',
           'Precipitation': 'Prcp(mm)', 'ReferenceET': 'Et0(mm)'}
print("Writing per-site weather files (reduced precip)...")
for site in climate['site'].unique():
    df_s = (climate[climate['site'] == site]
            [['Day','Month','Year','MinTemp','MaxTemp','Precipitation','ReferenceET']]
            .rename(columns=col_map))
    df_s.to_csv(os.path.join(WEATHER_DIR, f"site_{site}_weather.txt"), sep='\t', index=False)

site_ids = sorted(climate['site'].unique())
print(f"{len(site_ids)} sites, years {YEARS[0]}-{YEARS[-1]}")

# ── run simulations ───────────────────────────────────────────────────────────

for crop_name, crop_params, irr_levels, yield_col, out_subdir, file_prefix in CROP_CONFIGS:
    out_dir = os.path.join(ONEDRIVE_BASE, out_subdir)
    os.makedirs(out_dir, exist_ok=True)

    for year in YEARS:
        print(f"[{crop_name} CMIP245 RedPrcp] year={year} — "
              f"{len(site_ids)} sites × {len(irr_levels)} irr levels...")
        rows = Parallel(n_jobs=-1, backend='loky')(
            delayed(process_site)(sid, mx, year, crop_params, yield_col)
            for mx in irr_levels
            for sid in site_ids
        )
        rows = [r for r in rows if r is not None]
        if rows:
            df = pd.DataFrame(rows)
            out_file = os.path.join(out_dir, f"{file_prefix}{year}.csv")
            df.to_csv(out_file, index=False)
            print(f"  → {len(df)} rows → {file_prefix}{year}.csv")
        gc.collect()

print("All CMIP245 RedPrcp simulations complete.")
print("R analysis input ready for code_part7_option2.R")
