"""
CMIP245 CanESM5 marginal simulations — 20% reduced precipitation.
ET0 uses actual CanESM5 sfcWind (FAO-56 PM with observed wind), consistent with ERA5.

Run download_cmip6_canesm5.py first to generate CMIP245_ET_canesm5_20redPrcp.csv.

Outputs (one CSV per crop per year, 2030-2050):
  {ONEDRIVE}/WheatCMIP245/CanESM5_RedPrcp/WheatCMIP245_CanESM5_RedPrcp{year}.csv
  {ONEDRIVE}/CanolaCMIP245/CanESM5_RedPrcp/CanolaCMIP245_CanESM5_RedPrcp{year}.csv
  {ONEDRIVE}/PotatoCMIP245/CanESM5_RedPrcp/PotataoCMIP245_CanESM5_RedPrcp{year}.csv
  Columns: Site_ID, Max_Irrigation_mm, Yield_tonne_per_ha, Total_Irrigation_mm
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
    "ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP245_ET_canesm5_20redPrcp.csv"
)
WEATHER_DIR = os.path.join(ONEDRIVE_BASE, "ClimateData/CMIP_CanESM5_Weather_RedPrcp")

# ── settings ──────────────────────────────────────────────────────────────────
YEARS = list(range(2030, 2051))
N_JOBS = max(1, os.cpu_count() - 1)

CROP_CONFIGS = [
    # (name, params, irr_levels, yield_col, out_subdir, file_prefix)
    ('wheat',  WHEAT,  [0] + list(range(10, 210, 10)), 'Dry yield (tonne/ha)',
     'WheatCMIP245/CanESM5_RedPrcp',  'WheatCMIP245_CanESM5_RedPrcp'),
    ('canola', CANOLA, [0] + list(range(10, 210, 10)), 'Dry yield (tonne/ha)',
     'CanolaCMIP245/CanESM5_RedPrcp', 'CanolaCMIP245_CanESM5_RedPrcp'),
    ('potato', POTATO, [0] + list(range(10, 270, 10)), 'Fresh yield (tonne/ha)',
     'PotatoCMIP245/CanESM5_RedPrcp', 'PotataoCMIP245_CanESM5_RedPrcp'),
]

# ── helpers ───────────────────────────────────────────────────────────────────

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

if not os.path.exists(CLIMATE_CSV):
    print(f"ERROR: Climate file not found:\n  {CLIMATE_CSV}")
    print("Run download_cmip6_canesm5.py first.")
    sys.exit(1)

print("Loading CanESM5 CMIP245 climate data (20% reduced precipitation)...")
climate = pd.read_csv(CLIMATE_CSV, parse_dates=["Date"])
climate = climate.dropna(subset=["Date"])
climate["Day"]   = climate["Date"].dt.day
climate["Month"] = climate["Date"].dt.month
climate["Year"]  = climate["Date"].dt.year

# ReferenceET is pre-computed in download_cmip6_canesm5.py using actual sfcWind
# Do NOT overwrite with fixed-wind compute_et0() — that was the original inconsistency
assert "ReferenceET" in climate.columns, "Missing ReferenceET in climate CSV"
assert "u2_ms" in climate.columns, "Missing u2_ms — run download_cmip6_canesm5.py first"

print(f"  Sites: {climate['site'].nunique()}")
print(f"  Years: {climate['Year'].min()}-{climate['Year'].max()}")
gs = climate[climate["Month"].between(5, 8)]
print(f"  Growing season ET0 mean: {gs['ReferenceET'].mean():.2f} mm/day")
print(f"  Growing season wind u2 mean: {gs['u2_ms'].mean():.2f} m/s")

os.makedirs(WEATHER_DIR, exist_ok=True)

col_map = {"MinTemp": "Tmin(c)", "MaxTemp": "Tmax(c)",
           "Precipitation": "Prcp(mm)", "ReferenceET": "Et0(mm)"}
print("Writing per-site weather files...")
for site in sorted(climate["site"].unique()):
    df_s = (climate[climate["site"] == site]
            [["Day", "Month", "Year", "MinTemp", "MaxTemp",
              "Precipitation", "ReferenceET"]]
            .rename(columns=col_map))
    df_s.to_csv(os.path.join(WEATHER_DIR, f"site_{site}_weather.txt"),
                sep="\t", index=False)

site_ids = sorted(climate["site"].unique())
print(f"{len(site_ids)} sites, years {YEARS[0]}-{YEARS[-1]}")

# ── run simulations ────────────────────────────────────────────────────────────

for crop_name, crop_params, irr_levels, yield_col, out_subdir, file_prefix in CROP_CONFIGS:
    out_dir = os.path.join(ONEDRIVE_BASE, out_subdir)
    os.makedirs(out_dir, exist_ok=True)

    for yr in YEARS:
        out_file = os.path.join(out_dir, f"{file_prefix}{yr}.csv")
        if os.path.exists(out_file):
            print(f"  Skip (exists): {os.path.basename(out_file)}")
            continue

        print(f"\n[{crop_name}] year={yr} — {len(site_ids)} sites × {len(irr_levels)} levels")
        tasks = [(sid, irr, yr, crop_params, yield_col)
                 for sid in site_ids for irr in irr_levels]

        results = Parallel(n_jobs=N_JOBS, verbose=5)(
            delayed(process_site)(*t) for t in tasks
        )
        results = [r for r in results if r is not None]

        df_yr = pd.DataFrame(results)
        df_yr.to_csv(out_file, index=False)
        print(f"  Saved: {out_file} ({len(df_yr)} rows)")
        gc.collect()

print("\nAll simulations complete.")
print("Next: update generate_figure8_climate_reallocation_option2.R to use CanESM5_RedPrcp paths.")
