"""
Average AquaCrop simulations for wheat, canola, and potato (2018-2023).
Replaces notebooks 01.Wheat Average, 02.Canola Average, 03.Potato Average.

Outputs (written to OUTPUT_DIR):
  wheat_rainfed_{year}.csv        ← code_part3.R, code_part4.R
  wheat_netirridemand_{year}.csv  ← code_part3.R, code_part4.R
  canola_rainfed_{year}.csv
  canola_netirridemand_{year}.csv
  potato_netirridemand_{year}.csv (no rainfed baseline for potato)
"""
import os
import gc
import pandas as pd
import numpy as np
from joblib import Parallel, delayed
from aquacrop import AquaCropModel, Soil, Crop, InitialWaterContent, IrrigationManagement
from aquacrop.utils import prepare_weather

from config import (BASE_DIR, CLIMATE_CSV, WEATHER_DIR, OUTPUT_DIR,
                    YEARS, SOIL_TYPE, CROPS,
                    PRECIP_START_MONTH, PRECIP_END_MONTH)

os.makedirs(WEATHER_DIR, exist_ok=True)
os.makedirs(OUTPUT_DIR,  exist_ok=True)

# ── ET₀ calculation (Penman-Monteith) ────────────────────────────────────────

def compute_et0(df):
    T_max, T_min = df['MaxTemp'], df['MinTemp']
    T_mean = (T_max + T_min) / 2
    e_s    = 0.6108 * np.exp(17.27 * T_mean / (T_mean + 237.3))
    e_a    = df['e_a']
    R_n    = df['R_n']
    delta  = 4098 * e_s / (T_mean + 237.3) ** 2
    gamma  = 0.066
    u      = 1.2
    return (0.408 * delta * R_n + gamma * 900 / (T_mean + 273) * u * (e_s - e_a)) / \
           (delta + gamma * (1 + 0.34 * u))

print("Loading climate data...")
climate = pd.read_csv(CLIMATE_CSV, on_bad_lines='skip')
climate['Date'] = pd.to_datetime(climate['Date'], errors='coerce')
climate = climate.dropna(subset=['Date'])
climate['Day']   = climate['Date'].dt.day
climate['Month'] = climate['Date'].dt.month
climate['Year']  = climate['Date'].dt.year
climate['ReferenceET'] = compute_et0(climate)

# ── write per-site weather files (all years combined) ────────────────────────

print("Writing per-site weather files...")
col_map = {'MinTemp': 'Tmin(c)', 'MaxTemp': 'Tmax(c)',
           'Precipitation': 'Prcp(mm)', 'ReferenceET': 'Et0(mm)'}
for site in climate['site'].unique():
    df_site = (climate[climate['site'] == site]
               [['Day', 'Month', 'Year', 'MinTemp', 'MaxTemp', 'Precipitation', 'ReferenceET']]
               .rename(columns=col_map))
    df_site.to_csv(os.path.join(WEATHER_DIR, f"site_{site}_weather_data.txt"),
                   sep='\t', index=False)

unique_sites = climate['site'].unique()

# Pre-compute growing-season precip per (site, year) — avoids global DataFrame
# access inside parallel workers
print("Pre-computing precipitation totals...")
precip_lookup = {}
for site in unique_sites:
    for year in YEARS:
        mask = ((climate['site'] == site) &
                (climate['Year'] == year) &
                (climate['Month'].between(PRECIP_START_MONTH, PRECIP_END_MONTH)))
        precip_lookup[(site, year)] = climate.loc[mask, 'Precipitation'].sum()

# ── simulation helpers ────────────────────────────────────────────────────────

def make_crop(params):
    ctype = params['crop_type']
    kw = {k: v for k, v in params.items() if k != 'crop_type'}
    return Crop(ctype, **kw)

def run_site(site, year, irr_method, smt, crop_params, precip_total):
    weather_file = os.path.join(WEATHER_DIR, f"site_{site}_weather_data.txt")
    wdf = prepare_weather(weather_file)

    if irr_method == 0:
        irr_mngt = IrrigationManagement(irrigation_method=0)
    else:
        irr_mngt = IrrigationManagement(irrigation_method=4, NetIrrSMT=smt)

    crop   = make_crop(crop_params)
    soil   = Soil(SOIL_TYPE)
    initWC = InitialWaterContent(value=['FC'])

    model = AquaCropModel(
        f'{year}/01/01', f'{year}/12/30',
        wdf, soil, crop,
        initial_water_content=initWC,
        irrigation_management=irr_mngt,
    )
    model.run_model(till_termination=True)

    df = model._outputs.final_stats.copy()
    df['Site'] = site
    df['Total_Precipitation(mm)'] = precip_total
    return df

def simulate_year(crop_name, crop_params, year, irr_method, smt=70):
    print(f"  {crop_name} year={year} irr_method={irr_method}...")

    results = Parallel(n_jobs=-1, backend='loky', verbose=0)(
        delayed(run_site)(
            site, year, irr_method, smt, crop_params,
            precip_lookup.get((site, year), 0.0)
        )
        for site in unique_sites
        if os.path.exists(os.path.join(WEATHER_DIR, f"site_{site}_weather_data.txt"))
    )

    frames = [r for r in results if r is not None]
    gc.collect()
    return pd.concat(frames, ignore_index=True) if frames else pd.DataFrame()

# ── main loop ─────────────────────────────────────────────────────────────────

CROP_CONFIGS = [
    ('wheat',  CROPS['wheat'],  True),
    ('canola', CROPS['canola'], True),
    ('potato', CROPS['potato'], False),
]

for crop_name, crop_params, has_rainfed in CROP_CONFIGS:
    for year in YEARS:
        df_irr = simulate_year(crop_name, crop_params, year, irr_method=4)
        df_irr.to_csv(os.path.join(OUTPUT_DIR, f"{crop_name}_netirridemand_{year}.csv"),
                      index=False)

        if has_rainfed:
            df_rf = simulate_year(crop_name, crop_params, year, irr_method=0)
            df_rf.to_csv(os.path.join(OUTPUT_DIR, f"{crop_name}_rainfed_{year}.csv"),
                         index=False)

print("All average simulations complete.")
