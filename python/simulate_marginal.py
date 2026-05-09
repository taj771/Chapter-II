"""
Marginal AquaCrop simulations for wheat, canola, and potato (2018-2023).
Replaces notebooks 04.Wheat Marginal, 05.Canola Marginal, 06.Potato Marginal.

For each (crop, year, site, max_irr_level): optimizes soil moisture thresholds
(SMT) to maximize yield within the seasonal irrigation budget (MaxIrrSeason).

Output (one CSV per crop per year in OUTPUT_DIR):
  merged_simulation_results_{crop}_marginal_{year}_irrigation.csv
  Columns: Site_ID, Max_Irrigation_mm, Yield_tonne_per_ha, Total_Irrigation_mm

These CSVs are read by:
  code_part5.R  (Figure 5: marginal value curves)
  code_part6.R  (Figure 7: flexible allocation benefits)
"""
import os
import gc
import pandas as pd
import numpy as np
from joblib import Parallel, delayed
from scipy.optimize import fmin
from aquacrop import AquaCropModel, Soil, Crop, InitialWaterContent, IrrigationManagement
from aquacrop.utils import prepare_weather

from config import (WEATHER_DIR, OUTPUT_DIR, YEARS, SOIL_TYPE, CROPS,
                    MAX_IRR_LEVELS, MAX_IRR_POTATO)

# ── model runner ──────────────────────────────────────────────────────────────

def make_crop(params):
    ctype = params['crop_type']
    kw = {k: v for k, v in params.items() if k != 'crop_type'}
    return Crop(ctype, **kw)

def run_model(smts, max_irr_season, year, wdf, crop_params):
    crop   = make_crop(crop_params)
    soil   = Soil(SOIL_TYPE)
    initWC = InitialWaterContent(wc_type='Pct', value=[70])
    irr    = IrrigationManagement(irrigation_method=1, SMT=smts,
                                   MaxIrrSeason=max_irr_season)
    p_date = crop_params['planting_date']   # e.g. '05/01'
    h_date = crop_params['harvest_date']    # e.g. '10/30'

    model = AquaCropModel(
        f'{year}/{p_date}', f'{year}/{h_date}',
        wdf, soil, crop,
        irrigation_management=irr,
        initial_water_content=initWC,
    )
    model.run_model(till_termination=True)
    return model.get_simulation_results()

def evaluate(smts, max_irr_season, year, wdf, crop_params, test=False):
    out = run_model(smts, max_irr_season, year, wdf, crop_params)
    # Use Fresh yield for potato, Dry yield for grains
    yield_col = ('Fresh yield (tonne/ha)' if crop_params['crop_type'] == 'Potato'
                 else 'Dry yield (tonne/ha)')
    yld  = out[yield_col].mean()
    tirr = out['Seasonal irrigation (mm)'].mean()
    return (yld, tirr) if test else -yld

def get_starting_point(num_smts, max_irr, year, wdf, crop_params, num_searches=50):
    x0list = np.random.rand(num_searches, num_smts) * 100
    losses = [evaluate(x, max_irr, year, wdf, crop_params) for x in x0list]
    return x0list[np.argmin(losses)]

def optimize_smts(num_smts, max_irr, year, wdf, crop_params):
    x0  = get_starting_point(num_smts, max_irr, year, wdf, crop_params)
    res = fmin(evaluate, x0, args=(max_irr, year, wdf, crop_params), disp=False)
    return res.squeeze()

# ── per-site-per-level worker ─────────────────────────────────────────────────

def process_site_irr(site_id, max_irr, year, wdf, crop_params):
    try:
        smts = optimize_smts(4, max_irr, year, wdf, crop_params)
        yld, tirr = evaluate(smts, max_irr, year, wdf, crop_params, test=True)
        return {
            'Site_ID':           site_id,
            'Max_Irrigation_mm': max_irr,
            'Yield_tonne_per_ha': yld,
            'Total_Irrigation_mm': tirr,
        }
    except Exception as e:
        print(f"    site={site_id} max_irr={max_irr} year={year}: {e}")
        return None

# ── main loop ─────────────────────────────────────────────────────────────────

# Discover all sites from pre-written weather files
site_ids = sorted(
    int(f.split('_')[1])
    for f in os.listdir(WEATHER_DIR)
    if f.startswith('site_') and f.endswith('_weather_data.txt')
)

CROP_CONFIGS = [
    ('wheat',  CROPS['wheat'],  MAX_IRR_LEVELS),
    ('canola', CROPS['canola'], MAX_IRR_LEVELS),
    ('potato', CROPS['potato'], MAX_IRR_POTATO),
]

for crop_name, crop_params, irr_levels in CROP_CONFIGS:
    for year in YEARS:
        print(f"[{crop_name}] year={year} — {len(site_ids)} sites × {len(irr_levels)} levels")

        # Pre-load all wdfs for this year (avoid repeated disk reads)
        wdfs = {}
        for sid in site_ids:
            wf = os.path.join(WEATHER_DIR, f"site_{sid}_weather_data.txt")
            if os.path.exists(wf):
                wdfs[sid] = prepare_weather(wf)

        tasks = [
            delayed(process_site_irr)(sid, mx, year, wdfs[sid], crop_params)
            for mx in irr_levels
            for sid in site_ids
            if sid in wdfs
        ]

        raw = Parallel(n_jobs=-1, backend='loky', verbose=5)(tasks)
        results = [r for r in raw if r is not None]

        if results:
            df = pd.DataFrame(results)
            out_name = f"merged_simulation_results_{crop_name}_marginal_{year}_irrigation.csv"
            df.to_csv(os.path.join(OUTPUT_DIR, out_name), index=False)
            print(f"  → saved {len(df)} rows to {out_name}")

        gc.collect()

print("All marginal simulations complete.")
