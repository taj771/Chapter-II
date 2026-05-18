"""
Smoke test: 1 site, 1 year, 3 irrigation levels, all 3 crops.
Run this before launching full simulate_marginal.py.
Expected runtime: ~2-5 minutes on Databricks.
"""
import os, sys

# Use Databricks paths if running on cluster, local paths otherwise
if os.path.exists("/Workspace/Users/tajayalath@ua.edu"):
    sys.path.insert(0, "/Workspace/Users/tajayalath@ua.edu/.bundle/WEP_Submission/dev/files/python")
    import config_databricks as config
else:
    sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
    import config

import pandas as pd
from aquacrop import AquaCropModel, Soil, Crop, InitialWaterContent, IrrigationManagement
from aquacrop.utils import prepare_weather
from scipy.optimize import fmin
import numpy as np

_SOIL_MAP = {
    'SiltyClayLoam': 'SiltClayLoam',
    'SiltyClay':     'SiltClay',
    'Silt':          'SiltLoam',
    'Sandy':         'LoamySand',
}

# Load soil + planting date lookups
if os.path.exists(config.SOIL_CSV):
    _sdf = pd.read_csv(config.SOIL_CSV)[['grid_id', 'soil_type']]
    site_soil = dict(zip(_sdf['grid_id'], _sdf['soil_type']))
else:
    site_soil = {}

if os.path.exists(config.PLANT_CSV):
    _pdf = pd.read_csv(config.PLANT_CSV)
    _pdf['wheat_plant']  = pd.to_datetime(_pdf['wheat_plant']).dt.strftime('%m/%d')
    _pdf['canola_plant'] = pd.to_datetime(_pdf['canola_plant']).dt.strftime('%m/%d')
    _pdf['potato_plant'] = pd.to_datetime(_pdf['potato_plant']).dt.strftime('%m/%d')
    plant_dates = {(r.site, r.year): {'wheat': r.wheat_plant,
                                      'canola': r.canola_plant,
                                      'potato': r.potato_plant}
                   for _, r in _pdf.iterrows()}
else:
    plant_dates = {}

# Pick first available site
site_ids = sorted(
    int(f.split('_')[1])
    for f in os.listdir(config.WEATHER_DIR)
    if f.startswith('site_') and f.endswith('_weather_data.txt')
)
TEST_SITE    = site_ids[0]
TEST_YEAR    = 2020
TEST_IRR     = [50, 100, 150]   # 3 levels only

print(f"Test: site={TEST_SITE}, year={TEST_YEAR}, irr_levels={TEST_IRR}")
print(f"Soil type: {_SOIL_MAP.get(site_soil.get(TEST_SITE, config.SOIL_TYPE), site_soil.get(TEST_SITE, config.SOIL_TYPE))}")
print(f"Planting dates: {plant_dates.get((TEST_SITE, TEST_YEAR), 'fallback')}")

wf  = os.path.join(config.WEATHER_DIR, f"site_{TEST_SITE}_weather_data.txt")
wdf = __import__('aquacrop.utils', fromlist=['prepare_weather']).prepare_weather(wf)

def run_model(smts, max_irr, year, wdf, crop_params, site_id, crop_name):
    raw_soil  = site_soil.get(site_id, config.SOIL_TYPE)
    soil_name = _SOIL_MAP.get(raw_soil, raw_soil)
    p_date    = plant_dates.get((site_id, year), {}).get(crop_name, crop_params['planting_date'])
    params    = {**crop_params, 'planting_date': p_date}
    ctype     = params.pop('crop_type')
    crop      = Crop(ctype, **params)
    soil      = Soil(soil_name)
    initWC    = InitialWaterContent(wc_type='Pct', value=[70])
    irr       = IrrigationManagement(irrigation_method=1, SMT=smts, MaxIrrSeason=max_irr)
    model = AquaCropModel(
        f'{year}/{p_date}', f'{year}/{crop_params["harvest_date"]}',
        wdf, soil, crop, irrigation_management=irr, initial_water_content=initWC,
    )
    model.run_model(till_termination=True)
    return model.get_simulation_results()

CROP_CONFIGS = [
    ('wheat',  config.CROPS['wheat'],  TEST_IRR),
    ('canola', config.CROPS['canola'], TEST_IRR),
    ('potato', config.CROPS['potato'], TEST_IRR),
]

for crop_name, crop_params, irr_levels in CROP_CONFIGS:
    print(f"\n── {crop_name} ──")
    for max_irr in irr_levels:
        x0   = np.random.rand(4) * 100
        smts = fmin(
            lambda s: -(run_model(s, max_irr, TEST_YEAR, wdf, dict(crop_params), TEST_SITE, crop_name)
                        [('Fresh yield (tonne/ha)' if crop_name == 'potato' else 'Dry yield (tonne/ha)')].mean()),
            x0, disp=False
        )
        out  = run_model(smts, max_irr, TEST_YEAR, wdf, dict(crop_params), TEST_SITE, crop_name)
        yld  = out[('Fresh yield (tonne/ha)' if crop_name == 'potato' else 'Dry yield (tonne/ha)')].mean()
        tirr = out['Seasonal irrigation (mm)'].mean()
        print(f"  max_irr={max_irr:3d}mm → yield={yld:.2f} t/ha, irrigation={tirr:.1f}mm")

print("\nSmoke test PASSED — safe to launch full simulate_marginal.py")
