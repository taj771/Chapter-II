"""
Databricks Asset Bundle override for AquaCrop simulation config.
Paths point to Workspace bundle location synced by VS Code extension.

Bundle root: /Workspace/Users/tajayalath@ua.edu/.bundle/WEP_Submission/dev/files

Usage in simulate_marginal.py / simulate_average.py on Databricks:
    import config_databricks as config   # swap this line only

Install on cluster (run once in notebook cell):
    %pip install aquacrop joblib scipy pandas numpy
"""
import os

# ── Workspace bundle paths (synced by VS Code Databricks extension) ───────────
BUNDLE_ROOT  = "/Workspace/Users/tajayalath@ua.edu/.bundle/WEP_Submission/dev/files"

CLIMATE_CSV  = f"{BUNDLE_ROOT}/Data/ERA5/era5_daily_et0.csv"
SOIL_CSV     = f"{BUNDLE_ROOT}/Data/ERA5/era5_soil_types.csv"
PLANT_CSV    = f"{BUNDLE_ROOT}/Data/ERA5/era5_planting_dates.csv"
WEATHER_DIR  = "/dbfs/FileStore/WEP/python/ClimateData"   # large files → DBFS
OUTPUT_DIR   = "/dbfs/FileStore/WEP/Data Main Analysis"    # simulation outputs → DBFS

os.makedirs(WEATHER_DIR, exist_ok=True)
os.makedirs(OUTPUT_DIR,  exist_ok=True)

# ── simulation years ──────────────────────────────────────────────────────────
YEARS = list(range(2018, 2024))

# ── soil ──────────────────────────────────────────────────────────────────────
SOIL_TYPE = 'LoamySand'

# ── growing season precipitation window ──────────────────────────────────────
PRECIP_START_MONTH = 5
PRECIP_END_MONTH   = 10

# ── crop parameters (identical to config.py) ─────────────────────────────────
WHEAT = dict(
    crop_type='Wheat',
    planting_date='05/01',
    harvest_date='10/30',
    CropType=3,
    Tbase=5,
    Tupp=35,
    Zmax=0.7,
    WP=10,          # calibrated to ICDC 75 bu/ac benchmark (NRMSE 12.4%)
    Tmin_up=8,
    Tmax_lo=40,
    exc=50,
    CGC=0.16764,
    CCx=0.95,
    CDC=0.13653,
    Kcb=1.10,
    fshape_r=15,
    SxTopQ=0.020,
    SxBotQ=0.005,
    p_up4=0.8,
    p_up2=0.55,
    fshape_w1=4,
    PlantPop=2000000,
)

CANOLA = dict(
    crop_type='Sunflower',
    planting_date='05/01',
    harvest_date='10/30',
    CropType=3,
    Tbase=5,
    Tupp=35,
    Zmax=0.7,
    WP=14,
    Tmin_up=8,
    Tmax_lo=40,
    exc=50,
    CGC=0.16764,
    CCx=0.95,
    CDC=0.13653,
    SeedSize=5,
    Kcb=1.10,
    fshape_r=15,
    SxTopQ=0.020,
    SxBotQ=0.005,
    p_up4=0.8,
    p_up2=0.55,
    fshape_w1=4,
)

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
    HIstart=75,
    dHI_pre=0.10,
    fshape_w2=8,
    dHI0=0.10,
)

CROPS = {'wheat': WHEAT, 'canola': CANOLA, 'potato': POTATO}

MAX_IRR_LEVELS = list(range(10, 210, 20))   # 20mm steps — wheat/canola
MAX_IRR_POTATO = list(range(10, 270, 10))   # 10mm steps — potato
