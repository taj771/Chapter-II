"""
Centralized AquaCrop simulation configuration.
Crop parameters, paths, and simulation settings for all pipeline scripts.
"""
import os

# ── paths ─────────────────────────────────────────────────────────────────────
BASE_DIR    = os.path.dirname(os.path.abspath(__file__))
CLIMATE_CSV  = os.path.join(BASE_DIR, "..", "Data", "ERA5", "era5_daily_et0.csv")
SOIL_CSV     = os.path.join(BASE_DIR, "..", "Data", "ERA5", "era5_soil_types.csv")
PLANT_CSV    = os.path.join(BASE_DIR, "..", "Data", "ERA5", "era5_planting_dates.csv")
WEATHER_DIR  = os.path.join(BASE_DIR, "ClimateData")
OUTPUT_DIR   = os.path.join(BASE_DIR, "..", "Data Main Analysis")

# ── simulation years ──────────────────────────────────────────────────────────
YEARS = list(range(2018, 2024))

# ── soil ──────────────────────────────────────────────────────────────────────
SOIL_TYPE = 'LoamySand'   # single averaged type across study area (SoilGrids)

# ── growing season precipitation window (used for wheat output) ───────────────
PRECIP_START_MONTH = 5    # May
PRECIP_END_MONTH   = 10   # October

# ── crop parameters ──────────────────────────────────────────────────────────
# Planting date harmonized to May 1 across all simulations.
# Canola modeled as Sunflower proxy (no canola template in AquaCrop-OS).

WHEAT = dict(
    crop_type='Wheat',
    planting_date='05/01',
    harvest_date='10/30',
    CropType=3,
    Tbase=5,
    Tupp=35,
    Zmax=0.7,
    WP=10,          # calibrated to ICDC district benchmark 75 bu/ac (5.04 t/ha); Saiyed et al. 2009 default=16
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

CANOLA = dict(      # Sunflower proxy; WP=14 per Zeleke et al. 2011
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

POTATO = dict(      # WP=15 AquaCrop default; all potato simulations
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

# Lookup by name for simulate scripts
CROPS = {'wheat': WHEAT, 'canola': CANOLA, 'potato': POTATO}

# Maximum irrigation levels tested in marginal simulations (mm)
MAX_IRR_LEVELS = list(range(10, 210, 20))   # 10, 30, ..., 190 mm (20mm steps — wheat/canola)
MAX_IRR_POTATO = list(range(10, 270, 10))   # 10, 20, ..., 260 mm (10mm steps — potato steep early curve)
