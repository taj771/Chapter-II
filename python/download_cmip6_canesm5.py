"""
Download CanESM5 SSP2-4.5 daily data from Globus HTTPS (ESGF LLNL open access).
Subsets to Saskatchewan (lat 49-55N, lon 248-257E) and 2030-2050.
Computes ET0 via FAO-56 Penman-Monteith with actual CanESM5 sfcWind.

Downloads ~5.4 GB total (6 global NetCDF files, ~900 MB each).
Cached per-variable parquet files (~2 MB each) written after first extract.

Install first:
    pip install xarray netCDF4 scipy

Usage:
    python download_cmip6_canesm5.py
"""

import os, sys, subprocess, warnings, gc
from pathlib import Path
import pandas as pd
import numpy as np

# ── install if needed ──────────────────────────────────────────────────────────
def install(pkg):
    subprocess.check_call([sys.executable, "-m", "pip", "install", "-q", pkg])

for pkg in ["xarray", "netCDF4", "scipy"]:
    try:
        __import__(pkg.replace("-", "_"))
    except ImportError:
        print(f"Installing {pkg}...")
        install(pkg)

import xarray as xr
from scipy.spatial import cKDTree
import requests

warnings.filterwarnings("ignore")

# ── paths ──────────────────────────────────────────────────────────────────────

ONEDRIVE = (
    "/Users/tharakajayalath/Library/CloudStorage"
    "/OneDrive-UniversityofSaskatchewan"
    "/Chapter II-IrrigationValue/Chapter-II/AquaCropOPSyData"
)
SITES_CSV = (
    "/Users/tharakajayalath/Desktop/Claude_codex"
    "/WEP_Submission/Data/ERA5/era5_grid_sites.csv"
)
NC_DIR    = os.path.join(ONEDRIVE, "ClimateData", "CMIP6", "CanESM5_SSP245_nc")
CACHE_DIR = os.path.join(ONEDRIVE, "ClimateData", "CMIP6", "CanESM5_SSP245_cache")
OUT_CSV   = os.path.join(ONEDRIVE, "ClimateData", "CMIP6",
                         "ClimateProjforAquaCrop", "CMIP245_ET_canesm5.csv")
OUT_CSV_RED = OUT_CSV.replace("canesm5.csv", "canesm5_20redPrcp.csv")

for d in [NC_DIR, CACHE_DIR, os.path.dirname(OUT_CSV)]:
    os.makedirs(d, exist_ok=True)

# Saskatchewan bounding box
LAT_MIN, LAT_MAX = 49.0, 55.0
LON_MIN, LON_MAX = 248.0, 257.0   # CanESM5 uses 0-360; -112→248, -103→257
YEAR_MIN, YEAR_MAX = 2030, 2050
P_KPA = 91.0  # surface pressure at ~570m elevation (Lake Diefenbaker)

# ── CMIP6 variables — direct download from Globus HTTPS (open, no auth) ───────
# Source: CMIP6.ScenarioMIP.CCCma.CanESM5.ssp245.r1i1p1f1.day
# Mirror: Globus LLNL ESGF (g-52ba3.fd635.8443.data.globus.org)

GLOBUS = "https://g-52ba3.fd635.8443.data.globus.org/css03_data/CMIP6/ScenarioMIP/CCCma/CanESM5/ssp245/r1i1p1f1/day"
VARIABLES = {
    "sfcWind": f"{GLOBUS}/sfcWind/gn/v20190306/sfcWind_day_CanESM5_ssp245_r1i1p1f1_gn_20150101-21001231.nc",
    "tasmin":  f"{GLOBUS}/tasmin/gn/v20190306/tasmin_day_CanESM5_ssp245_r1i1p1f1_gn_20150101-21001231.nc",
    "tasmax":  f"{GLOBUS}/tasmax/gn/v20190429/tasmax_day_CanESM5_ssp245_r1i1p1f1_gn_20150101-21001231.nc",
    "pr":      f"{GLOBUS}/pr/gn/v20190306/pr_day_CanESM5_ssp245_r1i1p1f1_gn_20150101-21001231.nc",
    "rsds":    f"{GLOBUS}/rsds/gn/v20190429/rsds_day_CanESM5_ssp245_r1i1p1f1_gn_20150101-21001231.nc",
    "huss":    f"{GLOBUS}/huss/gn/v20190429/huss_day_CanESM5_ssp245_r1i1p1f1_gn_20150101-21001231.nc",
}

# ── load ERA5 sites ─────────────────────────────────────────────────────────────

sites     = pd.read_csv(SITES_CSV)
site_ids  = sites["grid_id"].values
site_lats = sites["lat"].values
site_lons_360 = sites["lon"].values % 360   # convert -107.7 → 252.3

site_coords = np.column_stack([site_lons_360, site_lats])
print(f"ERA5 sites: {len(site_ids)}  |  Lon 0-360: {site_lons_360.min():.1f}-{site_lons_360.max():.1f}  |  Lat: {site_lats.min():.1f}-{site_lats.max():.1f}")

# ── download helper ────────────────────────────────────────────────────────────

def download_nc(var, url):
    dest = os.path.join(NC_DIR, f"{var}_ssp245_CanESM5.nc")
    if os.path.exists(dest):
        size_mb = os.path.getsize(dest) / 1e6
        print(f"  [{var}] Already downloaded: {size_mb:.0f} MB")
        return dest

    print(f"  [{var}] Downloading from Globus HTTPS...")
    r_head = requests.head(url, timeout=20)
    total_mb = int(r_head.headers.get("content-length", 0)) / 1e6
    print(f"  [{var}] File size: {total_mb:.0f} MB — this may take several minutes")

    with requests.get(url, stream=True, timeout=600) as r:
        r.raise_for_status()
        downloaded = 0
        with open(dest, "wb") as f:
            for chunk in r.iter_content(chunk_size=8 * 1024 * 1024):  # 8 MB chunks
                f.write(chunk)
                downloaded += len(chunk)
                pct = downloaded / (total_mb * 1e6) * 100
                print(f"\r  [{var}] {pct:5.1f}% ({downloaded/1e6:.0f}/{total_mb:.0f} MB)", end="", flush=True)
    print(f"\n  [{var}] Done: {dest}")
    return dest

# ── spatial extraction ─────────────────────────────────────────────────────────

def extract_sites(nc_path, var):
    """Spatial+temporal subset and nearest-neighbour extraction to ERA5 sites."""
    cache = os.path.join(CACHE_DIR, f"{var}_sites.parquet")
    if os.path.exists(cache):
        print(f"  [{var}] Loading cache")
        return pd.read_parquet(cache)

    print(f"  [{var}] Opening NetCDF and subsetting...")
    ds = xr.open_dataset(nc_path, engine="netcdf4")
    da = ds[var]

    # Determine lat/lon dimension names
    lat_name = "lat" if "lat" in da.dims else "latitude"
    lon_name = "lon" if "lon" in da.dims else "longitude"
    lats = da[lat_name].values
    lons = da[lon_name].values  # 0-360

    # CanESM5 lat is sorted descending (-90 → 90 reversed), check
    if lats[0] > lats[-1]:
        lat_slice = slice(LAT_MAX, LAT_MIN)   # descending
    else:
        lat_slice = slice(LAT_MIN, LAT_MAX)   # ascending

    da_sub = da.sel(
        {lat_name: lat_slice,
         lon_name: slice(LON_MIN, LON_MAX),
         "time":   slice(f"{YEAR_MIN}-01-01", f"{YEAR_MAX}-12-31")}
    )
    print(f"  [{var}] Subset shape: {da_sub.shape}  (loading into memory...)")
    da_sub = da_sub.load()  # pull from disk into RAM
    ds.close()

    # Build KDTree over CMIP grid
    lats_s = da_sub[lat_name].values
    lons_s = da_sub[lon_name].values
    grid_lon, grid_lat = np.meshgrid(lons_s, lats_s)
    grid_pts = np.column_stack([grid_lon.ravel(), grid_lat.ravel()])
    tree = cKDTree(grid_pts)
    _, idx = tree.query(site_coords)
    lat_idx, lon_idx = np.unravel_index(idx, (len(lats_s), len(lons_s)))

    arr   = da_sub.values  # (time, lat, lon)
    times = pd.to_datetime(da_sub["time"].values)
    data  = {sid: arr[:, lat_idx[i], lon_idx[i]]
             for i, sid in enumerate(site_ids)}
    df = pd.DataFrame(data, index=times)
    df.to_parquet(cache)
    print(f"  [{var}] Cached {df.shape}")
    return df

# ── step 1: download all variables ────────────────────────────────────────────

print("\n" + "=" * 60)
print("STEP 1: Downloading CanESM5 SSP245 NetCDF files")
print("=" * 60)
nc_paths = {var: download_nc(var, url) for var, url in VARIABLES.items()}

# ── step 2: extract to ERA5 sites ─────────────────────────────────────────────

print("\n" + "=" * 60)
print("STEP 2: Extracting to ERA5 site locations")
print("=" * 60)
raw = {var: extract_sites(nc_paths[var], var) for var in VARIABLES}

# ── step 3: unit conversions ──────────────────────────────────────────────────

print("\nSTEP 3: Unit conversions...")
Tmin = raw["tasmin"] - 273.15                        # K → °C
Tmax = raw["tasmax"] - 273.15                        # K → °C
Prcp = raw["pr"]     * 86400.0                       # kg/m²/s → mm/day
Rs   = raw["rsds"]   * 0.0864                        # W/m² → MJ/m²/day
e_a  = raw["huss"]   * P_KPA / 0.622                # spec. humidity → e_a kPa
# 10m wind → 2m (FAO-56 Eq 47)
u2   = raw["sfcWind"] * (4.87 / np.log(67.8 * 10 - 5.42))

# ── step 4: net radiation (FAO-56) ────────────────────────────────────────────

print("STEP 4: Computing net radiation (FAO-56)...")

doy  = pd.Series(Rs.index.day_of_year.values, index=Rs.index)
lat_rad = np.deg2rad(51.3)  # centroid of study area
dr   = 1 + 0.033 * np.cos(2 * np.pi / 365 * doy.values[:, None])
delt = 0.409 * np.sin(2 * np.pi / 365 * doy.values[:, None] - 1.39)
ws   = np.arccos(-np.tan(lat_rad) * np.tan(delt))
Ra   = (24 / np.pi * 0.0820 * dr *
        (ws * np.sin(lat_rad) * np.sin(delt) +
         np.cos(lat_rad) * np.cos(delt) * np.sin(ws)))
Ra   = np.maximum(Ra, 0.1)
Rs0  = 0.75 * Ra

sigma = 4.903e-9
T4   = ((Tmax.values + 273.16)**4 + (Tmin.values + 273.16)**4) / 2
Rnl  = (sigma * T4 *
        (0.34 - 0.14 * np.sqrt(np.maximum(e_a.values, 1e-4))) *
        (1.35 * np.minimum(Rs.values / Rs0, 1.0) - 0.35))
Rn   = pd.DataFrame((1 - 0.23) * Rs.values - Rnl,
                    index=Rs.index, columns=Rs.columns)

# ── step 5: FAO-56 ET₀ with actual CanESM5 sfcWind ───────────────────────────

print("STEP 5: Computing ET₀ with actual CanESM5 wind (FAO-56 PM)...")
T_mean = (Tmax + Tmin) / 2
e_s    = 0.6108 * np.exp(17.27 * T_mean / (T_mean + 237.3))
delta  = 4098 * e_s / (T_mean + 237.3) ** 2
gamma  = 0.066
ET0    = ((0.408 * delta * Rn +
           gamma * 900 / (T_mean + 273) * u2 * (e_s - e_a)) /
          (delta + gamma * (1 + 0.34 * u2))).clip(lower=0)

# ── step 6: assemble long-format CSV ──────────────────────────────────────────

print("STEP 6: Assembling output CSV...")
dates = Tmin.index
records = []
for sid in site_ids:
    records.append(pd.DataFrame({
        "site":          int(sid),
        "Date":          dates.strftime("%Y-%m-%d"),
        "Year":          dates.year,
        "Month":         dates.month,
        "Day":           dates.day,
        "MinTemp":       Tmin[sid].values,
        "MaxTemp":       Tmax[sid].values,
        "Precipitation": Prcp[sid].values,
        "R_s":           Rs[sid].values,
        "R_n":           Rn[sid].values,
        "e_a":           e_a[sid].values,
        "u2_ms":         u2[sid].values,
        "ReferenceET":   ET0[sid].values,
    }))
    if len(records) % 50 == 0:
        print(f"  {len(records)}/{len(site_ids)} sites done...", end="\r")

df_all = pd.concat(records, ignore_index=True)
print()

# ── save ──────────────────────────────────────────────────────────────────────

df_all.to_csv(OUT_CSV, index=False)
print(f"\nSaved: {OUT_CSV}  ({len(df_all):,} rows, {df_all['site'].nunique()} sites)")

df_red = df_all.copy()
df_red["Precipitation"] *= 0.80
df_red.to_csv(OUT_CSV_RED, index=False)
print(f"Saved (20% reduced precip): {OUT_CSV_RED}")

# ── summary ───────────────────────────────────────────────────────────────────

gs = df_all[df_all["Month"].between(5, 8)]
print(f"\n=== Growing season (May-Aug) summary ===")
print(f"  ET0 mean:      {gs['ReferenceET'].mean():.3f} mm/day")
print(f"  Wind u2 mean:  {gs['u2_ms'].mean():.2f} m/s  (original CMIP used fixed 2.0)")
print(f"  Tmax mean:     {gs['MaxTemp'].mean():.1f} °C")
print(f"  Prcp mean:     {gs['Precipitation'].mean():.2f} mm/day")
print(f"\nNext: python simulate_cmip_canesm5_redprcp.py")
