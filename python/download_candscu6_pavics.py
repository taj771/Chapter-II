"""
Download CanDCS-U6 (BCCAQv2+ANUSPLIN300) climate data from PAVICS THREDDS.
Subsets to Saskatchewan study area and 2030-2050, SSP2-4.5.

CanDCS-U6 provides: pr, tasmax, tasmin at 0.0833° (~9 km) — no wind/radiation.
ET₀ uses ERA5 monthly wind climatology (consistent with ERA5 historical runs).
R_n from FAO-56 solar geometry; e_a from Tmin-as-dewpoint proxy.

Outputs (written to ClimateProjforAquaCrop/):
  CMIP245_ET_candscu6_CanESM5.csv            — original precipitation
  CMIP245_ET_candscu6_CanESM5_20redPrcp.csv  — 20% reduced precipitation

Install first:
    pip install xarray pydap scipy

Usage:
    python download_candscu6_pavics.py
    python download_candscu6_pavics.py --model MPI-ESM1-2-HR  # use different model
"""

import os, sys, gc, warnings, argparse
from pathlib import Path
import numpy as np
import pandas as pd

warnings.filterwarnings("ignore")

# ── install dependencies if needed ────────────────────────────────────────────

def _install(pkg):
    import subprocess
    subprocess.check_call([sys.executable, "-m", "pip", "install", "-q", pkg])

for pkg in ["xarray", "pydap", "scipy"]:
    try:
        __import__(pkg.replace("-", "_"))
    except ImportError:
        print(f"Installing {pkg}...")
        _install(pkg)

import xarray as xr
from scipy.spatial import cKDTree

# ── paths ─────────────────────────────────────────────────────────────────────

ONEDRIVE = (
    "/Users/tharakajayalath/Library/CloudStorage"
    "/OneDrive-UniversityofSaskatchewan"
    "/Chapter II-IrrigationValue/Chapter-II/AquaCropOPSyData"
)
SITES_CSV = (
    "/Users/tharakajayalath/Desktop/Claude_codex"
    "/WEP_Submission/Data/ERA5/era5_grid_sites.csv"
)
CACHE_DIR = os.path.join(ONEDRIVE, "ClimateData", "CMIP6", "CanDCSU6_cache")
OUT_DIR   = os.path.join(ONEDRIVE, "ClimateData", "CMIP6", "ClimateProjforAquaCrop")

for d in [CACHE_DIR, OUT_DIR]:
    os.makedirs(d, exist_ok=True)

# ── argument parsing ──────────────────────────────────────────────────────────

parser = argparse.ArgumentParser()
parser.add_argument("--model",   default="CanESM5",
                    help="CMIP6 model name (default: CanESM5)")
parser.add_argument("--ssp",     default="ssp245",
                    choices=["ssp126", "ssp245", "ssp585"])
parser.add_argument("--variant", default=None,
                    help="Ensemble variant (auto-detected if omitted)")
args = parser.parse_args()

MODEL   = args.model
SSP     = args.ssp
VARIANT = args.variant  # resolved below if None

# ── PAVICS THREDDS OPeNDAP base ───────────────────────────────────────────────
# Twitcher proxy provides authenticated OPeNDAP access to PCIC archive.

PAVICS_OPENDAP = (
    "https://pavics.ouranos.ca/twitcher/ows/proxy/thredds/dodsC"
    "/birdhouse/pcic/CanDCS-U6/CMIP6_BCCAQv2"
)

# Known ensemble variants per model (r1i1p1f1 unless model uses different forcing)
KNOWN_VARIANTS = {
    "CanESM5":       "r1i1p2f1",   # CanESM5 uses p2 (different forcing)
    "ACCESS-CM2":    "r1i1p1f1",
    "ACCESS-ESM1-5": "r1i1p1f1",
    "BCC-CSM2-MR":   "r1i1p1f1",
    "EC-Earth3":     "r1i1p1f1",
    "GFDL-ESM4":     "r1i1p1f2",
    "MPI-ESM1-2-HR": "r1i1p1f1",
    "MRI-ESM2-0":    "r1i1p1f1",
    "NorESM2-MM":    "r1i1p1f1",
    "UKESM1-0-LL":   "r1i1p1f2",
}

if VARIANT is None:
    VARIANT = KNOWN_VARIANTS.get(MODEL, "r1i1p1f1")

print(f"\nModel: {MODEL}  SSP: {SSP}  Variant: {VARIANT}")

# ── study area ────────────────────────────────────────────────────────────────

LAT_MIN, LAT_MAX = 50.3, 52.3    # Saskatchewan study area
LON_MIN, LON_MAX = -107.8, -105.9
YEAR_MIN, YEAR_MAX = 2030, 2050

# ── ERA5 monthly wind climatology (m/s) ───────────────────────────────────────
# Computed from ERA5 2018-2023 across 342 sites. Spatial std < 0.05 m/s.
# Used in place of CanDCS-U6 wind (not available) to ensure ET₀ consistency
# with historical ERA5 simulations.

ERA5_U2_MONTHLY = {
    1: 2.920, 2: 2.868, 3: 3.006, 4: 3.038, 5: 2.801,
    6: 2.794, 7: 2.708, 8: 2.663, 9: 2.842, 10: 2.913,
    11: 2.863, 12: 2.747
}

# ── load ERA5 site grid ───────────────────────────────────────────────────────

sites     = pd.read_csv(SITES_CSV)
site_ids  = sites["grid_id"].values
site_lats = sites["lat"].values
site_lons = sites["lon"].values    # already -180 to 180
site_coords = np.column_stack([site_lons, site_lats])

print(f"ERA5 sites: {len(site_ids)}")
print(f"  Lat: {site_lats.min():.2f} – {site_lats.max():.2f}")
print(f"  Lon: {site_lons.min():.2f} – {site_lons.max():.2f}")

# ── OPeNDAP URL builder ───────────────────────────────────────────────────────

def opendap_url(var):
    fname = (f"{var}_day_BCCAQv2+ANUSPLIN300_{MODEL}"
             f"_historical+{SSP}_{VARIANT}_gn_19500101-21001231.nc")
    return f"{PAVICS_OPENDAP}/{MODEL}/{fname}"

VARIABLES = ["tasmin", "tasmax", "pr"]

# ── spatial extraction ────────────────────────────────────────────────────────

def extract_variable(var):
    """Open via OPeNDAP, spatially subset, temporally subset, nearest-neighbour match."""
    cache = os.path.join(CACHE_DIR, f"{MODEL}_{SSP}_{var}_sites.parquet")
    if os.path.exists(cache):
        print(f"  [{var}] Loading from cache")
        return pd.read_parquet(cache)

    url = opendap_url(var)
    print(f"  [{var}] Opening OPeNDAP: {url}")

    # decode_times=False avoids cftime dependency entirely.
    # CanDCS-U6 uses noleap calendar; time is raw int "days since 1950-01-01".
    try:
        ds = xr.open_dataset(url, engine="pydap", decode_times=False)
    except Exception as e:
        raise RuntimeError(
            f"Cannot open {url}\n"
            f"  Error: {e}\n\n"
            "Ensure pydap is installed: pip install pydap\n"
            "If the server returns 401/403, PAVICS may require a token;\n"
            "set env var PAVICS_TOKEN and re-run."
        )

    da = ds[var]

    # Detect lat/lon dim names
    lat_name = next(d for d in da.dims if "lat" in d.lower())
    lon_name = next(d for d in da.dims if "lon" in d.lower())
    lats = da[lat_name].values    # coordinate arrays are small — fetch OK
    lons = da[lon_name].values    # CanDCS-U6 is -180 to 180

    # Decode time axis: raw ints "days since 1950-01-01" (noleap) → DatetimeIndex
    # time coordinate is 55115 ints — tiny, safe to fetch
    origin    = pd.Timestamp("1950-01-01")
    time_days = ds["time"].values.astype(int)
    all_dates = pd.to_datetime([origin + pd.Timedelta(days=int(d)) for d in time_days])

    # Find contiguous integer slice indices for 2030-2050
    year_mask = (all_dates.year >= YEAR_MIN) & (all_dates.year <= YEAR_MAX)
    t_idx     = np.where(year_mask)[0]
    t_start, t_end = int(t_idx[0]), int(t_idx[-1])
    dates_sub = all_dates[year_mask]

    # Find contiguous integer slice indices for spatial subset
    lat_mask = (lats >= LAT_MIN) & (lats <= LAT_MAX)
    lon_mask = (lons >= LON_MIN) & (lons <= LON_MAX)
    la_idx   = np.where(lat_mask)[0]; la0, la1 = int(la_idx[0]), int(la_idx[-1])
    lo_idx   = np.where(lon_mask)[0]; lo0, lo1 = int(lo_idx[0]), int(lo_idx[-1])

    n_t  = t_end  - t_start  + 1
    n_la = la1    - la0      + 1
    n_lo = lo1    - lo0      + 1
    print(f"  [{var}] OPeNDAP range request: {n_t} days × {n_la} lat × {n_lo} lon "
          f"({n_t * n_la * n_lo / 1e6:.1f}M values)...")

    # isel sends a single OPeNDAP range request — no full-file download
    da_sub = da.isel(
        {lat_name: slice(la0, la1 + 1),
         lon_name: slice(lo0, lo1 + 1),
         "time":   slice(t_start, t_end + 1)}
    )
    # decode_times=False leaves only time undecoded; scale_factor/add_offset still applied
    arr_f = da_sub.values.astype(np.float64)   # triggers OPeNDAP fetch of subset only
    ds.close()

    # KDTree nearest-neighbour: CanDCS-U6 subset grid → ERA5 site coordinates
    lats_s = lats[lat_mask]
    lons_s = lons[lon_mask]
    grid_lon, grid_lat = np.meshgrid(lons_s, lats_s)
    grid_pts = np.column_stack([grid_lon.ravel(), grid_lat.ravel()])
    tree = cKDTree(grid_pts)
    _, idx = tree.query(site_coords)
    lat_idx, lon_idx = np.unravel_index(idx, (len(lats_s), len(lons_s)))

    arr   = arr_f      # (n_t, n_la, n_lo) — decoded values
    times = dates_sub
    data  = {int(sid): arr[:, lat_idx[i], lon_idx[i]]
             for i, sid in enumerate(site_ids)}
    df = pd.DataFrame(data, index=times)
    df.to_parquet(cache)
    print(f"  [{var}] Cached: {df.shape} → {os.path.basename(cache)}")
    return df

# ── net radiation (FAO-56 temperature method) ─────────────────────────────────

def compute_rn(Tmin_df, Tmax_df):
    """
    FAO-56 net radiation from temperature only (no measured radiation).
    Assumes 60% of clear-sky radiation reaches surface (cloudy/arid correction).
    R_n returned as MJ/m²/day.
    """
    dates  = Tmin_df.index
    doy    = dates.day_of_year.values
    lat_r  = np.deg2rad(51.3)   # study area centroid latitude (radians)

    # Extraterrestrial radiation Ra (MJ/m²/day) — FAO-56 Eq. 21
    # Gsc = 0.0820 MJ/m²/min; factor 24*60/π converts to daily total
    dr    = 1 + 0.033 * np.cos(2 * np.pi / 365 * doy)
    delta = 0.409 * np.sin(2 * np.pi / 365 * doy - 1.39)
    ws    = np.arccos(-np.tan(lat_r) * np.tan(delta))
    Ra    = (24 * 60 / np.pi * 0.0820 * dr *
             (ws * np.sin(lat_r) * np.sin(delta) +
              np.cos(lat_r) * np.cos(delta) * np.sin(ws)))
    Ra    = np.maximum(Ra[:, None], 0.1)   # broadcast to (time, sites)

    Rs0   = 0.75 * Ra                      # clear-sky solar radiation
    # Hargreaves solar radiation estimate
    Rs    = 0.16 * Ra * np.sqrt(
                np.maximum(Tmax_df.values - Tmin_df.values, 0))
    Rs    = np.minimum(Rs, Rs0)            # clip to clear-sky ceiling

    Tmin  = Tmin_df.values
    Tmax  = Tmax_df.values
    T_mean = (Tmin + Tmax) / 2

    e_s   = 0.6108 * np.exp(17.27 * T_mean / (T_mean + 237.3))
    # e_a from Tmin as dewpoint proxy (standard when humidity unavailable)
    e_a   = 0.6108 * np.exp(17.27 * Tmin  / (Tmin  + 237.3))

    sigma = 4.903e-9   # MJ/K4/m2/day
    T4    = ((Tmax + 273.16)**4 + (Tmin + 273.16)**4) / 2
    Rnl   = (sigma * T4 *
             (0.34 - 0.14 * np.sqrt(np.maximum(e_a, 1e-4))) *
             (1.35 * np.minimum(Rs / Rs0, 1.0) - 0.35))
    Rn    = (1 - 0.23) * Rs - Rnl

    Rn_df  = pd.DataFrame(Rn,  index=Tmin_df.index, columns=Tmin_df.columns)
    ea_df  = pd.DataFrame(e_a, index=Tmin_df.index, columns=Tmin_df.columns)
    return Rn_df, ea_df

# ── ET₀ (FAO-56 Penman-Monteith with ERA5 monthly wind) ──────────────────────

def compute_et0(Tmin_df, Tmax_df, Rn_df, ea_df):
    months  = Tmin_df.index.month.values
    u2_arr  = np.array([ERA5_U2_MONTHLY[m] for m in months])[:, None]  # (time,1)
    T_mean  = (Tmax_df.values + Tmin_df.values) / 2
    e_s     = 0.6108 * np.exp(17.27 * T_mean / (T_mean + 237.3))
    delta   = 4098 * e_s / (T_mean + 237.3) ** 2
    gamma   = 0.066
    ET0     = ((0.408 * delta * Rn_df.values +
                gamma * 900 / (T_mean + 273) * u2_arr * (e_s - ea_df.values)) /
               (delta + gamma * (1 + 0.34 * u2_arr))).clip(0)
    return pd.DataFrame(ET0, index=Tmin_df.index, columns=Tmin_df.columns)

# ── step 1: extract variables ─────────────────────────────────────────────────

print("\n" + "=" * 60)
print(f"Extracting {MODEL} {SSP} from PAVICS THREDDS")
print("=" * 60)

raw = {}
for var in VARIABLES:
    raw[var] = extract_variable(var)
    gc.collect()

# ── step 2: unit conversions ──────────────────────────────────────────────────

print("\nChecking units (CanDCS-U6 BCCAQv2 already uses degC and mm/day)...")
# tasmin/tasmax: units = degC  (bias-corrected, already Celsius — no K conversion)
# pr:           units = kg m-2 d-1 = mm/day  (no 86400 factor needed)
Tmin = raw["tasmin"].copy()
Tmax = raw["tasmax"].copy()
Prcp = raw["pr"].copy().clip(lower=0)   # remove floating-point negatives

print(f"  Tmin mean (growing season): "
      f"{Tmin[Tmin.index.month.isin([5,6,7,8])].mean().mean():.1f} °C")
print(f"  Tmax mean (growing season): "
      f"{Tmax[Tmax.index.month.isin([5,6,7,8])].mean().mean():.1f} °C")
print(f"  Prcp mean (growing season): "
      f"{Prcp[Prcp.index.month.isin([5,6,7,8])].mean().mean():.2f} mm/day")

# ── step 3: compute R_n and e_a ───────────────────────────────────────────────

print("Computing net radiation (FAO-56 temperature method)...")
Rn, ea = compute_rn(Tmin, Tmax)

# ── step 4: compute ET₀ ───────────────────────────────────────────────────────

print("Computing ET₀ (ERA5 monthly wind climatology)...")
ET0 = compute_et0(Tmin, Tmax, Rn, ea)

gs_et0  = ET0[ET0.index.month.isin([5, 6, 7, 8])].mean().mean()
gs_u2   = np.mean([ERA5_U2_MONTHLY[m] for m in [5, 6, 7, 8]])
print(f"  Growing season ET₀ mean: {gs_et0:.3f} mm/day")
print(f"  Growing season u2 used:  {gs_u2:.3f} m/s (ERA5 climatology)")

# ── step 5: assemble long-format CSV ──────────────────────────────────────────

print("\nAssembling output CSV...")
dates   = Tmin.index
records = []
for i, sid in enumerate(site_ids):
    sid_int = int(sid)
    records.append(pd.DataFrame({
        "site":          sid_int,
        "Date":          dates.strftime("%Y-%m-%d"),
        "Year":          dates.year,
        "Month":         dates.month,
        "Day":           dates.day,
        "MinTemp":       Tmin[sid_int].values,
        "MaxTemp":       Tmax[sid_int].values,
        "Precipitation": Prcp[sid_int].values,
        "R_n":           Rn[sid_int].values,
        "e_a":           ea[sid_int].values,
        "ReferenceET":   ET0[sid_int].values,
    }))
    if (i + 1) % 50 == 0:
        print(f"  {i+1}/{len(site_ids)} sites...", end="\r")

df_all = pd.concat(records, ignore_index=True)
print(f"\n  Total rows: {len(df_all):,}  |  Sites: {df_all['site'].nunique()}")

# ── step 6: save ──────────────────────────────────────────────────────────────

tag      = f"candscu6_{MODEL}"
out_csv  = os.path.join(OUT_DIR, f"CMIP245_ET_{tag}.csv")
out_red  = os.path.join(OUT_DIR, f"CMIP245_ET_{tag}_20redPrcp.csv")

df_all.to_csv(out_csv, index=False)
print(f"\nSaved: {out_csv}")

df_red = df_all.copy()
df_red["Precipitation"] *= 0.80
df_red.to_csv(out_red, index=False)
print(f"Saved (20% reduced): {out_red}")

# ── summary ───────────────────────────────────────────────────────────────────

gs = df_all[df_all["Month"].between(5, 8)]
print(f"\n=== Growing season (May-Aug) summary ===")
print(f"  ET₀ mean:   {gs['ReferenceET'].mean():.3f} mm/day")
print(f"  Tmax mean:  {gs['MaxTemp'].mean():.1f} °C")
print(f"  Prcp mean:  {gs['Precipitation'].mean():.2f} mm/day")
print(f"  R_n mean:   {gs['R_n'].mean():.3f} MJ/m²/day")
print(f"\nNext steps:")
print(f"  → Run simulate_cmip_candscu6_oriprcp.py  (original precip)")
print(f"  → Run simulate_cmip_candscu6_redprcp.py  (20% reduced precip)")
