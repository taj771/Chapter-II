# ERA5-Land download for Lake Diefenbaker study area (2018-2023)
# Uses ecmwfr v2.x (new CDS Beta API, Sept 2024+)
#
# Prerequisites:
#   1. Account at https://cds.climate.copernicus.eu
#   2. Personal Access Token from: CDS profile -> "Personal Access Token"
#   3. Run ONCE in R console (not in script):
#        library(ecmwfr)
#        wf_set_key(key = "paste-your-token-here")
#   4. Accept Terms of Use for reanalysis-era5-land at cds.climate.copernicus.eu
#   5. install.packages(c("ecmwfr", "terra", "tidyverse", "ncdf4"))
#
# Strategy: one request per year-month (CDS rejects full-year multi-var requests).
#   Downloads 72 monthly NC files -> Data/ERA5/monthly/era5_{year}_{month}.nc
#   era5_et0.R reads monthly files directly.
#
# Output: Data/ERA5/monthly/era5_{year}_{month}.nc  (72 files, ~3-5 MB each)
# Next step: run era5_et0.R

library(ecmwfr)
library(tidyverse)

BBOX    <- c(N = 52.2, W = -107.7, S = 50.4, E = -106.0)
YEARS   <- 2018:2023
MONTHS  <- 1:12
OUT_DIR <- "./Data/ERA5/monthly"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

ERA5_VARS <- c(
  "2m_temperature",
  "2m_dewpoint_temperature",
  "10m_u_component_of_wind",
  "10m_v_component_of_wind",
  "surface_net_solar_radiation",
  "surface_net_thermal_radiation",
  "total_precipitation"
)

for (yr in YEARS) {
  for (mo in MONTHS) {
    out_file <- file.path(OUT_DIR, sprintf("era5_%d_%02d.nc", yr, mo))

    if (file.exists(out_file)) {
      message(sprintf("Already exists: era5_%d_%02d.nc -- skipping", yr, mo))
      next
    }

    message(sprintf("Requesting ERA5-Land %d-%02d ...", yr, mo))
    Sys.sleep(5)

    request <- list(
      dataset_short_name = "reanalysis-era5-land",
      product_type       = "reanalysis",
      variable           = ERA5_VARS,
      year               = as.character(yr),
      month              = sprintf("%02d", mo),
      day                = sprintf("%02d", 1:31),
      time               = sprintf("%02d:00", 0:23),
      area               = c(BBOX["N"], BBOX["W"], BBOX["S"], BBOX["E"]),
      data_format        = "netcdf",
      download_format    = "unarchived",
      target             = basename(out_file)
    )

    ok <- tryCatch({
      wf_request(request = request, path = OUT_DIR, verbose = TRUE)
      TRUE
    }, error = function(e) {
      message("FAILED: ", yr, "-", sprintf("%02d", mo), ": ", e$message)
      FALSE
    })

    if (ok) Sys.sleep(10) else Sys.sleep(30)
  }
}

message("\nAll downloads complete.")
message("Files in: ", OUT_DIR)
message("Next: run R/era5_et0.R")
