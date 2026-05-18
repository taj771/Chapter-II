# ERA5-Land -> daily ET0 (FAO-56 Penman-Monteith) at ERA5 native grid cells
# Reads:  Data/ERA5/monthly/era5_{year}_{month}.nc
# Sites:  ERA5 native 0.1-degree grid centroids (extracted from NC file)
# Writes: Data/ERA5/era5_grid_sites.csv   (grid_id, lon, lat for ~306 sites)
#         Data/ERA5/era5_daily_et0.csv    (Python pipeline input)
#
# Run after: era5_download.R

rm(list = ls())
library(tidyverse)
library(terra)
library(ncdf4)

ERA5_DIR <- "./Data/ERA5/monthly"
OUT_CSV  <- "./Data/ERA5/era5_daily_et0.csv"
SITES_CSV <- "./Data/ERA5/era5_grid_sites.csv"
YEARS    <- 2018:2023
MONTHS   <- 1:12

ELEV_M <- 550

# ── helpers ────────────────────────────────────────────────────────────────────

gamma_kpa <- function(elev) {
  P <- 101.3 * ((293 - 0.0065 * elev) / 293)^5.26
  0.000665 * P
}
GAMMA <- gamma_kpa(ELEV_M)

esat <- function(T) 0.6108 * exp(17.27 * T / (T + 237.3))

wind_10_to_2 <- function(u10) u10 * (4.87 / log(67.8 * 10 - 5.42))

pm_et0 <- function(Tmax, Tmin, ea, Rn, u2) {
  Tmean <- (Tmax + Tmin) / 2
  es    <- (esat(Tmax) + esat(Tmin)) / 2
  delta <- 4098 * es / (Tmean + 237.3)^2
  num   <- 0.408 * delta * Rn + GAMMA * (900 / (Tmean + 273)) * u2 * (es - ea)
  den   <- delta + GAMMA * (1 + 0.34 * u2)
  pmax(num / den, 0)
}

# ── build site list from first available NC file ───────────────────────────────

build_sites <- function() {
  nc_files <- list.files(ERA5_DIR, pattern = "era5_[0-9]{4}_[0-9]{2}\\.nc",
                         full.names = TRUE)
  if (length(nc_files) == 0) stop("No ERA5 monthly files found in ", ERA5_DIR)
  r <- rast(nc_files[1], subds = "t2m")[[1]]   # single layer for coords
  coords <- as.data.frame(r, xy = TRUE, na.rm = TRUE)[, c("x", "y")]
  sites <- tibble(
    grid_id = seq_len(nrow(coords)),
    lon     = round(coords$x, 2),
    lat     = round(coords$y, 2)
  )
  write_csv(sites, SITES_CSV)
  message(sprintf("ERA5 native grid: %d sites saved -> %s", nrow(sites), SITES_CSV))
  sites
}

sites <- build_sites()
n_sites <- nrow(sites)

# ── process one month using native raster values ──────────────────────────────

process_month <- function(yr, mo) {
  nc_file <- file.path(ERA5_DIR, sprintf("era5_%d_%02d.nc", yr, mo))
  if (!file.exists(nc_file)) {
    warning("Missing: ", nc_file); return(NULL)
  }
  message(sprintf("Processing %d-%02d ...", yr, mo))

  r_t2m  <- rast(nc_file, subds = "t2m")
  r_d2m  <- rast(nc_file, subds = "d2m")
  r_u10  <- rast(nc_file, subds = "u10")
  r_v10  <- rast(nc_file, subds = "v10")
  r_snsr <- rast(nc_file, subds = "ssr")
  r_sntr <- rast(nc_file, subds = "str")
  r_tp   <- rast(nc_file, subds = "tp")

  r_t2m <- r_t2m - 273.15
  r_d2m <- r_d2m - 273.15

  # terra::time() returns NA for valid_time dim — read directly with ncdf4
  nc_tmp <- nc_open(nc_file)
  tv_raw <- ncvar_get(nc_tmp, "valid_time")
  nc_close(nc_tmp)
  times   <- as.POSIXct(as.numeric(tv_raw), origin = "1970-01-01", tz = "UTC")
  dates   <- as.Date(times)
  u_dates <- sort(unique(dates))

  # Extract at native ERA5 grid cells (no interpolation — exact cell values)
  # values() returns matrix: rows = cells, cols = layers (hours)
  v_t  <- values(r_t2m)   # [n_cells x n_hours]
  v_d  <- values(r_d2m)
  v_u  <- values(r_u10)
  v_v  <- values(r_v10)
  v_sr <- values(r_snsr)
  v_tr <- values(r_sntr)
  v_tp <- values(r_tp)

  map_dfr(seq_len(n_sites), function(i) {
    t_h  <- as.numeric(v_t[i,  ])
    d_h  <- as.numeric(v_d[i,  ])
    u_h  <- as.numeric(v_u[i,  ])
    vv_h <- as.numeric(v_v[i,  ])
    sr_h <- as.numeric(v_sr[i, ])
    tr_h <- as.numeric(v_tr[i, ])
    tp_h <- as.numeric(v_tp[i, ])

    map_dfr(u_dates, function(dt) {
      idx     <- which(dates == dt)
      # ssr/str/tp are cumulative from 01:00 UTC; 00:00 value = previous day's carryover
      # Daily total = max (ssr) or min (str) among hours 01-23 only
      hrs     <- as.integer(format(times[idx], "%H"))
      idx_acc <- idx[hrs != 0]
      Tmax   <- max(t_h[idx],  na.rm = TRUE)
      Tmin   <- min(t_h[idx],  na.rm = TRUE)
      Td     <- mean(d_h[idx], na.rm = TRUE)
      u10    <- mean(sqrt(u_h[idx]^2 + vv_h[idx]^2), na.rm = TRUE)
      u2     <- wind_10_to_2(u10)
      Rn     <- (max(sr_h[idx_acc], na.rm = TRUE) + min(tr_h[idx_acc], na.rm = TRUE)) / 1e6
      precip <- max(tp_h[idx_acc], na.rm = TRUE) * 1000
      ea     <- esat(Td)
      ET0    <- pm_et0(Tmax, Tmin, ea, Rn, u2)

      tibble(
        site          = sites$grid_id[i],
        Date          = dt,
        Day           = as.integer(format(dt, "%d")),
        Month         = as.integer(format(dt, "%m")),
        Year          = yr,
        MinTemp       = Tmin,
        MaxTemp       = Tmax,
        Precipitation = precip,
        R_n           = Rn,
        e_a           = ea,
        ReferenceET   = ET0,
        u2_ms         = u2
      )
    })
  })
}

# ── run all year-months and save ──────────────────────────────────────────────

df_all <- map_dfr(YEARS, function(yr) map_dfr(MONTHS, function(mo) process_month(yr, mo)))

write_csv(df_all, OUT_CSV)
message(sprintf("\nSaved %d rows -> %s", nrow(df_all), OUT_CSV))
message(sprintf("Sites: %d ERA5 native grid cells", n_sites))
message("Columns: ", paste(names(df_all), collapse = ", "))
message("\nNext: update python/config.py CLIMATE_CSV and run simulate_average.py")
