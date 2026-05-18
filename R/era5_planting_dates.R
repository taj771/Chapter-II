# Thermal-threshold planting dates for wheat, canola, potato
# Per-site, per-year from ERA5 daily temperatures
#
# Thresholds (literature-backed):
#   Wheat:  Tmean > 5°C, 3 consecutive days (Lafond et al. 2006; FAO crop calendar)
#   Canola: Tmean > 7°C, 3 consecutive days (Canola Council of Canada)
#   Potato: Tmean > 8°C + Tmin > 0°C, 3 consecutive days (frost risk)
#
# Search window: April 1 – May 31; fallback = May 15 if threshold never met
# Input:  Data/ERA5/era5_daily_et0.csv  (MinTemp, MaxTemp per site per day)
# Output: Data/ERA5/era5_planting_dates.csv  (site, year, wheat_plant, canola_plant, potato_plant)
#
# Run after: era5_et0.R

rm(list = ls())
library(tidyverse)

IN_CSV  <- "./Data/ERA5/era5_daily_et0.csv"
OUT_CSV <- "./Data/ERA5/era5_planting_dates.csv"
YEARS   <- 2018:2023

FALLBACK_DATE <- "05-15"   # MM-DD used if threshold never met within window

# ── threshold checker ─────────────────────────────────────────────────────────
# Returns first date where crop's consecutive-day rule is satisfied in Apr-May
# df_yr: data.frame with columns Date, Tmean, Tmin for one site-year
find_plant_date <- function(df_yr, tmean_thresh, tmin_thresh = -Inf, n_consec = 3) {
  df_yr <- df_yr %>%
    filter(format(Date, "%m") %in% c("04", "05")) %>%
    arrange(Date)

  if (nrow(df_yr) < n_consec) return(as.Date(NA))

  tmean <- df_yr$Tmean
  tmin  <- df_yr$Tmin
  dates <- df_yr$Date

  for (i in seq_len(nrow(df_yr) - n_consec + 1)) {
    idx <- i:(i + n_consec - 1)
    if (all(tmean[idx] > tmean_thresh) && all(tmin[idx] > tmin_thresh)) {
      return(dates[i])   # first day of the qualifying run
    }
  }
  return(as.Date(NA))
}

# ── load and prepare ──────────────────────────────────────────────────────────
message("Loading ERA5 daily data...")
df <- read_csv(IN_CSV, show_col_types = FALSE) %>%
  mutate(
    Date  = as.Date(Date),
    Tmean = (MinTemp + MaxTemp) / 2,
    Tmin  = MinTemp
  )

sites <- sort(unique(df$site))
message(sprintf("Sites: %d  Years: %d", length(sites), length(YEARS)))

# ── compute per-site, per-year planting dates ─────────────────────────────────
results <- map_dfr(YEARS, function(yr) {
  if (yr %% 1 == 0) message(sprintf("  Year %d ...", yr))
  df_yr_all <- df %>% filter(Year == yr)

  map_dfr(sites, function(s) {
    df_sy <- df_yr_all %>% filter(site == s)
    fallback <- as.Date(sprintf("%d-%s", yr, FALLBACK_DATE))

    wheat_dt  <- find_plant_date(df_sy, tmean_thresh = 5, tmin_thresh = -Inf)
    canola_dt <- find_plant_date(df_sy, tmean_thresh = 7, tmin_thresh = -Inf)
    potato_dt <- find_plant_date(df_sy, tmean_thresh = 8, tmin_thresh =  0)

    tibble(
      site         = s,
      year         = yr,
      wheat_plant  = if_else(is.na(wheat_dt),  fallback, wheat_dt),
      canola_plant = if_else(is.na(canola_dt), fallback, canola_dt),
      potato_plant = if_else(is.na(potato_dt), fallback, potato_dt)
    )
  })
})

write_csv(results, OUT_CSV)

# ── summary ───────────────────────────────────────────────────────────────────
message(sprintf("\nSaved %d site-year records -> %s", nrow(results), OUT_CSV))
message("\nPlanting date summary (all sites, all years):")
results %>%
  summarise(
    wheat_mean  = format(as.Date(mean(as.numeric(wheat_plant)),  origin = "1970-01-01"), "%b %d"),
    wheat_range = paste(format(min(wheat_plant),  "%b %d"), "–", format(max(wheat_plant),  "%b %d")),
    canola_mean = format(as.Date(mean(as.numeric(canola_plant)), origin = "1970-01-01"), "%b %d"),
    canola_range= paste(format(min(canola_plant), "%b %d"), "–", format(max(canola_plant), "%b %d")),
    potato_mean = format(as.Date(mean(as.numeric(potato_plant)), origin = "1970-01-01"), "%b %d"),
    potato_range= paste(format(min(potato_plant), "%b %d"), "–", format(max(potato_plant), "%b %d"))
  ) %>%
  pivot_longer(everything()) %>%
  print()

message("\nFallback (May 15) used:")
results %>%
  summarise(
    wheat  = sum(format(wheat_plant,  "%m-%d") == FALLBACK_DATE),
    canola = sum(format(canola_plant, "%m-%d") == FALLBACK_DATE),
    potato = sum(format(potato_plant, "%m-%d") == FALLBACK_DATE)
  ) %>% print()

message("\nNext: update python/simulate_average.py to read era5_planting_dates.csv")
