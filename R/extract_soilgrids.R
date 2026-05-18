# SoilGrids v2 soil texture extraction at ERA5 native grid centroids
# Queries ISRIC SoilGrids REST API for sand/silt/clay (0-30 cm)
# Classifies USDA texture class -> maps to AquaCrop-OS soil type
#
# Inputs:  ERA5 BBOX + 0.1-degree grid (generated internally)
# Outputs: Data/ERA5/era5_soil_types.csv  (grid_id, lon, lat, sand%, silt%, clay%, soil_type)
#
# Run before: era5_et0.R  (era5_et0 will read this to annotate sites)
# Run time:   ~6-10 min (306 API calls, 1/sec rate limit)

rm(list = ls())
library(tidyverse)
library(httr)
library(jsonlite)

OUT_CSV <- "./Data/ERA5/era5_soil_types.csv"

# ── ERA5 native grid centroids (matches era5_et0.R site list) ─────────────────
# 0.1-degree grid within study area BBOX
lats <- seq(50.4, 52.2, by = 0.1)
lons <- seq(-107.7, -106.0, by = 0.1)

sites <- expand.grid(lat = lats, lon = lons) %>%
  arrange(desc(lat), lon) %>%   # N->S, W->E (matches terra raster order)
  mutate(
    grid_id = row_number(),
    lat     = round(lat, 1),
    lon     = round(lon, 1)
  )

message(sprintf("ERA5 native grid: %d sites", nrow(sites)))

# ── USDA texture triangle classifier ─────────────────────────────────────────
# Returns USDA class name matching AquaCrop-OS built-in soil types

classify_texture <- function(sand, silt, clay) {
  # sand/silt/clay in percent (sum ~100)
  if (is.na(sand) | is.na(silt) | is.na(clay)) return("LoamySand")  # fallback

  if (clay >= 40) {
    if (silt >= 40)                         return("SiltyClay")
    if (sand >= 45)                         return("SandyClay")
    return("Clay")
  }
  if (clay >= 35 & sand >= 45)              return("SandyClay")
  if (clay >= 27) {
    if (silt >= 40)                         return("SiltyClayLoam")
    if (sand <= 45)                         return("ClayLoam")
    return("SandyClayLoam")
  }
  if (silt >= 80 & clay < 12)              return("Silt")
  if (silt >= 50)                           return("SiltLoam")
  if (sand >= 70 & clay < 15) {
    if (sand >= 85)                         return("Sandy")
    return("LoamySand")
  }
  if (sand >= 52 & clay < 20)              return("SandyLoam")
  if (clay >= 7 & silt >= 28)              return("Loam")
  return("SandyLoam")  # residual
}

# ── SoilGrids v2 API query ────────────────────────────────────────────────────
# Returns weighted-mean sand/silt/clay for 0-30cm (g/kg -> percent)

query_soilgrids <- function(lat, lon) {
  Sys.sleep(1.1)  # ISRIC rate limit: 1 req/sec
  url <- "https://rest.isric.org/soilgrids/v2.0/properties/query"
  resp <- tryCatch(
    GET(url,
        query = list(
          lon      = lon,
          lat      = lat,
          property = "sand",
          property = "silt",
          property = "clay",
          depth    = "0-5cm",
          depth    = "5-15cm",
          depth    = "15-30cm",
          value    = "mean"
        ),
        timeout(30),
        add_headers(`Accept` = "application/json")
    ),
    error = function(e) NULL
  )

  if (is.null(resp) || status_code(resp) != 200) {
    message(sprintf("  API fail (%.1f, %.1f): status %s",
                    lat, lon, if (is.null(resp)) "NULL" else status_code(resp)))
    return(c(sand = NA, silt = NA, clay = NA))
  }

  d <- tryCatch(fromJSON(content(resp, "text", encoding = "UTF-8")),
                error = function(e) NULL)
  if (is.null(d)) return(c(sand = NA, silt = NA, clay = NA))

  # Extract layered values and compute depth-weighted mean (0-5=5cm, 5-15=10cm, 15-30=15cm)
  extract_prop <- function(prop_name) {
    prop <- d$properties$layers[d$properties$layers$name == prop_name, ]
    if (nrow(prop) == 0) return(NA)
    depths <- prop$depths[[1]]
    # depth weights
    wts <- c(`0-5cm` = 5, `5-15cm` = 10, `15-30cm` = 15)
    vals <- sapply(depths$label, function(lbl) {
      v <- depths$values$mean[depths$label == lbl]
      if (length(v) == 0 || is.null(v)) NA else v
    })
    wt_vec <- wts[names(vals)]
    wt_vec[is.na(wt_vec)] <- 0
    if (all(is.na(vals))) return(NA)
    sum(vals * wt_vec, na.rm = TRUE) / sum(wt_vec[!is.na(vals)])
  }

  sand_gkg <- extract_prop("sand")
  silt_gkg <- extract_prop("silt")
  clay_gkg <- extract_prop("clay")

  # g/kg -> percent
  c(sand = sand_gkg / 10, silt = silt_gkg / 10, clay = clay_gkg / 10)
}

# ── run queries ───────────────────────────────────────────────────────────────

message("Querying SoilGrids API (", nrow(sites), " sites, ~1 req/sec)...")
message("Estimated time: ", round(nrow(sites) * 1.1 / 60, 0), " minutes")

results <- map_dfr(seq_len(nrow(sites)), function(i) {
  if (i %% 20 == 0) message(sprintf("  %d / %d", i, nrow(sites)))
  vals <- query_soilgrids(sites$lat[i], sites$lon[i])
  tibble(
    grid_id   = sites$grid_id[i],
    lon       = sites$lon[i],
    lat       = sites$lat[i],
    sand_pct  = round(vals["sand"], 1),
    silt_pct  = round(vals["silt"], 1),
    clay_pct  = round(vals["clay"], 1)
  )
})

# Classify texture and map to AquaCrop soil type
results <- results %>%
  mutate(
    soil_type = pmap_chr(list(sand_pct, silt_pct, clay_pct), classify_texture)
  )

# Fill any NA sites with most common soil type
dominant_soil <- results %>% count(soil_type, sort = TRUE) %>% slice(1) %>% pull(soil_type)
results <- results %>%
  mutate(soil_type = if_else(is.na(soil_type) | soil_type == "LoamySand" & is.na(sand_pct),
                             dominant_soil, soil_type))

# Save
dir.create(dirname(OUT_CSV), recursive = TRUE, showWarnings = FALSE)
write_csv(results, OUT_CSV)

message(sprintf("\nSaved %d sites -> %s", nrow(results), OUT_CSV))
message("\nSoil type distribution:")
print(results %>% count(soil_type, sort = TRUE))
message("\nNext: run era5_et0.R (reads this file for per-site soil assignment)")
