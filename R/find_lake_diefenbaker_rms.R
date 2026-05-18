# Find Saskatchewan RM numbers for Lake Diefenbaker study area
# Method: Nominatim reverse geocoding of sampled fishnet points
# Output: prints unique RM numbers → paste into code_validation.R

rm(list = ls())
library(tidyverse)
library(httr)
library(jsonlite)

FISHNET_CSV <- "./Data Main Analysis copy/Shapefiles/MergeLakeDiefenbakerfishnet5Kmpoints.csv"
RM_YIELD_URL <- "https://dashboard.saskatchewan.ca/export/rm-yields-data/4950.csv"
YEARS <- 2018:2023

# ── 1. Sample fishnet points (every 15th = ~26 pts covering full area) ─────────

sites <- read_csv(FISHNET_CSV, show_col_types = FALSE)
sample_pts <- sites %>% slice(seq(1, nrow(.), by = 15))
cat(sprintf("Reverse-geocoding %d sample points via Nominatim...\n", nrow(sample_pts)))

# ── 2. Nominatim reverse geocode ───────────────────────────────────────────────

get_rm_name <- function(lat, lon) {
  Sys.sleep(1.1)  # Nominatim rate limit: 1 req/sec
  r <- tryCatch(
    GET(
      "https://nominatim.openstreetmap.org/reverse",
      query = list(lat = lat, lon = lon, format = "json", zoom = 8,
                   addressdetails = 1),
      add_headers(`User-Agent` = "academic-research/1.0 taj.aravinda@gmail.com",
                  `Accept-Language` = "en")
    ),
    error = function(e) NULL
  )
  if (is.null(r) || status_code(r) != 200) {
    warning(sprintf("Nominatim failed for (%.4f, %.4f)", lat, lon))
    return(NA_character_)
  }
  d <- tryCatch(fromJSON(content(r, "text", encoding = "UTF-8")), error = function(e) NULL)
  if (is.null(d)) return(NA_character_)
  # County field holds the RM name in SK ("Rural Municipality of X No. NNN")
  addr <- d$address
  county <- addr[["county"]] %||% addr[["state_district"]] %||% NA_character_
  county
}

geocode_results <- sample_pts %>%
  mutate(county = map2_chr(lat, lon, get_rm_name))

cat("\n── Raw county names returned ───────────────────────────────────────────────\n")
print(geocode_results %>% select(grid_id, lon, lat, county) %>% distinct(county, .keep_all = TRUE))

# ── 3. Extract RM numbers from county names ─────────────────────────────────────
# SK RM names: "Rural Municipality of Arm River No. 252" → 252

rm_numbers <- geocode_results %>%
  filter(!is.na(county)) %>%
  mutate(rm_num = as.integer(str_extract(county, "(?<=No\\.\\s)\\d+"))) %>%
  filter(!is.na(rm_num)) %>%
  distinct(county, rm_num) %>%
  arrange(rm_num)

cat("\n── RM numbers identified ───────────────────────────────────────────────────\n")
print(rm_numbers)

if (nrow(rm_numbers) == 0) {
  stop("No RM numbers extracted. Check Nominatim county field names above.")
}

LAKE_DIEF_RMS <- rm_numbers$rm_num
cat(sprintf("\nRM numbers: %s\n", paste(LAKE_DIEF_RMS, collapse = ", ")))

# ── 4. Download SK Dashboard RM yield CSV and filter ──────────────────────────

cat("\nDownloading SK Dashboard RM yield data...\n")
rm_df <- tryCatch(
  read_csv(RM_YIELD_URL, show_col_types = FALSE),
  error = function(e) { stop("Failed to download RM yields: ", e$message) }
)

cat(sprintf("Downloaded %d rows, %d RMs total\n",
            nrow(rm_df), n_distinct(rm_df$RM)))

# Filter to Lake Diefenbaker RMs and study years
rm_local <- rm_df %>%
  filter(RM %in% LAKE_DIEF_RMS, Year %in% YEARS) %>%
  arrange(Year, RM)

cat(sprintf("Filtered: %d rows for %d RMs, years %d-%d\n",
            nrow(rm_local), n_distinct(rm_local$RM), min(YEARS), max(YEARS)))

# ── 5. Convert bushels/acre → t/ha ─────────────────────────────────────────────
# Spring wheat: 1 bu/ac = 0.06725 t/ha (60 lb/bu wheat; 1 lb = 0.000453592 t; 1 ac = 0.404686 ha)
# Canola:       1 bu/ac = 0.05610 t/ha (50 lb/bu canola)
BU_AC_TO_T_HA_WHEAT  <- 60 * 0.000453592 / 0.404686   # 0.06725
BU_AC_TO_T_HA_CANOLA <- 50 * 0.000453592 / 0.404686   # 0.05604

obs_local <- rm_local %>%
  group_by(year = Year) %>%
  summarise(
    wheat_bu_ac  = mean(`Spring Wheat`, na.rm = TRUE),
    canola_bu_ac = mean(Canola,         na.rm = TRUE),
    .groups = "drop"
  ) %>%
  transmute(
    year,
    Wheat  = wheat_bu_ac  * BU_AC_TO_T_HA_WHEAT,
    Canola = canola_bu_ac * BU_AC_TO_T_HA_CANOLA
  ) %>%
  pivot_longer(c(Wheat, Canola), names_to = "crop", values_to = "obs_yield_local")

cat("\n── Lake Diefenbaker RM-average yields (t/ha) ───────────────────────────────\n")
print(obs_local)

# ── 6. Save for use in code_validation.R ───────────────────────────────────────

write_csv(obs_local,
          "./Data Main Analysis copy/validation_obs_local.csv")
cat("\nSaved → Data Main Analysis copy/validation_obs_local.csv\n")
cat("Copy RM numbers above into code_validation.R LAKE_DIEF_RMS vector.\n")
