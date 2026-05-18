# Crop budget data for Lake Diefenbaker irrigated study area, SK
#
# ALL VALUES HARDCODED FROM PRIMARY SOURCES — no live API required.
#
# Prices and irrigated costs:
#   ICDC "Irrigation Economics and Agronomics" annual guides 2019-2023
#   Crop_Planning_Guide_irrigation_20XX.pdf (in Data/Corp Production Guides/)
#   Hard Wheat p.3, Canola p.8, Table Potato p.29 of each guide.
#
# 2018: no ICDC economics guide in folder (only variety guide).
#   Values estimated from 2019 guide with ~5% downward adjustment for costs,
#   wheat price ~SK market avg 2018 (~$5.50/bu CWRS #1).
#   Flag in Methods as estimated.
#
# irri_cost_fix_ac derivation:
#   = ICDC "Total Costs" ($/ac) − irri_cost_var_ac × ICDC avg inches applied
#   Wheat inches: 5.5 (2019), 5.1 (2020-2023)
#   Canola inches: 6.5 all years
#   Potato inches: 7 (2019), 6 (2020-2023)
#
# irri_cost_var_ac: ICDC "Irrigation power" cost ÷ inches applied
#   Wheat/Canola: $1.96/ac-in (2018-2020), $2.00/ac-in (2021-2023)
#   Potato: $1.82/ac-in (2018-2019), $1.95/ac-in (2020), $2.00/ac-in (2021),
#           $2.08/ac-in (2022-2023)
#
# dry_cost_ac: SK Agriculture Crop Planning Guide, Dark Brown soil zone
#   https://www.saskatchewan.ca/.../crop-planning-guide
#   Direct + operating costs for dryland (no irrigation component)
#
# Outputs:
#   Data Main Analysis/CropReturnDarkBrown.csv  (wheat + canola, 2018-2023)
#   Data Main Analysis/CropReturnPotato.csv     (potato, 2018-2023)

rm(list = ls())
library(tidyverse)

YEARS   <- 2018:2023
OUT_DIR <- "./Data Main Analysis"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ── 1. Prices (ICDC annual guides 2019-2023; 2018 estimated) ──────────────────

wheat_prices <- tibble(
  year     = YEARS,
  crop     = "wheat",
  # ICDC Hard Wheat p.3, "Price $/bu (#1 13.5%)" column
  price.bu = c(5.50,   # 2018: estimated (SK CWRS #1 market avg; no ICDC guide)
               6.75,   # 2019: ICDC guide
               6.42,   # 2020: ICDC guide
               6.04,   # 2021: ICDC guide
               10.56,  # 2022: ICDC guide
               10.15)  # 2023: ICDC guide
)

canola_prices <- tibble(
  year     = YEARS,
  crop     = "canola",
  # ICDC Canola p.8, "Price $/bu"
  price.bu = c(10.50,  # 2018: estimated
               11.59,  # 2019: ICDC guide
               10.70,  # 2020: ICDC guide
               11.25,  # 2021: ICDC guide
               17.01,  # 2022: ICDC guide
               17.61)  # 2023: ICDC guide
)

potato_prices <- tibble(
  year      = YEARS,
  # ICDC Table Potato p.29, "Price $/ton"
  price.ton = c(330,   # 2018: estimated (similar to 2019 market)
                330,   # 2019: ICDC guide
                330,   # 2020: ICDC guide
                397,   # 2021: ICDC guide
                397,   # 2022: ICDC guide
                397)   # 2023: ICDC guide (unchanged from 2022 in guide)
)

# ── 2. Irrigated costs (from ICDC total costs, minus variable pump component) ──

# irri_cost_fix_ac = ICDC "Total Costs" − irri_cost_var_ac × avg_inches_applied
# This is the "all-in" cost excluding the per-inch pump energy charge.
# Represents: seed, fertilizer, chemicals, insurance, labour, overhead,
#             irrigation repair, district levy, equipment capital, land.

costs_wheat <- tibble(
  year             = YEARS,
  crop             = "wheat",
  # SK Ag Crop Planning Guide, Dark Brown soil zone (dryland, no irrigation)
  dry_cost_ac      = c(317.67, 373.57, 360.39, 374.75, 421.38, 453.81),
  # ICDC total − irri_var × inches; wheat: 5.5in(2019), 5.1in(2020-2023)
  irri_cost_fix_ac = c(484.00,  # 2018: estimated (~2019 value)
                        484.65,  # 2019: $495.43 − $1.96×5.5
                        478.40,  # 2020: $488.40 − $1.96×5.1
                        527.64,  # 2021: $537.84 − $2.00×5.1
                        621.50,  # 2022: $631.70 − $2.00×5.1
                        769.09), # 2023: $779.29 − $2.00×5.1
  # ICDC "Irrigation power" $/ac ÷ applied inches
  irri_cost_var_ac = c(2.00, 1.96, 1.96, 2.00, 2.00, 2.00)
)

costs_canola <- tibble(
  year             = YEARS,
  crop             = "canola",
  # SK Ag Crop Planning Guide, Dark Brown soil zone (dryland)
  dry_cost_ac      = c(387.19, 444.84, 480.07, 498.32, 551.43, 589.22),
  # ICDC total − irri_var × 6.5in; all years 6.5in applied
  irri_cost_fix_ac = c(529.00,  # 2018: estimated
                        529.46,  # 2019: $542.20 − $1.96×6.5
                        586.81,  # 2020: $599.55 − $1.96×6.5
                        635.34,  # 2021: $648.34 − $2.00×6.5
                        671.40,  # 2022: $684.40 − $2.00×6.5
                        958.11), # 2023: $971.11 − $2.00×6.5
  irri_cost_var_ac = c(2.00, 1.96, 1.96, 2.00, 2.00, 2.00)
)

costs_potato <- tibble(
  year             = YEARS,
  # ICDC Table Potato total − irri_var × inches; 7in(2019), 6in(2020-2023)
  irri_cost_fix_ac = c(2317.00,  # 2018: estimated
                        2317.12,  # 2019: $2329.86 − $1.82×7
                        2355.80,  # 2020: $2367.50 − $1.95×6
                        2398.77,  # 2021: $2410.77 − $2.00×6
                        2535.01,  # 2022: $2547.51 − $2.08×6
                        2549.52), # 2023: $2562.02 − $2.08×6
  irri_cost_var_ac = c(1.82, 1.82, 1.95, 2.00, 2.08, 2.08)
)

# ── 3. Assemble and write CropReturnDarkBrown.csv ─────────────────────────────

grain_budget <- bind_rows(
  costs_wheat %>% left_join(wheat_prices,  by = c("year", "crop")),
  costs_canola %>% left_join(canola_prices, by = c("year", "crop"))
) %>%
  select(year, crop, dry_cost_ac, irri_cost_fix_ac, irri_cost_var_ac, price.bu)

write_csv(grain_budget, file.path(OUT_DIR, "CropReturnDarkBrown.csv"))
message(sprintf("Saved CropReturnDarkBrown.csv (%d rows)", nrow(grain_budget)))
print(grain_budget)

# ── 4. Assemble and write CropReturnPotato.csv ────────────────────────────────

potato_budget <- costs_potato %>%
  left_join(potato_prices, by = "year") %>%
  mutate(crop = "potato") %>%
  select(year, crop, irri_cost_fix_ac, irri_cost_var_ac, price.ton)

write_csv(potato_budget, file.path(OUT_DIR, "CropReturnPotato.csv"))
message(sprintf("Saved CropReturnPotato.csv (%d rows)", nrow(potato_budget)))
print(potato_budget)

# ── 5. Source summary ─────────────────────────────────────────────────────────

message("\n── Data sources ─────────────────────────────────────────────────────────")
message("Prices (2019-2023): ICDC Irrigation Economics and Agronomics annual guides")
message("  Crop_Planning_Guide_irrigation_20XX.pdf")
message("  Hard Wheat p.3, Canola p.8, Table Potato p.29")
message("Prices (2018):      Estimated — no ICDC economics guide for 2018 in folder")
message("Irrigated costs:    ICDC guide total $/ac minus variable pump cost")
message("Dryland costs:      SK Agriculture Crop Planning Guide, Dark Brown zone")
message("Variable pump rate: ICDC 'Irrigation power' row ÷ applied inches")
message("")
message("⚠  2018 values estimated. Flag in Methods as estimated/interpolated.")
message("⚠  dry_cost_ac from SK Ag dryland guide — verify against Dark Brown")
message("    zone PDFs (Crop_Planning_Guide_2018-2023.pdf) if precision needed.")
message("\nNext: run code_part3.R through code_part7.R")
