# Mechanism chart: stacked grouped bars — revenue by crop, rigid vs flex
# Each year: 2 bars (rigid, flex) stacked by crop (wheat/canola/potato)
# Gap between bars = benefit; crop colours show where gain comes from

library(tidyverse)
library(priceR)
library(patchwork)
library(here)

ONEDRIVE <- paste0(
  "/Users/tharakajayalath/Library/CloudStorage",
  "/OneDrive-UniversityofSaskatchewan",
  "/Chapter II-IrrigationValue/Chapter-II/AquaCropOPSyData"
)
BUDGET_GRAIN  <- file.path(ONEDRIVE, "CropReturn/CropReturnDarkBrown.csv")
BUDGET_POTATO <- file.path(ONEDRIVE, "CropReturn/CropReturnPotato.csv")
RESULTS_DIR   <- here::here("CanDCSU6_results")
OUT_DIR       <- here::here("outputs/figures_candscu6")

YEARS <- c(2031, 2044, 2050)
YEAR_LABS <- c(
  "2031" = "Driest (2031)",
  "2044" = "Average (2044)",
  "2050" = "Wettest (2050)"
)

inflate <- function(x, yrs, to = 2023)
  adjust_for_inflation(x, as.Date(paste0(yrs, "-07-01")), "CA",
                       to_date = as.Date(paste0(to, "-07-01")))

# ── Prices ────────────────────────────────────────────────────────────────────
bg_grain <- read.csv(BUDGET_GRAIN, fileEncoding = "UTF-8-BOM")
bg_grain$price.bu <- inflate(bg_grain$price.bu, bg_grain$year)
budget_grain <- bg_grain %>%
  group_by(crop) %>%
  summarise(price.bu = mean(price.bu), .groups = "drop")
bw <- filter(budget_grain, crop == "wheat")
bc <- filter(budget_grain, crop == "canola")

bg_potato <- read.csv(BUDGET_POTATO, fileEncoding = "UTF-8-BOM")
bg_potato$price.ton <- inflate(bg_potato$price.ton, bg_potato$year)
bp <- bg_potato %>% summarise(price.ton = mean(price.ton))

# ── Helpers ───────────────────────────────────────────────────────────────────
fn_lookup <- function(fn_list) {
  ids <- map_dbl(fn_list, ~ .x$site)
  setNames(map(fn_list, ~ .x$fn), as.character(ids))
}

build_yield_fns <- function(csv_path) {
  df <- read_csv(csv_path, show_col_types = FALSE)
  df %>%
    group_by(Site_ID) %>%
    group_map(~ list(
      site = .y$Site_ID,
      fn   = approxfun(.x$Max_Irrigation_mm, .x$Yield_tonne_per_ha,
                       method = "linear", rule = 2)
    ))
}

rev_grain  <- function(y, price, conv) (y * conv / 2.47) * price
rev_potato <- function(y, price)       (y / 2.47) * price

# ── Compute per-site, per-crop revenues ───────────────────────────────────────
opt_alloc <- readRDS(file.path(OUT_DIR, "site_benefits_candscu6_3crop.rds")) %>%
  filter(scenario == "RedPrcp") %>%
  mutate(year = as.integer(year))

raw <- map_dfr(YEARS, function(yr) {
  cat("Year", yr, "...\n")

  wf <- fn_lookup(build_yield_fns(file.path(RESULTS_DIR, "RedPrcp",
    sprintf("WheatCanDCSU6_RedPrcp%d.csv",  yr))))
  cf <- fn_lookup(build_yield_fns(file.path(RESULTS_DIR, "RedPrcp",
    sprintf("CanolaCanDCSU6_RedPrcp%d.csv", yr))))
  pf <- fn_lookup(build_yield_fns(file.path(RESULTS_DIR, "RedPrcp",
    sprintf("PotatoCanDCSU6_RedPrcp%d.csv", yr))))

  opt_yr <- opt_alloc %>% filter(year == yr)

  map_dfr(seq_len(nrow(opt_yr)), function(i) {
    row <- opt_yr[i, ]
    sid <- as.character(row$Site_ID)
    if (!sid %in% names(wf)) return(NULL)

    tibble(
      Site_ID        = row$Site_ID,
      year           = yr,
      # Rigid revenues (150mm each)
      rigid_wheat    = rev_grain(wf[[sid]](150), bw$price.bu, 36.74),
      rigid_canola   = rev_grain(cf[[sid]](150), bc$price.bu, 44.09),
      rigid_potato   = rev_potato(pf[[sid]](150), bp$price.ton),
      # Flex revenues (optimal allocation)
      flex_wheat     = rev_grain(wf[[sid]](row$opt_wheat_mm),  bw$price.bu, 36.74),
      flex_canola    = rev_grain(cf[[sid]](row$opt_canola_mm), bc$price.bu, 44.09),
      flex_potato    = rev_potato(pf[[sid]](row$opt_potato_mm), bp$price.ton)
    )
  })
})

# ── Compute medians, reshape to long ──────────────────────────────────────────
med <- raw %>%
  mutate(year_lab = factor(YEAR_LABS[as.character(year)], levels = YEAR_LABS)) %>%
  group_by(year_lab) %>%
  summarise(across(starts_with("rigid_") | starts_with("flex_"), median, na.rm = TRUE),
            .groups = "drop")

cat("\n=== REVENUE MEDIANS BY CROP AND ALLOCATION ===\n")
print(med)

long <- med %>%
  pivot_longer(
    cols      = starts_with("rigid_") | starts_with("flex_"),
    names_to  = c("allocation", "crop"),
    names_sep = "_"
  ) %>%
  mutate(
    allocation = factor(allocation,
                        levels = c("rigid", "flex"),
                        labels = c("Rigid\n(150 mm/crop)", "Flexible\n(optimal)")),
    crop = factor(str_to_title(crop), levels = c("Potato", "Canola", "Wheat"))
  )

crop_cols <- c(
  "Wheat"  = "#e8a838",
  "Canola" = "#5aae61",
  "Potato" = "#9970ab"
)

# ── Revenue benefit annotation (total, per year) ──────────────────────────────
benefit_ann <- med %>%
  mutate(
    benefit   = (flex_wheat + flex_canola + flex_potato) -
                (rigid_wheat + rigid_canola + rigid_potato),
    flex_total = flex_wheat + flex_canola + flex_potato,
    label     = paste0("+$", round(benefit, 0), "/ac")
  )

cat("\n=== TOTAL BENEFIT (revenue) ===\n")
benefit_ann %>% select(year_lab, benefit, label) %>% print()

p <- ggplot(long, aes(x = allocation, y = value, fill = crop)) +
  geom_col(colour = "grey25", linewidth = 0.3, width = 0.75) +
  # Benefit arrow annotation
  geom_text(
    data = benefit_ann,
    aes(x = 1.5, y = flex_total + 80, label = label),
    inherit.aes = FALSE,
    size = 3.3, fontface = "bold", colour = "grey15"
  ) +
  geom_segment(
    data = benefit_ann,
    aes(x = 1.07, xend = 1.93,
        y = flex_total + 55, yend = flex_total + 55),
    inherit.aes = FALSE,
    arrow = arrow(ends = "both", length = unit(0.08, "inches")),
    colour = "grey30", linewidth = 0.4
  ) +
  scale_fill_manual(values = crop_cols, name = "Crop") +
  scale_y_continuous(
    labels = function(x) paste0("$", round(x, 0)),
    expand = expansion(mult = c(0, 0.15))
  ) +
  facet_wrap(~ year_lab, nrow = 1) +
  labs(
    title    = "Flexible allocation concentrates water on potato, raising total revenue",
    subtitle = paste0(
      "Three-crop rotation (wheat + canola + potato) | 20% precipitation reduction | 342 Saskatchewan sites\n",
      "Rigid: equal 150 mm per crop (450 mm total) | Flexible: optimal reallocation within same 450 mm budget\n",
      "Median gross revenue (yield × price, $/ac) — bars stacked by crop"
    ),
    x = NULL,
    y = "Median gross revenue ($/ac)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position  = "right",
    legend.text      = element_text(size = 10),
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(size = 8.5, colour = "grey30"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text       = element_text(face = "bold", size = 11),
    axis.text.x      = element_text(size = 10)
  )

ggsave(file.path(OUT_DIR, "fig_mechanism_stacked.png"),
       p, width = 11, height = 6, dpi = 300)
cat("\nSaved: fig_mechanism_stacked.png\n")
