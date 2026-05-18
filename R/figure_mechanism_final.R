# Final mechanism figure: revenue_rigid vs revenue_flex (3-crop, RedPrcp)
# Single panel — rigid vs flex bars, annotated benefit, grouped by climate year

library(tidyverse)
library(priceR)
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
  "2031" = "Driest\n(2031, P5)",
  "2044" = "Average\n(2044, P50)",
  "2050" = "Wettest\n(2050, P95)"
)

inflate <- function(x, yrs, to = 2023)
  adjust_for_inflation(x, as.Date(paste0(yrs, "-07-01")), "CA",
                       to_date = as.Date(paste0(to, "-07-01")))

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

opt_alloc <- readRDS(file.path(OUT_DIR, "site_benefits_candscu6_3crop.rds")) %>%
  filter(scenario == "RedPrcp") %>%
  mutate(year = as.integer(year))

results <- map_dfr(YEARS, function(yr) {
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

    rev_rigid <- (wf[[sid]](150) * 36.74 / 2.47) * bw$price.bu +
                 (cf[[sid]](150) * 44.09 / 2.47) * bc$price.bu +
                 (pf[[sid]](150) / 2.47)          * bp$price.ton

    rev_flex  <- (wf[[sid]](row$opt_wheat_mm)  * 36.74 / 2.47) * bw$price.bu +
                 (cf[[sid]](row$opt_canola_mm)  * 44.09 / 2.47) * bc$price.bu +
                 (pf[[sid]](row$opt_potato_mm)  / 2.47)          * bp$price.ton

    tibble(Site_ID = row$Site_ID, year = yr,
           rev_rigid = rev_rigid, rev_flex = rev_flex,
           rev_benefit = rev_flex - rev_rigid)
  })
}) %>%
  mutate(year_lab = factor(YEAR_LABS[as.character(year)], levels = YEAR_LABS))

medians <- results %>%
  group_by(year_lab) %>%
  summarise(
    rigid_med   = median(rev_rigid,   na.rm = TRUE),
    flex_med    = median(rev_flex,    na.rm = TRUE),
    benefit_med = median(rev_benefit, na.rm = TRUE),
    .groups     = "drop"
  ) %>%
  mutate(
    label = paste0("+$", round(benefit_med, 0), "/ac")
  )

cat("\n=== KEY NUMBERS ===\n")
print(medians %>% select(year_lab, rigid_med, flex_med, benefit_med))

bar_df <- medians %>%
  select(year_lab, rigid_med, flex_med) %>%
  pivot_longer(c(rigid_med, flex_med),
               names_to = "allocation", values_to = "revenue") %>%
  mutate(
    allocation = factor(allocation,
                        levels = c("rigid_med", "flex_med"),
                        labels = c("Rigid (equal 150 mm per crop)",
                                   "Flexible (optimal reallocation)"))
  )

alloc_cols <- c(
  "Rigid (equal 150 mm per crop)"  = "#d73027",
  "Flexible (optimal reallocation)" = "#4575b4"
)

p <- ggplot(bar_df, aes(x = year_lab, y = revenue, fill = allocation)) +
  geom_col(
    position = position_dodge(0.72), width = 0.65,
    colour = "grey25", linewidth = 0.3
  ) +
  geom_text(
    data = medians,
    aes(x = year_lab, y = flex_med + max(medians$flex_med) * 0.04,
        label = label),
    inherit.aes = FALSE,
    size = 3.2, colour = "grey15", lineheight = 0.9, vjust = 0
  ) +
  scale_fill_manual(values = alloc_cols, name = NULL) +
  scale_y_continuous(
    labels = scales::dollar_format(suffix = "/ac"),
    expand = expansion(mult = c(0, 0.20))
  ) +
  labs(
    x = "Representative climate year (growing-season precipitation quantile)",
    y = "Median gross revenue (CAD/ac)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position    = "top",
    legend.text        = element_text(size = 11),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x        = element_text(size = 11)
  )

ggsave(file.path(OUT_DIR, "fig_mechanism_final.png"),
       p, width = 9, height = 6, dpi = 300)
cat("\nSaved: fig_mechanism_final.png\n")
