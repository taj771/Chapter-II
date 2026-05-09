# The Economic Value of Irrigation Water
**Chapter II — AquaCrop-OS Simulation Pipeline**

## Pipeline Overview

```
python/ simulations  →  Data Main Analysis/ (CSVs)  →  R/ analysis  →  Results
```

---

## Step 1: Python Simulations (`python/`)

Run in order:

| Script | What it does | Run where |
|--------|-------------|-----------|
| `config.py` | Crop params, paths, years — imported by other scripts | (imported) |
| `simulate_average.py` | Rainfed + irrigated average yield for wheat, canola, potato (2018–2023) | Local (joblib parallel) |
| `simulate_marginal_databricks.py` | Marginal yield–irrigation curves at 397 sites × 10–13 irrigation levels | Databricks cluster |
| `simulate_marginal.py` | Same as above but local joblib version (slower) | Local |
| `08–12.py` | Climate change projections (CMIP6 SSP126/245) for wheat, canola, potato | Local |

**Outputs** written to `Data Main Analysis/`:
- `wheat_rainfed_{year}.csv`, `wheat_netirridemand_{year}.csv`
- `canola_rainfed_{year}.csv`, `canola_netirridemand_{year}.csv`
- `potato_netirridemand_{year}.csv`
- `merged_simulation_results_{crop}_marginal_{year}_irrigation.csv`

---

## Step 2: R Econometric Analysis (`R/`)

Run sequentially (or use `Run All.R`):

| Script | What it does |
|--------|-------------|
| `code_part1.R` | Data preparation, site-level aggregation |
| `code_part2.R` | Net irrigation demand, yield summary |
| `code_part3.R` | Production function estimation (marginal yield curves) |
| `code_part4.R` | Water value calculation ($/mm, $/acre-foot) |
| `code_part5.R` | Spatial analysis and mapping |
| `code_part6.R` | Reallocation benefits analysis |
| `code_part7.R` / `code_part7_option2.R` | Climate change cost analysis |
| `utils.R` | Shared helper functions (imported by all parts) |

---

## Setup

### Python
```bash
pip install aquacrop pandas numpy scipy joblib
```

### Databricks (marginal simulations)
```bash
# Upload input data (once)
databricks fs cp daymet_data_with_et0.csv dbfs:/aquacrop/input/daymet_data_with_et0.csv

# Deploy and run
databricks bundle deploy
databricks bundle run simulate_marginal

# Download outputs when done
databricks fs cp -r dbfs:/aquacrop/output/ "./Data Main Analysis/"
```

### R
```r
# Required packages loaded in utils.R
# Run full pipeline:
source("R/Run All.R")
```

---

## Data
- Input: `daymet_data_with_et0.csv` — daily weather (Daymet) + ET₀ for 397 sites (2018–2023)
- Shapefiles: Lake Diefenbaker irrigation district boundary
- Crop return data: `CropReturnDarkBrown.csv`, `CropReturnPotato.csv`

Data files stored on OneDrive, not tracked in git.
