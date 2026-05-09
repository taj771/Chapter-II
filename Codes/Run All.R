# Chapter II - The Economic Value of Irrigation Water under Policy and Environmental Changes

# Load sf package for spatial operations
library(sf)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(priceR)
library(tmap)



# Step 1 
#Download weather data from Daymet series and create Weather data file for AquaCrop
source("./codes/code_part1.R")

# Step 2 simulation crop production fucntion using AquaCrop-OS
# Python Code available at the folder named AquaCrop.py
# Wheat Average - No limits on irrigation Q
# AquaCrop-OS-part 1.ipynb
# Canola Average - No limits on irrigation Q
# AquaCrop-OS-part 2.ipynb
# Potato Average - No limits on irrigation Q
# AquaCrop-OS-part 3.ipynb
# Wheat Marginal - Controlled iriigation
# AquaCrop-OS-part 4.ipynb
# Canola Marginal - Controlled iriigation
# AquaCrop-OS-part 5.ipynb
# Potato Marginal - Controlled iriigation
# AquaCrop-OS-part 6.ipynb
# Crop proudction simulation under climate chenge scenario CMIP245-controlled irrigation
# AquaCrop-OS-part 7.py

# Manuscript Tables and Figures

# Figure 1 Lake Diefenbaker irrigation development area
source("./codes/code_part01.R")

# Figure 2 Cumulative precipitation and irrigation demand during the crop cycle for
# wheat, canola, and potato across 397 fields (2018–2023)
source("./codes/code_part2.R")

# Figure 3: Average shadow price of a cubic meter of irrigation water from 2018 to 2023.
source("./codes/code_part3.R")

# Figure 4: Spatial variation in the average weighted shadow price of irrigation water.
source("./codes/code_part4.R")

# Figure 5: Marginal value of irrigation water (m−3) at different irrigation levels for A) Wheat, B) Canola, and C) Potato.
source("./codes/code_part5.R")

# Figure 7: Net economic benefits (%) of flexible allocation of irrigation water
source("./codes/code_part6.R")

# Figure 8: Net economic benefits (%) of flexible irrigation water allocation under a 20%
# reduction in projected daily precipitation from 2030 to 2050.

source("./codes/code_part7.R")
