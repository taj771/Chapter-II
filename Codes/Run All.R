# Chapter II - The Economic Value of Irrigation Water under Policy and Environmental Changes

# Step 1 
#Download weather data from Daymet series and create Weather data file for AquaCrop
source("./codes/code_part1.R")

# Step 2 simulation crop production fucntion using AquaCrop-OS
# Python Code available at the folder named AquaCrop.py
# Wheat Average - No limits on irrigation Q
# 01.Wheat Average.py
# Canola Average - No limits on irrigation Q
# 02.Canola Average.py
# Potato Average - No limits on irrigation Q
# 03.Potato Average.py
# Wheat Marginal - Controlled iriigation
# 04.Wheat Marginal.py
# Canola Marginal - Controlled iriigation
# 05.Canola Marginal.py
# Potato Marginal - Controlled iriigation
# 06.Potatao Marginal.py
# Crop proudction simulation under climate chenge scenario CMIP245-controlled irrigation
# 12. Climate_chnage_CMIP245_PrcpRed.py

# Manuscript Tables and Figures
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
