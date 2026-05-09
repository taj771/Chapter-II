import gc
import os
import psutil
import pandas as pd
import numpy as np

# Run garbage collection
gc.collect()

# Print system memory information
print(psutil.virtual_memory())

# Set environment variable
os.environ['DEVELOPMENT'] = 'True'

# Load the downloaded Daymet data
file_path = '/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/weather_data_Aqua.csv'
climate_data = pd.read_csv(file_path, on_bad_lines='skip')

# Check if 'Date' column exists
if 'Date' in climate_data.columns:
    # Convert 'Date' column to datetime
    climate_data['Date'] = pd.to_datetime(climate_data['Date'], errors='coerce')

    # Remove rows with missing Date values
    climate_data = climate_data.dropna(subset=['Date'])

    # Extract Day, Month, and Year from the Date column
    climate_data['Day'] = climate_data['Date'].dt.day
    climate_data['Month'] = climate_data['Date'].dt.month
    climate_data['Year'] = climate_data['Date'].dt.year

    # Extract climate variables
    e_a = climate_data['e_a']  # actual vapor pressure (kPa)
    T_max = climate_data['MaxTemp']  # Maximum temperature (°C)
    T_min = climate_data['MinTemp']  # Minimum temperature (°C)
    precipitation = climate_data['Precipitation']  # Precipitation (mm)
    solar_radiation = climate_data['R_n']  # Solar radiation (MJ/m²/day)

    # Calculate mean temperature
    T_mean = (T_max + T_min) / 2

    # Calculate saturation vapor pressure (e_s) in kPa
    e_s = 0.6108 * np.exp((17.27 * T_mean) / (T_mean + 237.3))

    # Define constants
    R_n = solar_radiation  # Net radiation (MJ/m²/day)
    G = 0  # Soil heat flux density (MJ/m²/day), often negligible
    gamma = 0.066  # Psychrometric constant (kPa/°C)
    u = 2  # Average wind speed in m/s (assumed)

    # Calculate slope of the saturation vapor pressure curve (Δ)
    delta = (4098 * e_s) / ((T_mean + 237.3) ** 2)

    # Calculate reference ET (ET₀) using the Penman-Monteith equation
    ReferenceET = (0.408 * delta * (R_n - G) + gamma * (900 / (T_mean + 273)) * u * (e_s - e_a)) / (
                delta + gamma * (1 + 0.34 * u))

    # Add ET₀ to the DataFrame
    climate_data['ReferenceET'] = ReferenceET

    # Reorder columns
    columns_order = ['Day', 'Month', 'Year'] + [col for col in climate_data.columns if
                                                col not in ['Day', 'Month', 'Year']]
    climate_data = climate_data[columns_order]

    # Save the DataFrame to a CSV file
    output_file = 'daymet_data_with_et0.csv'
    climate_data.to_csv(output_file, index=False)

    print(f"Reference ET₀ calculated and saved to '{output_file}'.")
else:
    print("The 'Date' column does not exist in the dataset.")

import os
import pandas as pd

# Load the data
df = pd.read_csv("daymet_data_with_et0.csv")

# Get a list of unique sites
unique_sites = df['site'].unique()

# Specify the directory where the weather files will be saved
weather_dir = "./ClimateData"
os.makedirs(weather_dir, exist_ok=True)

# Define the columns to select and rename
columns_to_select = ['Day', 'Month', 'Year', 'MinTemp', 'MaxTemp', 'Precipitation', 'ReferenceET']
columns_to_rename = {
    'MinTemp': 'Tmin(c)',
    'MaxTemp': 'Tmax(c)',
    'Precipitation': 'Prcp(mm)',
    'ReferenceET': 'Et0(mm)'
}

# Loop through each unique site and save its weather data to a text file
for site in unique_sites:
    df_filtered = df[df['site'] == site][columns_to_select].rename(columns=columns_to_rename)

    # Create a filename for the current site's weather data
    weather_filename = os.path.join(weather_dir, f"site_{site}.txt")

    # Save the filtered data to the weather file
    df_filtered.to_csv(weather_filename, sep='\t', index=False)
    print(f"Weather data saved for site {site}.")

import os
from aquacrop.utils.prepare_weather import prepare_weather

# Define the directory containing the weather files
input_dir = "./ClimateData"
prepared_wdf_dir = "./PreparedWDF"  # Directory to save prepared wdf

# Create the output directory if it does not exist
os.makedirs(prepared_wdf_dir, exist_ok=True)

# Number of sites
num_sites = 397  # Adjust as necessary

# Create a list of climate file names dynamically
climate_files = [f"site_{i}.txt" for i in range(1, num_sites + 1)]  # e.g., site_1.txt, site_2.txt, etc.

# Loop through each file name
for climate_file in climate_files:
    file_path = os.path.join(input_dir, climate_file)  # Construct the full file path

    if os.path.exists(file_path):  # Check if the file exists
        try:
            # Prepare weather data
            wdf = prepare_weather(file_path)

            # Save the prepared weather data to CSV
            prepared_wdf_path = os.path.join(prepared_wdf_dir, f"prepared_{climate_file}")
            wdf.to_csv(prepared_wdf_path, index=False)  # Assuming wdf can be converted to CSV

            print(f"Weather data prepared and saved for {climate_file}.")
        except Exception as e:
            print(f"Error processing {climate_file}: {e}")
    else:
        print(f"File not found: {file_path}")


# At this point, wdf_list will contain all the prepared weather data
import os
import numpy as np
import pandas as pd
from aquacrop import AquaCropModel, Soil, Crop, InitialWaterContent, IrrigationManagement
from tqdm import tqdm
from scipy.optimize import fmin  # Ensure fmin is imported for optimization


# Prepare the weather data

#wdf1 = prepare_weather("ClimateData/champion_climate1.txt")
#wdf2 = prepare_weather("ClimateData/champion_climate2.txt")

def run_model(smts, max_irr_season, year1, year2, wdf):
    """
    Function to run model and return results for a given set of soil moisture targets.
    """
    wheat = Crop('Sunflower',
                planting_date='04/01',
                harvest_date='11/30',
                CropType=3,  # Conservative parameters
                Tbase=5,
                Tupp=35,
                #Maturity=975,
                #Emergence=42,
                #MaxRooting=597,
                #Flowering=114,
                #Senescence=821,
                #HIstart=224,
                Zmax=0.7,
                WP=16,
                Tmin_up=8,
                Tmax_lo=40,
                exc=50,
                CGC=0.16764,
                CCx=0.95,
                CDC=0.13653,
                SeedSize=5,
                Kcb=1.10,
                fshape_r=15,
                SxTopQ=0.020,
                SxBotQ=0.005,
                p_up4=0.8,
                p_up2=0.55,
                fshape_w1=4,
                #PlantPop=500000
                )  # Plant population (plants/ha))  # Define crop
    soil = Soil('LoamySand')  # Define soil
    init_wc = InitialWaterContent(wc_type='Pct', value=[70])  # Define initial soil water conditions

    irrmngt = IrrigationManagement(irrigation_method=1, SMT=smts, MaxIrrSeason=max_irr_season)  # Define irrigation management

    # Create and run model
    model = AquaCropModel(f'{year1}/01/01', f'{year2}/12/31', wdf, soil, wheat,
                          irrigation_management=irrmngt, initial_water_content=init_wc)

    model.run_model(till_termination=True)
    return model.get_simulation_results()

def evaluate(smts, max_irr_season, wdf, test=False):
    """
    Function to run model and calculate reward (yield) for a given set of soil moisture targets.
    """
    # Run model # year chnage ### year chnage
    out = run_model(smts, max_irr_season, year1=2023, year2=2023, wdf=wdf)

    # Get yields and total irrigation
    yld = out['Fresh yield (tonne/ha)'].mean()
    tirr = out['Seasonal irrigation (mm)'].mean()

    reward = yld

    # Return either the negative reward (for the optimization)
    # or the yield and total irrigation (for analysis)
    if test:
        return yld, tirr, reward
    else:
        return -reward

# Modify get_starting_point to accept wdf
def get_starting_point(num_smts, max_irr_season, num_searches, wdf):
    """
    Find good starting threshold(s) for optimization.
    """
    # Get random SMT's
    x0list = np.random.rand(num_searches, num_smts) * 100
    rlist = []

    # Evaluate random SMT's
    for xtest in x0list:
        r = evaluate(xtest, max_irr_season, wdf)
        rlist.append(r)

    # Save best SMT
    x0 = x0list[np.argmin(rlist)]
    return x0

# Modify optimize to accept wdf
def optimize(num_smts, max_irr_season, wdf, num_searches=100):
    """
    Optimize thresholds to be profit-maximizing.
    """
    # Get starting optimization strategy
    x0 = get_starting_point(num_smts, max_irr_season, num_searches, wdf)

    # Run optimization
    res = fmin(evaluate, x0, disp=0, args=(max_irr_season, wdf))

    # Reshape array
    smts = res.squeeze()

    # Evaluate optimal strategy
    return smts

max_irr_values = [10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200]

from joblib import Parallel, delayed
import pandas as pd
from tqdm import tqdm

# Assuming you have 300 sites
site_ids = range(1, 20)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}  # Prepare wdfs for all sites

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Replace with your optimization function
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Replace with your evaluation function

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }



# Instead of creating a new list every time, we can use a preallocated list
results_list = []

# Parallel processing using joblib
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert the list of results to a DataFrame
results_df = pd.DataFrame(all_results)

# Save the results to a CSV file
results_df.to_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_1.csv", index=False)


# Assuming you have 300 sites
site_ids = range(20, 30)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}  # Prepare wdfs for all sites

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Replace with your optimization function
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Replace with your evaluation function

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }



# Instead of creating a new list every time, we can use a preallocated list
results_list = []

# Parallel processing using joblib
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert the list of results to a DataFrame
results_df = pd.DataFrame(all_results)

# Save the results to a CSV file
results_df.to_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_2.csv", index=False)


# Assuming you have 300 sites
site_ids = range(30, 40)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}  # Prepare wdfs for all sites

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Replace with your optimization function
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Replace with your evaluation function

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }



# Instead of creating a new list every time, we can use a preallocated list
results_list = []

# Parallel processing using joblib
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert the list of results to a DataFrame
results_df = pd.DataFrame(all_results)

# Save the results to a CSV file
results_df.to_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_3.csv", index=False)


# Assuming you have 300 sites
site_ids = range(40, 50)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}  # Prepare wdfs for all sites

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Replace with your optimization function
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Replace with your evaluation function

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }



# Instead of creating a new list every time, we can use a preallocated list
results_list = []

# Parallel processing using joblib
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert the list of results to a DataFrame
results_df = pd.DataFrame(all_results)

# Save the results to a CSV file
results_df.to_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_4.csv", index=False)


# Assuming you have 300 sites
site_ids = range(50, 60)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}  # Prepare wdfs for all sites

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Replace with your optimization function
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Replace with your evaluation function

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }



# Instead of creating a new list every time, we can use a preallocated list
results_list = []

# Parallel processing using joblib
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert the list of results to a DataFrame
results_df = pd.DataFrame(all_results)

# Save the results to a CSV file
results_df.to_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_5.csv", index=False)


# Assuming you have 300 sites
site_ids = range(60, 70)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}  # Prepare wdfs for all sites

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Replace with your optimization function
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Replace with your evaluation function

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }



# Instead of creating a new list every time, we can use a preallocated list
results_list = []

# Parallel processing using joblib
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert the list of results to a DataFrame
results_df = pd.DataFrame(all_results)

# Save the results to a CSV file
results_df.to_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_6.csv", index=False)


# Assuming you have 300 sites
site_ids = range(70, 80)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}  # Prepare wdfs for all sites

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Replace with your optimization function
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Replace with your evaluation function

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }

# Instead of creating a new list every time, we can use a preallocated list
results_list = []

# Parallel processing using joblib
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert the list of results to a DataFrame
results_df = pd.DataFrame(all_results)

# Save the results to a CSV file
results_df.to_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_7.csv", index=False)


# Assuming you have 300 sites
site_ids = range(80, 90)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}  # Prepare wdfs for all sites

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Replace with your optimization function
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Replace with your evaluation function

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }



# Instead of creating a new list every time, we can use a preallocated list
results_list = []

# Parallel processing using joblib
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert the list of results to a DataFrame
results_df = pd.DataFrame(all_results)

# Save the results to a CSV file
results_df.to_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_8.csv", index=False)


# Assuming you have 300 sites
site_ids = range(90, 100)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}  # Prepare wdfs for all sites

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Replace with your optimization function
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Replace with your evaluation function

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }



# Instead of creating a new list every time, we can use a preallocated list
results_list = []

# Parallel processing using joblib
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert the list of results to a DataFrame
results_df = pd.DataFrame(all_results)

# Save the results to a CSV file
results_df.to_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_9.csv", index=False)

# Assuming you have 300 sites
site_ids = range(100, 110)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}  # Prepare wdfs for all sites

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Replace with your optimization function
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Replace with your evaluation function

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }



# Instead of creating a new list every time, we can use a preallocated list
results_list = []

# Parallel processing using joblib
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert the list of results to a DataFrame
results_df = pd.DataFrame(all_results)

# Save the results to a CSV file
results_df.to_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_10.csv", index=False)


# Assuming you have 300 sites
site_ids = range(110, 120)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}  # Prepare wdfs for all sites

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Replace with your optimization function
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Replace with your evaluation function

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }

# Instead of creating a new list every time, we can use a preallocated list
results_list = []

# Parallel processing using joblib
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert the list of results to a DataFrame
results_df = pd.DataFrame(all_results)

# Save the results to a CSV file
results_df.to_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_11.csv", index=False)


# Assuming you have 300 sites
site_ids = range(120, 130)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}  # Prepare wdfs for all sites

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Replace with your optimization function
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Replace with your evaluation function

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }



# Instead of creating a new list every time, we can use a preallocated list
results_list = []

# Parallel processing using joblib
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert the list of results to a DataFrame
results_df = pd.DataFrame(all_results)

# Save the results to a CSV file
results_df.to_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_12.csv", index=False)

# Assuming you have 300 sites
site_ids = range(130, 140)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}  # Prepare wdfs for all sites

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Replace with your optimization function
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Replace with your evaluation function

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }



# Instead of creating a new list every time, we can use a preallocated list
results_list = []

# Parallel processing using joblib
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert the list of results to a DataFrame
results_df = pd.DataFrame(all_results)

# Save the results to a CSV file
results_df.to_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_13.csv", index=False)

# Assuming you have 300 sites
site_ids = range(140, 150)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}  # Prepare wdfs for all sites

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Replace with your optimization function
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Replace with your evaluation function

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }



# Instead of creating a new list every time, we can use a preallocated list
results_list = []

# Parallel processing using joblib
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert the list of results to a DataFrame
results_df = pd.DataFrame(all_results)

# Save the results to a CSV file
results_df.to_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_14.csv", index=False)


# Assuming you have 300 sites
site_ids = range(150, 160)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}  # Prepare wdfs for all sites

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Replace with your optimization function
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Replace with your evaluation function

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }



# Instead of creating a new list every time, we can use a preallocated list
results_list = []

# Parallel processing using joblib
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert the list of results to a DataFrame
results_df = pd.DataFrame(all_results)

# Save the results to a CSV file
results_df.to_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_15.csv", index=False)


# Assuming you have 300 sites
site_ids = range(160, 170)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}  # Prepare wdfs for all sites

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Replace with your optimization function
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Replace with your evaluation function

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }



# Instead of creating a new list every time, we can use a preallocated list
results_list = []

# Parallel processing using joblib
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert the list of results to a DataFrame
results_df = pd.DataFrame(all_results)

# Save the results to a CSV file
results_df.to_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_16.csv", index=False)


# Assuming you have 300 sites
site_ids = range(170, 180)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}  # Prepare wdfs for all sites

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Replace with your optimization function
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Replace with your evaluation function

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }



# Instead of creating a new list every time, we can use a preallocated list
results_list = []

# Parallel processing using joblib
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert the list of results to a DataFrame
results_df = pd.DataFrame(all_results)

# Save the results to a CSV file
results_df.to_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_17.csv", index=False)


# Assuming you have 300 sites
site_ids = range(180, 190)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}  # Prepare wdfs for all sites

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Replace with your optimization function
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Replace with your evaluation function

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }



# Instead of creating a new list every time, we can use a preallocated list
results_list = []

# Parallel processing using joblib
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert the list of results to a DataFrame
results_df = pd.DataFrame(all_results)

# Save the results to a CSV file
results_df.to_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_18.csv", index=False)


# Assuming you have 300 sites
site_ids = range(190, 200)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}  # Prepare wdfs for all sites

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Replace with your optimization function
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Replace with your evaluation function

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }

# Instead of creating a new list every time, we can use a preallocated list
results_list = []

# Parallel processing using joblib
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert the list of results to a DataFrame
results_df = pd.DataFrame(all_results)

# Save the results to a CSV file
results_df.to_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_19.csv", index=False)


# Assuming you have 300 sites
site_ids = range(200, 210)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}  # Prepare wdfs for all sites

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Replace with your optimization function
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Replace with your evaluation function

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }

# Instead of creating a new list every time, we can use a preallocated list
results_list = []

# Parallel processing using joblib
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert the list of results to a DataFrame
results_df = pd.DataFrame(all_results)

# Save the results to a CSV file
results_df.to_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_20.csv", index=False)


# Assuming you have 300 sites
site_ids = range(210, 220)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}  # Prepare wdfs for all sites

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Replace with your optimization function
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Replace with your evaluation function

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }



# Instead of creating a new list every time, we can use a preallocated list
results_list = []

# Parallel processing using joblib
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert the list of results to a DataFrame
results_df = pd.DataFrame(all_results)

# Save the results to a CSV file
results_df.to_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_21.csv", index=False)


# Assuming you have 300 sites
site_ids = range(220, 230)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}  # Prepare wdfs for all sites

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Replace with your optimization function
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Replace with your evaluation function

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }



# Instead of creating a new list every time, we can use a preallocated list
results_list = []

# Parallel processing using joblib
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert the list of results to a DataFrame
results_df = pd.DataFrame(all_results)

# Save the results to a CSV file
results_df.to_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_22.csv", index=False)


# Assuming you have 300 sites
site_ids = range(230, 240)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}  # Prepare wdfs for all sites

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Replace with your optimization function
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Replace with your evaluation function

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }



# Instead of creating a new list every time, we can use a preallocated list
results_list = []

# Parallel processing using joblib
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert the list of results to a DataFrame
results_df = pd.DataFrame(all_results)

# Save the results to a CSV file
results_df.to_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_23.csv", index=False)


# Assuming you have 300 sites
site_ids = range(240, 250)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}  # Prepare wdfs for all sites

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Replace with your optimization function
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Replace with your evaluation function

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }



# Instead of creating a new list every time, we can use a preallocated list
results_list = []

# Parallel processing using joblib
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert the list of results to a DataFrame
results_df = pd.DataFrame(all_results)

# Save the results to a CSV file
results_df.to_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_24.csv", index=False)


# Assuming you have 300 sites
site_ids = range(250, 260)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}  # Prepare wdfs for all sites

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Replace with your optimization function
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Replace with your evaluation function

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }



# Instead of creating a new list every time, we can use a preallocated list
results_list = []

# Parallel processing using joblib
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert the list of results to a DataFrame
results_df = pd.DataFrame(all_results)

# Save the results to a CSV file
results_df.to_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_25.csv", index=False)


# Assuming you have 300 sites
site_ids = range(260, 270)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}  # Prepare wdfs for all sites

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Replace with your optimization function
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Replace with your evaluation function

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }



# Instead of creating a new list every time, we can use a preallocated list
results_list = []

# Parallel processing using joblib
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert the list of results to a DataFrame
results_df = pd.DataFrame(all_results)

# Save the results to a CSV file
results_df.to_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_26.csv", index=False)


# Assuming you have 300 sites
site_ids = range(270, 280)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}  # Prepare wdfs for all sites

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Replace with your optimization function
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Replace with your evaluation function

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }



# Instead of creating a new list every time, we can use a preallocated list
results_list = []

# Parallel processing using joblib
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert the list of results to a DataFrame
results_df = pd.DataFrame(all_results)

# Save the results to a CSV file
results_df.to_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_27.csv", index=False)


# Assuming you have 300 sites
site_ids = range(280, 290)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}  # Prepare wdfs for all sites

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Replace with your optimization function
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Replace with your evaluation function

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }



# Instead of creating a new list every time, we can use a preallocated list
results_list = []

# Parallel processing using joblib
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert the list of results to a DataFrame
results_df = pd.DataFrame(all_results)

# Save the results to a CSV file
results_df.to_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_28.csv", index=False)


# Assuming you have 300 sites
site_ids = range(290, 300)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}  # Prepare wdfs for all sites

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Replace with your optimization function
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Replace with your evaluation function

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }



# Instead of creating a new list every time, we can use a preallocated list
results_list = []

# Parallel processing using joblib
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert the list of results to a DataFrame
results_df = pd.DataFrame(all_results)

# Save the results to a CSV file
results_df.to_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_29.csv", index=False)


# Assuming you have 300 sites
site_ids = range(300, 310)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}  # Prepare wdfs for all sites

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Replace with your optimization function
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Replace with your evaluation function

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }



# Instead of creating a new list every time, we can use a preallocated list
results_list = []

# Parallel processing using joblib
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert the list of results to a DataFrame
results_df = pd.DataFrame(all_results)

# Save the results to a CSV file
results_df.to_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_30.csv", index=False)

# Assuming you have 300 sites
site_ids = range(310, 320)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}  # Prepare wdfs for all sites

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Replace with your optimization function
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Replace with your evaluation function

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }



# Instead of creating a new list every time, we can use a preallocated list
results_list = []

# Parallel processing using joblib
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert the list of results to a DataFrame
results_df = pd.DataFrame(all_results)

# Save the results to a CSV file
results_df.to_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_31.csv", index=False)


# Assuming you have 300 sites
site_ids = range(320, 330)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}  # Prepare wdfs for all sites

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Replace with your optimization function
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Replace with your evaluation function

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }



# Instead of creating a new list every time, we can use a preallocated list
results_list = []

# Parallel processing using joblib
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert the list of results to a DataFrame
results_df = pd.DataFrame(all_results)

# Save the results to a CSV file
results_df.to_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_32.csv", index=False)


# Assuming you have 300 sites
site_ids = range(330, 340)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}  # Prepare wdfs for all sites

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Replace with your optimization function
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Replace with your evaluation function

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }



# Instead of creating a new list every time, we can use a preallocated list
results_list = []

# Parallel processing using joblib
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert the list of results to a DataFrame
results_df = pd.DataFrame(all_results)

# Save the results to a CSV file
results_df.to_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_33.csv", index=False)


# Assuming you have 300 sites
site_ids = range(340, 350)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}  # Prepare wdfs for all sites

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Replace with your optimization function
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Replace with your evaluation function

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }



# Instead of creating a new list every time, we can use a preallocated list
results_list = []

# Parallel processing using joblib
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert the list of results to a DataFrame
results_df = pd.DataFrame(all_results)

# Save the results to a CSV file
results_df.to_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_34.csv", index=False)

# Assuming you have 300 sites
site_ids = range(350, 360)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}  # Prepare wdfs for all sites

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Replace with your optimization function
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Replace with your evaluation function

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }



# Instead of creating a new list every time, we can use a preallocated list
results_list = []

# Parallel processing using joblib
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert the list of results to a DataFrame
results_df = pd.DataFrame(all_results)

# Save the results to a CSV file
results_df.to_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_35.csv", index=False)


# Assuming you have 300 sites
site_ids = range(360, 370)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}  # Prepare wdfs for all sites

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Replace with your optimization function
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Replace with your evaluation function

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }



# Instead of creating a new list every time, we can use a preallocated list
results_list = []

# Parallel processing using joblib
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert the list of results to a DataFrame
results_df = pd.DataFrame(all_results)

# Save the results to a CSV file
results_df.to_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_36.csv", index=False)


# Assuming you have 300 sites
site_ids = range(370, 380)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}  # Prepare wdfs for all sites

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Replace with your optimization function
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Replace with your evaluation function

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }



# Instead of creating a new list every time, we can use a preallocated list
results_list = []

# Parallel processing using joblib
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert the list of results to a DataFrame
results_df = pd.DataFrame(all_results)

# Save the results to a CSV file
results_df.to_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_37.csv", index=False)


# Assuming you have 300 sites
site_ids = range(380, 390)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}  # Prepare wdfs for all sites

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Replace with your optimization function
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Replace with your evaluation function

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }



# Instead of creating a new list every time, we can use a preallocated list
results_list = []

# Parallel processing using joblib
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert the list of results to a DataFrame
results_df = pd.DataFrame(all_results)

# Save the results to a CSV file
results_df.to_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_38.csv", index=False)


# Assuming you have 300 sites
site_ids = range(390, 398)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}  # Prepare wdfs for all sites

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Replace with your optimization function
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Replace with your evaluation function

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }



# Instead of creating a new list every time, we can use a preallocated list
results_list = []

# Parallel processing using joblib
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert the list of results to a DataFrame
results_df = pd.DataFrame(all_results)

# Save the results to a CSV file
results_df.to_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_39.csv", index=False)

# Load the two CSV files
df_1 = pd.read_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_1.csv")
df_2 = pd.read_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_2.csv")
df_3 = pd.read_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_3.csv")
df_4 = pd.read_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_4.csv")
df_5 = pd.read_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_5.csv")
df_6 = pd.read_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_6.csv")
df_7 = pd.read_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_7.csv")
df_8 = pd.read_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_8.csv")
df_9 = pd.read_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_9.csv")
df_10 = pd.read_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_10.csv")
df_11 = pd.read_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_11.csv")
df_12 = pd.read_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_12.csv")
df_13 = pd.read_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_13.csv")
df_14 = pd.read_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_14.csv")
df_15 = pd.read_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_15.csv")
df_16 = pd.read_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_16.csv")
df_17 = pd.read_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_17.csv")
df_18 = pd.read_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_18.csv")
df_19 = pd.read_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_19.csv")
df_20 = pd.read_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_20.csv")
df_21 = pd.read_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_21.csv")
df_22 = pd.read_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_22.csv")
df_23 = pd.read_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_23.csv")
df_24 = pd.read_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_24.csv")
df_25 = pd.read_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_25.csv")
df_26 = pd.read_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_26.csv")
df_27 = pd.read_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_27.csv")
df_28 = pd.read_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_28.csv")
df_29 = pd.read_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_29.csv")
df_30 = pd.read_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_30.csv")
df_31 = pd.read_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_31.csv")
df_32 = pd.read_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_32.csv")
df_33 = pd.read_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_33.csv")
df_34 = pd.read_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_34.csv")
df_35 = pd.read_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_35.csv")
df_36 = pd.read_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_36.csv")
df_37 = pd.read_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_37.csv")
df_38 = pd.read_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_38.csv")
df_39 = pd.read_csv("/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/TempDirectory/merged_results_39.csv")

# Merge the DataFrames
merged_results = pd.concat([df_1,df_2,df_3,df_4,df_5,df_6,df_7,df_8,df_9,
                            df_10,df_11,df_12,df_13,df_14,df_15,df_16,df_17,df_18,df_19,
                            df_20,df_21,df_22,df_23,df_24,df_25,df_26,df_27,df_28,df_29,
                            df_30,df_31,df_32,df_33,df_34,df_35,df_36,df_37,df_38,df_39], ignore_index=True)

# Save the merged DataFrame to a new CSV file
merged_results.to_csv('/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/CanolaMarginal/merged_simulation_results_canola_marginal_2023_irrigation.csv', index=False)


