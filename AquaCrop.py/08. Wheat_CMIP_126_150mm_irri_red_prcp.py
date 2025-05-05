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
file_path = '/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP126_20redPrcp_ET.csv'
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
num_sites = 143  # Adjust as necessary

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

import os
import numpy as np
import pandas as pd
from aquacrop import AquaCropModel, Soil, Crop, InitialWaterContent, IrrigationManagement
from tqdm import tqdm
from scipy.optimize import fmin


def run_model(smts, max_irr_season, year1, year2, wdf):
    """
    Function to run AquaCrop model and return results for a given set of soil moisture targets.
    """
    wheat = Crop('Wheat',
                 planting_date='05/01',
                 harvest_date='10/30',
                 CropType=3,
                 Tbase=5,
                 Tupp=35,
                 Zmax=0.7,
                 WP=16,
                 Tmin_up=8,
                 Tmax_lo=40,
                 exc=50,
                 CGC=0.16764,
                 CCx=0.95,
                 CDC=0.13653,
                 Kcb=1.10,
                 fshape_r=15,
                 SxTopQ=0.020,
                 SxBotQ=0.005,
                 p_up4=0.8,
                 p_up2=0.55,
                 fshape_w1=4,
                 PlantPop=2000000)

    soil = Soil('LoamySand')
    init_wc = InitialWaterContent(wc_type='Pct', value=[70])
    irrmngt = IrrigationManagement(irrigation_method=1, SMT=smts, MaxIrrSeason=max_irr_season)

    model = AquaCropModel(f'{year1}/05/01', f'{year2}/10/31', wdf, soil, wheat,
                          irrigation_management=irrmngt, initial_water_content=init_wc)

    model.run_model(till_termination=True)
    return model.get_simulation_results()


def evaluate(smts, max_irr_season, wdf, test=False):
    """
    Run model and calculate yield and irrigation for given soil moisture targets.
    """
    out = run_model(smts, max_irr_season, year1=2035, year2=2035, wdf=wdf)

    yld = out['Fresh yield (tonne/ha)'].mean()
    tirr = out['Seasonal irrigation (mm)'].mean()
    reward = yld

    return (yld, tirr, reward) if test else -reward


def get_starting_point(num_smts, max_irr_season, num_searches, wdf):
    """
    Find an initial set of soil moisture thresholds for optimization.
    """
    x0list = np.random.rand(num_searches, num_smts) * 100
    rlist = [evaluate(xtest, max_irr_season, wdf) for xtest in x0list]
    return x0list[np.argmin(rlist)]


def optimize(num_smts, max_irr_season, wdf, num_searches=10):
    """
    Optimize soil moisture thresholds for profit maximization.
    """
    x0 = get_starting_point(num_smts, max_irr_season, num_searches, wdf)
    res = fmin(evaluate, x0, disp=0, args=(max_irr_season, wdf))
    return res.squeeze()

max_irr_values = [150]

from joblib import Parallel, delayed
import pandas as pd
from tqdm import tqdm

# Define site IDs
site_ids = range(1, 20)  # Adjust as needed

# Prepare weather data for all sites
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Run optimization
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Evaluate results

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }

# Parallel processing
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert results to DataFrame
results_df = pd.DataFrame(all_results)

# Save results to CSV
output_path = "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_1.csv"
results_df.to_csv(output_path, index=False)


# Assuming you have 300 sites
site_ids = range(20, 30)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites

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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_2.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(30, 40)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_3.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(40, 50)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_4.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(50, 60)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_5.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(60, 70)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_6.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(70, 79)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_7.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(80, 90)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_8.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(90, 100)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_9.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(100, 110)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_10.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(110, 120)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_11.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(120, 130)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_12.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(130, 140)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_13.csv",
    index=False)



import pandas as pd

# Load the two CSV files
df_1 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_1.csv")
df_2 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_2.csv")
df_3 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_3.csv")
df_4 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_4.csv")
df_5 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_5.csv")
df_6 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_6.csv")
df_7 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_7.csv")
df_8 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_8.csv")
df_9 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_9.csv")
df_10 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_10.csv")
df_11 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_11.csv")
df_12 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_12.csv")
df_13 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_13.csv")

# Merge the DataFrames
merged_results = pd.concat([df_1, df_2, df_3, df_4, df_5, df_6, df_7, df_8, df_9,
                            df_10, df_11, df_12, df_13], ignore_index=True)

# Save the merged DataFrame to a new CSV file
merged_results.to_csv(
    '/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatCMIP126/RedPrcp/150mm/WheatCMIP126_20redPrcp2035.csv',
    index=False)

# 40%
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
file_path = '/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP126_40redPrcp_ET.csv'
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
num_sites = 143  # Adjust as necessary

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

import os
import numpy as np
import pandas as pd
from aquacrop import AquaCropModel, Soil, Crop, InitialWaterContent, IrrigationManagement
from tqdm import tqdm
from scipy.optimize import fmin


def run_model(smts, max_irr_season, year1, year2, wdf):
    """
    Function to run AquaCrop model and return results for a given set of soil moisture targets.
    """
    wheat = Crop('Wheat',
                 planting_date='05/01',
                 harvest_date='10/30',
                 CropType=3,
                 Tbase=5,
                 Tupp=35,
                 Zmax=0.7,
                 WP=16,
                 Tmin_up=8,
                 Tmax_lo=40,
                 exc=50,
                 CGC=0.16764,
                 CCx=0.95,
                 CDC=0.13653,
                 Kcb=1.10,
                 fshape_r=15,
                 SxTopQ=0.020,
                 SxBotQ=0.005,
                 p_up4=0.8,
                 p_up2=0.55,
                 fshape_w1=4,
                 PlantPop=2000000)

    soil = Soil('LoamySand')
    init_wc = InitialWaterContent(wc_type='Pct', value=[70])
    irrmngt = IrrigationManagement(irrigation_method=1, SMT=smts, MaxIrrSeason=max_irr_season)

    model = AquaCropModel(f'{year1}/05/01', f'{year2}/10/31', wdf, soil, wheat,
                          irrigation_management=irrmngt, initial_water_content=init_wc)

    model.run_model(till_termination=True)
    return model.get_simulation_results()


def evaluate(smts, max_irr_season, wdf, test=False):
    """
    Run model and calculate yield and irrigation for given soil moisture targets.
    """
    out = run_model(smts, max_irr_season, year1=2035, year2=2035, wdf=wdf)

    yld = out['Fresh yield (tonne/ha)'].mean()
    tirr = out['Seasonal irrigation (mm)'].mean()
    reward = yld

    return (yld, tirr, reward) if test else -reward


def get_starting_point(num_smts, max_irr_season, num_searches, wdf):
    """
    Find an initial set of soil moisture thresholds for optimization.
    """
    x0list = np.random.rand(num_searches, num_smts) * 100
    rlist = [evaluate(xtest, max_irr_season, wdf) for xtest in x0list]
    return x0list[np.argmin(rlist)]


def optimize(num_smts, max_irr_season, wdf, num_searches=10):
    """
    Optimize soil moisture thresholds for profit maximization.
    """
    x0 = get_starting_point(num_smts, max_irr_season, num_searches, wdf)
    res = fmin(evaluate, x0, disp=0, args=(max_irr_season, wdf))
    return res.squeeze()

max_irr_values = [150]

from joblib import Parallel, delayed
import pandas as pd
from tqdm import tqdm

# Define site IDs
site_ids = range(1, 20)  # Adjust as needed

# Prepare weather data for all sites
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Run optimization
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Evaluate results

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }

# Parallel processing
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert results to DataFrame
results_df = pd.DataFrame(all_results)

# Save results to CSV
output_path = "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_1.csv"
results_df.to_csv(output_path, index=False)


# Assuming you have 300 sites
site_ids = range(20, 30)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites

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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_2.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(30, 40)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_3.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(40, 50)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_4.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(50, 60)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_5.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(60, 70)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_6.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(70, 79)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_7.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(80, 90)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_8.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(90, 100)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_9.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(100, 110)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_10.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(110, 120)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_11.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(120, 130)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_12.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(130, 140)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_13.csv",
    index=False)



import pandas as pd

# Load the two CSV files
df_1 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_1.csv")
df_2 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_2.csv")
df_3 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_3.csv")
df_4 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_4.csv")
df_5 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_5.csv")
df_6 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_6.csv")
df_7 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_7.csv")
df_8 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_8.csv")
df_9 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_9.csv")
df_10 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_10.csv")
df_11 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_11.csv")
df_12 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_12.csv")
df_13 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_13.csv")

# Merge the DataFrames
merged_results = pd.concat([df_1, df_2, df_3, df_4, df_5, df_6, df_7, df_8, df_9,
                            df_10, df_11, df_12, df_13], ignore_index=True)

# Save the merged DataFrame to a new CSV file
merged_results.to_csv(
    '/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatCMIP126/RedPrcp/150mm/WheatCMIP126_40redPrcp2035.csv',
    index=False)

# 60%
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
file_path = '/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP126_60redPrcp_ET.csv'
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
num_sites = 143  # Adjust as necessary

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

import os
import numpy as np
import pandas as pd
from aquacrop import AquaCropModel, Soil, Crop, InitialWaterContent, IrrigationManagement
from tqdm import tqdm
from scipy.optimize import fmin


def run_model(smts, max_irr_season, year1, year2, wdf):
    """
    Function to run AquaCrop model and return results for a given set of soil moisture targets.
    """
    wheat = Crop('Wheat',
                 planting_date='05/01',
                 harvest_date='10/30',
                 CropType=3,
                 Tbase=5,
                 Tupp=35,
                 Zmax=0.7,
                 WP=16,
                 Tmin_up=8,
                 Tmax_lo=40,
                 exc=50,
                 CGC=0.16764,
                 CCx=0.95,
                 CDC=0.13653,
                 Kcb=1.10,
                 fshape_r=15,
                 SxTopQ=0.020,
                 SxBotQ=0.005,
                 p_up4=0.8,
                 p_up2=0.55,
                 fshape_w1=4,
                 PlantPop=2000000)

    soil = Soil('LoamySand')
    init_wc = InitialWaterContent(wc_type='Pct', value=[70])
    irrmngt = IrrigationManagement(irrigation_method=1, SMT=smts, MaxIrrSeason=max_irr_season)

    model = AquaCropModel(f'{year1}/05/01', f'{year2}/10/31', wdf, soil, wheat,
                          irrigation_management=irrmngt, initial_water_content=init_wc)

    model.run_model(till_termination=True)
    return model.get_simulation_results()


def evaluate(smts, max_irr_season, wdf, test=False):
    """
    Run model and calculate yield and irrigation for given soil moisture targets.
    """
    out = run_model(smts, max_irr_season, year1=2035, year2=2035, wdf=wdf)

    yld = out['Fresh yield (tonne/ha)'].mean()
    tirr = out['Seasonal irrigation (mm)'].mean()
    reward = yld

    return (yld, tirr, reward) if test else -reward


def get_starting_point(num_smts, max_irr_season, num_searches, wdf):
    """
    Find an initial set of soil moisture thresholds for optimization.
    """
    x0list = np.random.rand(num_searches, num_smts) * 100
    rlist = [evaluate(xtest, max_irr_season, wdf) for xtest in x0list]
    return x0list[np.argmin(rlist)]


def optimize(num_smts, max_irr_season, wdf, num_searches=10):
    """
    Optimize soil moisture thresholds for profit maximization.
    """
    x0 = get_starting_point(num_smts, max_irr_season, num_searches, wdf)
    res = fmin(evaluate, x0, disp=0, args=(max_irr_season, wdf))
    return res.squeeze()

max_irr_values = [150]

from joblib import Parallel, delayed
import pandas as pd
from tqdm import tqdm

# Define site IDs
site_ids = range(1, 20)  # Adjust as needed

# Prepare weather data for all sites
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Run optimization
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Evaluate results

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }

# Parallel processing
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert results to DataFrame
results_df = pd.DataFrame(all_results)

# Save results to CSV
output_path = "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_1.csv"
results_df.to_csv(output_path, index=False)


# Assuming you have 300 sites
site_ids = range(20, 30)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites

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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_2.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(30, 40)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_3.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(40, 50)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_4.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(50, 60)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_5.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(60, 70)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_6.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(70, 79)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_7.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(80, 90)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_8.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(90, 100)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_9.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(100, 110)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_10.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(110, 120)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_11.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(120, 130)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_12.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(130, 140)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_13.csv",
    index=False)



import pandas as pd

# Load the two CSV files
df_1 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_1.csv")
df_2 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_2.csv")
df_3 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_3.csv")
df_4 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_4.csv")
df_5 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_5.csv")
df_6 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_6.csv")
df_7 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_7.csv")
df_8 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_8.csv")
df_9 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_9.csv")
df_10 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_10.csv")
df_11 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_11.csv")
df_12 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_12.csv")
df_13 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_13.csv")

# Merge the DataFrames
merged_results = pd.concat([df_1, df_2, df_3, df_4, df_5, df_6, df_7, df_8, df_9,
                            df_10, df_11, df_12, df_13], ignore_index=True)

# Save the merged DataFrame to a new CSV file
merged_results.to_csv(
    '/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatCMIP126/RedPrcp/150mm/WheatCMIP126_60redPrcp2035.csv',
    index=False)

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
file_path = '/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/ClimateData/CMIP6/ClimateProjforAquaCrop/CMIP126_80redPrcp_ET.csv'
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
num_sites = 143  # Adjust as necessary

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

import os
import numpy as np
import pandas as pd
from aquacrop import AquaCropModel, Soil, Crop, InitialWaterContent, IrrigationManagement
from tqdm import tqdm
from scipy.optimize import fmin


def run_model(smts, max_irr_season, year1, year2, wdf):
    """
    Function to run AquaCrop model and return results for a given set of soil moisture targets.
    """
    wheat = Crop('Wheat',
                 planting_date='05/01',
                 harvest_date='10/30',
                 CropType=3,
                 Tbase=5,
                 Tupp=35,
                 Zmax=0.7,
                 WP=16,
                 Tmin_up=8,
                 Tmax_lo=40,
                 exc=50,
                 CGC=0.16764,
                 CCx=0.95,
                 CDC=0.13653,
                 Kcb=1.10,
                 fshape_r=15,
                 SxTopQ=0.020,
                 SxBotQ=0.005,
                 p_up4=0.8,
                 p_up2=0.55,
                 fshape_w1=4,
                 PlantPop=2000000)

    soil = Soil('LoamySand')
    init_wc = InitialWaterContent(wc_type='Pct', value=[70])
    irrmngt = IrrigationManagement(irrigation_method=1, SMT=smts, MaxIrrSeason=max_irr_season)

    model = AquaCropModel(f'{year1}/05/01', f'{year2}/10/31', wdf, soil, wheat,
                          irrigation_management=irrmngt, initial_water_content=init_wc)

    model.run_model(till_termination=True)
    return model.get_simulation_results()


def evaluate(smts, max_irr_season, wdf, test=False):
    """
    Run model and calculate yield and irrigation for given soil moisture targets.
    """
    out = run_model(smts, max_irr_season, year1=2035, year2=2035, wdf=wdf)

    yld = out['Fresh yield (tonne/ha)'].mean()
    tirr = out['Seasonal irrigation (mm)'].mean()
    reward = yld

    return (yld, tirr, reward) if test else -reward


def get_starting_point(num_smts, max_irr_season, num_searches, wdf):
    """
    Find an initial set of soil moisture thresholds for optimization.
    """
    x0list = np.random.rand(num_searches, num_smts) * 100
    rlist = [evaluate(xtest, max_irr_season, wdf) for xtest in x0list]
    return x0list[np.argmin(rlist)]


def optimize(num_smts, max_irr_season, wdf, num_searches=10):
    """
    Optimize soil moisture thresholds for profit maximization.
    """
    x0 = get_starting_point(num_smts, max_irr_season, num_searches, wdf)
    res = fmin(evaluate, x0, disp=0, args=(max_irr_season, wdf))
    return res.squeeze()

max_irr_values = [150]

from joblib import Parallel, delayed
import pandas as pd
from tqdm import tqdm

# Define site IDs
site_ids = range(1, 20)  # Adjust as needed

# Prepare weather data for all sites
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in site_ids}

# Function to process each site and irrigation level
def process_site_irr_max(site_id, max_irr, wdf):
    smts = optimize(4, max_irr, wdf)  # Run optimization
    yld, tirr, _ = evaluate(smts, max_irr, wdf, True)  # Evaluate results

    return {
        'Site_ID': site_id,
        'Max_Irrigation_mm': max_irr,
        'Optimal_SMTs': smts.tolist(),
        'Yield_tonne_per_ha': yld,
        'Total_Irrigation_mm': tirr
    }

# Parallel processing
all_results = Parallel(n_jobs=15)(
    delayed(process_site_irr_max)(site_id, max_irr, wdfs[site_id])
    for max_irr in max_irr_values
    for site_id in site_ids
)

# Convert results to DataFrame
results_df = pd.DataFrame(all_results)

# Save results to CSV
output_path = "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_1.csv"
results_df.to_csv(output_path, index=False)


# Assuming you have 300 sites
site_ids = range(20, 30)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites

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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_2.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(30, 40)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_3.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(40, 50)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_4.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(50, 60)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_5.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(60, 70)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_6.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(70, 79)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_7.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(80, 90)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_8.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(90, 100)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_9.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(100, 110)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_10.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(110, 120)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_11.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(120, 130)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_12.csv",
    index=False)

# Assuming you have 300 sites
site_ids = range(130, 140)  # Adjust site IDs as needed
wdfs = {site_id: prepare_weather(f"ClimateData/site_{site_id}.txt") for site_id in
        site_ids}  # Prepare wdfs for all sites


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
results_df.to_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_13.csv",
    index=False)



import pandas as pd

# Load the two CSV files
df_1 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_1.csv")
df_2 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_2.csv")
df_3 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_3.csv")
df_4 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_4.csv")
df_5 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_5.csv")
df_6 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_6.csv")
df_7 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_7.csv")
df_8 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_8.csv")
df_9 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_9.csv")
df_10 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_10.csv")
df_11 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_11.csv")
df_12 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_12.csv")
df_13 = pd.read_csv(
    "/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatMarginal/TempDirectory/merged_results_13.csv")

# Merge the DataFrames
merged_results = pd.concat([df_1, df_2, df_3, df_4, df_5, df_6, df_7, df_8, df_9,
                            df_10, df_11, df_12, df_13], ignore_index=True)

# Save the merged DataFrame to a new CSV file
merged_results.to_csv(
    '/Users/tharakajayalath/Library/CloudStorage/OneDrive-UniversityofSaskatchewan/Chapter II-IrrigationValue/AquaCropOPSyData/WheatCMIP126/RedPrcp/150mm/WheatCMIP126_80redPrcp2035.csv',
    index=False)

