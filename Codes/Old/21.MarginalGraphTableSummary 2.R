# Initialize an empty list to store results
mean_values_CMIP126 <- list()

# Define the years to loop over
years <- list(2030,2040,2050,2060,2080,2100)  # Sequence from 2030 to 2100 in 10-year steps

# Loop over the years
for (year in years) {
  c <- df_CMIP126 %>%
    filter(Year == year) %>%              # Filter for the current year
    filter(Irrigation_Level == 700)       # Filter for Irrigation_Level == 200
  
  # Calculate the mean and store it in the list
  mean_values_CMIP126[[as.character(year)]] <- mean(c$val_mm, na.rm = TRUE)
}

# Convert the list to a dataframe
result_CMIP126 <- data.frame(
  Year = names(mean_values_CMIP126),
  Mean_Val_mm = unlist(mean_values_CMIP126)
)%>%
  rename(val_CMIP126=Mean_Val_mm)

# CMIP245

# Initialize an empty list to store results
mean_values_CMIP245 <- list()

# Define the years to loop over
years <- list(2030,2040,2050,2060,2080,2100)  # Sequence from 2030 to 2100 in 10-year steps

# Loop over the years
for (year in years) {
  c <- df_CMIP245 %>%
    filter(Year == year) %>%              # Filter for the current year
    filter(Irrigation_Level == 700)       # Filter for Irrigation_Level == 200
  
  # Calculate the mean and store it in the list
  mean_values_CMIP126[[as.character(year)]] <- mean(c$val_mm, na.rm = TRUE)
}

# Convert the list to a dataframe
result_CMIP245 <- data.frame(
  Year = names(mean_values_CMIP126),
  Mean_Val_mm = unlist(mean_values_CMIP126)
)%>%
  rename(val_CMIP245=Mean_Val_mm)


# CMIP585

# Initialize an empty list to store results
mean_values_CMIP585 <- list()

# Define the years to loop over
years <- list(2030,2040,2050,2060,2080,2100)  # Sequence from 2030 to 2100 in 10-year steps

# Loop over the years
for (year in years) {
  c <- df_CMIP585 %>%
    filter(Year == year) %>%              # Filter for the current year
    filter(Irrigation_Level == 700)       # Filter for Irrigation_Level == 200
  
  # Calculate the mean and store it in the list
  mean_values_CMIP126[[as.character(year)]] <- mean(c$val_mm, na.rm = TRUE)
}

# Convert the list to a dataframe
result_CMIP585 <- data.frame(
  Year = names(mean_values_CMIP126),
  Mean_Val_mm = unlist(mean_values_CMIP126)
)%>%
  rename(val_CMIP585=Mean_Val_mm)




df_all <- result_CMIP126%>%
  left_join(result_CMIP245)%>%
  left_join(result_CMIP585)
  
