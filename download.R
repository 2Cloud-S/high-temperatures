# ------------------------------------------------------------------
# SCRIPT: Download and Save Seattle Daily Maximum Temperatures
# ------------------------------------------------------------------

# -- 1. LOAD LIBRARIES --
# Installs packages if you don't have them, then loads them.
if (!require(pacman)) install.packages("pacman")
pacman::p_load(worldmet, tidyverse, lubridate)

print("STEP 1: Libraries loaded.")

# -- 2. FETCH DATA --
# Define parameters for the data download from NOAA's ISD.
# Note: The download may take a few minutes.
station_code <- "727930-24233" # Seattle-Tacoma International Airport
start_year <- 1979
end_year <- 2025 # Fetches data up to the current year

print("STEP 2: Fetching raw hourly data from NOAA...")
seattle_raw_data <- importNOAA(
  code = station_code,
  year = start_year:end_year,
  quiet = FALSE # Shows download progress
)
print("...Raw data download complete.")

# -- 3. CLEAN & PROCESS DATA --
# This transforms the raw hourly data into a clean, daily summary.
print("STEP 3: Cleaning data and calculating daily maximums...")
seattle_daily_max_temps <- seattle_raw_data %>%
  # Ensure the main data column is present
  filter(!is.na(air_temp)) %>%
  
  # Create a 'day' column by converting the datetime.
  # Explicitly state the format to prevent errors.
  mutate(day = as.Date(date, format = "%Y-%m-%d")) %>%
  
  # Group all hourly data by each unique day
  group_by(day) %>%
  
  # Calculate the single highest temperature for each day's group
  summarise(
    daily_max_c = max(air_temp, na.rm = TRUE),
    .groups = 'drop' # Recommended to drop grouping after summarise
  ) %>%

  # Remove any days where a max temp couldn't be calculated
  filter(is.finite(daily_max_c)) %>%
  
  # Convert to Fahrenheit and add a day-of-year column for plotting
  mutate(
    daily_max_f = (daily_max_c * 9/5) + 32,
    day_of_year = yday(day)
  ) %>%
  
  # Keep only the most useful columns
  select(day, day_of_year, daily_max_f)

print("...Data cleaning complete.")
print("Final data preview:")
print(head(seattle_daily_max_temps))

# -- 4. SAVE FINAL DATA --
# Saves the clean data frame to a compressed file for easy reloading.
output_filename <- "seattle_daily_max_temps.rds"

print(paste("STEP 4: Saving final data to", output_filename))
write_rds(seattle_daily_max_temps, output_filename)

print("--- ALL DONE ---")