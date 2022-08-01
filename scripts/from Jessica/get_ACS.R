path_dropbox = "~/BurkeLab Dropbox/Data"
path_github = "~/Documents/GitHub/smoke_PM_prediction/"

library(readxl)
library(sf)
library(dplyr)
library(tigris)
library(tidycensus)

census_api_key(readRDS("../../Registrations/US Census/us_census_data_api_key.rds"))

#-------------------------------------------------------------------------------
# Get US Census Data on Income, Race, and Ethnicity
# Written by Jessica
# Last edited February 2022
#-------------------------------------------------------------------------------
# Choose geography
acs_geography = "tract"

# Choose survey
acs_survey = 5

# Choose years
# year1 should be earlier than year2
acs_year1 = 2009
acs_year2 = 2020

# Get CONUS
conus = setdiff(states()$STUSPS, c("AK", "AS", "GU", "HI", "MP", "PR", "VI"))

#-------------------------------------------------------------------------------
# Get data for year1
#-------------------------------------------------------------------------------
# Set up directories
acs_year = acs_year1
path_survey = file.path(path_dropbox, 
                        "US Census Bureau/American Community Survey",
                        paste0(acs_survey, "-Year"))
path_year = file.path(path_survey, acs_year)
file_codebook = file.path(path_year, "codebook.rds")
path_geography = file.path(path_year, acs_geography)
path_raw = file.path(path_geography, "raw")
path_processed = file.path(path_geography, "processed")

# List of variables
vars_acs_all = load_variables(year = acs_year, dataset = paste0("acs", acs_survey), cache = TRUE)
if (!dir.exists(path_survey)) dir.create(path_survey)
if (!dir.exists(path_year)) dir.create(path_year)
if (!file.exists(file_codebook)) saveRDS(vars_acs_all, file_codebook)

# Variables we want to pull
# Choose from searching vars_acs_all
# Make sure they are the same as those you will use in year2
vars_acs = c(
  "B01003_001", # Total Population
  
  "B03002_003", # Not Hispanic/Latino; White alone
  "B03002_004", # Not Hispanic/Latino; Black or African American alone
  "B03002_005", # Not Hispanic/Latino; American Indian and Alaska Native alone
  "B03002_006", # Not Hispanic/Latino; Asian alone
  "B03002_007", # Not Hispanic/Latino; Native Hawaiian and Other Pacific Islander alone
  "B03002_008", # Not Hispanic/Latino; Some other race alone
  
  "B03001_003", # Hispanic/Latino
  
  "B17017_001", # Universe of Poverty
  "B17017_002", # Income in the past 12 months below poverty level
  
  "B19013_001", # Median household income in the past 12 months (in 2009 inflation-adjusted dollars)
  "B19301_001", # Per capita income in the past 12 months (in 2009 inflation-adjusted dollars)
  
  # "B22001_001", # Universe of Food Stamps
  # "B22001_002", # Household received Food Stamps/SNAP in the past 12 months
  
  "B25008_002", # Owner occupied
  "B25008_003"  # Renter occupied
)

#-------------------------------------------------------------------------------
if (!dir.exists(path_geography)) dir.create(path_geography)
if (!dir.exists(path_raw)) dir.create(path_raw)

# Pull and save raw data by state
for (i in 1:length(conus)) {
  print(paste("Downloading raw:", conus[i], Sys.time()))
  df = get_acs(geography = acs_geography,
               variables = vars_acs,
               output = "wide",
               year = acs_year,
               survey = paste0("acs", acs_survey),
               state = conus[i],
               geometry = T) %>% 
    select(GEOID, matches("^B.*E$"))
  saveRDS(df, file.path(path_raw, paste0("race_ethnicity_income_", conus[i], ".rds")))
}

#-------------------------------------------------------------------------------
# Process into more useful variables
if (!dir.exists(path_processed)) dir.create(path_processed)

# Get CPI
# Download data from https://www.bls.gov/cpi/research-series/r-cpi-u-rs-home.htm
cpi = read_excel(file.path(path_github, "data/r-cpi-u-rs-allitems.xlsx"),
                 skip = 5)
cpi1 = cpi %>% filter(YEAR == acs_year1) %>% pull(AVG)
cpi2 = cpi %>% filter(YEAR == acs_year2) %>% pull(AVG)
adjustment_factor = cpi2/cpi1

for (i in 1:length(conus)) {
  print(paste("Processing:", conus[i], Sys.time()))
  df = readRDS(file.path(path_raw, paste0("race_ethnicity_income_", conus[i], ".rds")))
  df = df %>% 
    # Intuitive variable names
    rename(
      Population_Total = B01003_001E,
      
      Ethnicity_NotHL_Race_White = B03002_003E,
      Ethnicity_NotHL_Race_Black = B03002_004E,
      Ethnicity_NotHL_Race_AIAN = B03002_005E,
      Ethnicity_NotHL_Race_Asian = B03002_006E,
      Ethnicity_NotHL_Race_NHPI = B03002_007E,
      Ethnicity_NotHL_Race_Other = B03002_008E,
      
      Ethnicity_HL = B03001_003E,
      
      Poverty_Total = B17017_001E,
      Poverty_BelowPovertyLevel = B17017_002E,
      
      Income_MedianHousehold = B19013_001E,
      Income_PerCapita = B19301_001E,
      
      # FoodStamps_Total = B22001_001E,
      # FoodStamps_ReceivedFoodStamps = B22001_002E,
      
      HouseholdOccupied_Owner = B25008_002E,
      HouseholdOccupied_Renter = B25008_003E
    ) %>% 
    # Change to percents
    mutate(
      Percent.NotHL.White = ifelse(Population_Total, Ethnicity_NotHL_Race_White/Population_Total, 0),
      Percent.NotHL.Black = ifelse(Population_Total, Ethnicity_NotHL_Race_Black/Population_Total, 0),
      Percent.NotHL.AIAN = ifelse(Population_Total, Ethnicity_NotHL_Race_AIAN/Population_Total, 0),
      Percent.NotHL.Asian = ifelse(Population_Total, Ethnicity_NotHL_Race_Asian/Population_Total, 0),
      Percent.NotHL.NHPI = ifelse(Population_Total, Ethnicity_NotHL_Race_NHPI/Population_Total, 0),
      Percent.NotHL.Other = ifelse(Population_Total, Ethnicity_NotHL_Race_Other/Population_Total, 0),
      
      Percent.HL = ifelse(Population_Total, Ethnicity_HL/Population_Total, 0),
      
      Percent.BelowPovertyLevel = ifelse(Poverty_Total, Poverty_BelowPovertyLevel/Poverty_Total, 0),
      # Percent.ReceivedFoodStamps = ifelse(FoodStamps_Total, FoodStamps_ReceivedFoodStamps/FoodStamps_Total, 0),
      Percent.Renter = ifelse(HouseholdOccupied_Owner + HouseholdOccupied_Renter, HouseholdOccupied_Renter/(HouseholdOccupied_Owner + HouseholdOccupied_Renter), 0)
    ) %>%
    select(
      GEOID,
      Total.Population = Population_Total,
      Percent.HL,
      
      Percent.NotHL.White,
      Percent.NotHL.Black,
      Percent.NotHL.AIAN,
      Percent.NotHL.Asian,
      Percent.NotHL.NHPI,
      Percent.NotHL.Other,
      
      Income.MedianHousehold = Income_MedianHousehold,
      Income.PerCapita = Income_PerCapita,
      Percent.BelowPovertyLevel,
      # Percent.ReceivedFoodStamps,
      Percent.Renter
    ) %>% 
    # Get inflation-adjusted values for variables measured in dollars
    mutate(
      !!sprintf("Income.MedianHousehold.%sDollars", acs_year2) := Income.MedianHousehold*adjustment_factor,
      !!sprintf("Income.PerCapita.%sDollars", acs_year2) := Income.PerCapita*adjustment_factor
    )
  
  # Save
  saveRDS(df, file.path(path_processed, paste0("race_ethnicity_income_", conus[i], ".rds")))
}

#-------------------------------------------------------------------------------
# Get data for year2
#-------------------------------------------------------------------------------
# Set up directories
acs_year = acs_year2
path_year = file.path(path_survey, acs_year)
file_codebook = file.path(path_year, "codebook.rds")
path_geography = file.path(path_year, acs_geography)
path_raw = file.path(path_geography, "raw")
path_processed = file.path(path_geography, "processed")

# List of variables
vars_acs_all = load_variables(year = acs_year, dataset = paste0("acs", acs_survey), cache = TRUE)
if (!dir.exists(path_survey)) dir.create(path_survey)
if (!dir.exists(path_year)) dir.create(path_year)
if (!file.exists(file_codebook)) saveRDS(vars_acs_all, file_codebook)

# Variables we want to pull
vars_acs = c(
  "B01003_001", # Total Population
  
  "B03002_003", # Not Hispanic/Latino; White alone
  "B03002_004", # Not Hispanic/Latino; Black or African American alone
  "B03002_005", # Not Hispanic/Latino; American Indian and Alaska Native alone
  "B03002_006", # Not Hispanic/Latino; Asian alone
  "B03002_007", # Not Hispanic/Latino; Native Hawaiian and Other Pacific Islander alone
  "B03002_008", # Not Hispanic/Latino; Some other race alone
  
  "B03001_003", # Hispanic/Latino
  
  "B17017_001", # Universe of Poverty
  "B17017_002", # Income in the past 12 months below poverty level
  
  "B19013_001", # Median household income in the past 12 months (in 2019 inflation-adjusted dollars)
  "B19301_001", # Per capita income in the past 12 months (in 2019 inflation-adjusted dollars)
  
  "B22003_001", # Universe of Food Stamps
  "B22003_002", # Household received Food Stamps/SNAP in the past 12 months
  
  "B25008_002", # Owner occupied
  "B25008_003"  # Renter occupied
)

#-------------------------------------------------------------------------------
if (!dir.exists(path_geography)) dir.create(path_geography)
if (!dir.exists(path_raw)) dir.create(path_raw)

# Pull and save raw data by state
for (i in 1:length(conus)) {
  print(paste("Downloading raw:", conus[i], Sys.time()))
  df = get_acs(geography = acs_geography,
               variables = vars_acs,
               output = "wide",
               year = acs_year,
               survey = paste0("acs", acs_survey),
               state = conus[i],
               geometry = T) %>% 
    select(GEOID, matches("^B.*E$"))
  saveRDS(df, file.path(path_raw, paste0("race_ethnicity_income_", conus[i], ".rds")))
}

#-------------------------------------------------------------------------------
# Process into more useful variables
if (!dir.exists(path_processed)) dir.create(path_processed)

for (i in 1:length(conus)) {
  print(paste("Processing:", conus[i], Sys.time()))
  df = readRDS(file.path(path_raw, paste0("race_ethnicity_income_", conus[i], ".rds")))
  df = df %>% 
    # Intuitive variable names
    rename(
      Population_Total = B01003_001E,
      
      Ethnicity_NotHL_Race_White = B03002_003E,
      Ethnicity_NotHL_Race_Black = B03002_004E,
      Ethnicity_NotHL_Race_AIAN = B03002_005E,
      Ethnicity_NotHL_Race_Asian = B03002_006E,
      Ethnicity_NotHL_Race_NHPI = B03002_007E,
      Ethnicity_NotHL_Race_Other = B03002_008E,
      
      Ethnicity_HL = B03001_003E,
      
      Poverty_Total = B17017_001E,
      Poverty_BelowPovertyLevel = B17017_002E,
      
      Income_MedianHousehold = B19013_001E,
      Income_PerCapita = B19301_001E,
      
      FoodStamps_Total = B22003_001E,
      FoodStamps_ReceivedFoodStamps = B22003_002E,
      
      HouseholdOccupied_Owner = B25008_002E,
      HouseholdOccupied_Renter = B25008_003E
    ) %>% 
    # Change to percents
    mutate(
      Percent.NotHL.White = ifelse(Population_Total, Ethnicity_NotHL_Race_White/Population_Total, 0),
      Percent.NotHL.Black = ifelse(Population_Total, Ethnicity_NotHL_Race_Black/Population_Total, 0),
      Percent.NotHL.AIAN = ifelse(Population_Total, Ethnicity_NotHL_Race_AIAN/Population_Total, 0),
      Percent.NotHL.Asian = ifelse(Population_Total, Ethnicity_NotHL_Race_Asian/Population_Total, 0),
      Percent.NotHL.NHPI = ifelse(Population_Total, Ethnicity_NotHL_Race_NHPI/Population_Total, 0),
      Percent.NotHL.Other = ifelse(Population_Total, Ethnicity_NotHL_Race_Other/Population_Total, 0),
      
      Percent.HL = ifelse(Population_Total, Ethnicity_HL/Population_Total, 0),
      
      Percent.BelowPovertyLevel = ifelse(Poverty_Total, Poverty_BelowPovertyLevel/Poverty_Total, 0),
      Percent.ReceivedFoodStamps = ifelse(FoodStamps_Total, FoodStamps_ReceivedFoodStamps/FoodStamps_Total, 0),
      Percent.Renter = ifelse(HouseholdOccupied_Owner + HouseholdOccupied_Renter, HouseholdOccupied_Renter/(HouseholdOccupied_Owner + HouseholdOccupied_Renter), 0)
    ) %>%
    select(
      GEOID,
      Percent.HL,
      
      Percent.NotHL.White,
      Percent.NotHL.Black,
      Percent.NotHL.AIAN,
      Percent.NotHL.Asian,
      Percent.NotHL.NHPI,
      Percent.NotHL.Other,
      
      Income.MedianHousehold = Income_MedianHousehold,
      Income.PerCapita = Income_PerCapita,
      Percent.BelowPovertyLevel,
      Percent.ReceivedFoodStamps,
      Percent.Renter
    )
  
  # Save
  saveRDS(df, file.path(path_processed, paste0("race_ethnicity_income_", conus[i], ".rds")))
}

#-------------------------------------------------------------------------------
# Get urban-rural classification
#-------------------------------------------------------------------------------
# Choose whether to simplify boundary lines for quicker processing
simplify = T

# Choose years
uac_year1 = 2013
uac_year2 = 2019

# Set up directories
path_uac = file.path(path_dropbox, "US Census Bureau/Urban and Rural")
if (!dir.exists(path_uac)) dir.create(path_uac)
if (simplify) {
  path_cb = file.path(path_uac, "Cartographic Boundary")
  if (!dir.exists(path_cb)) dir.create(path_cb)
  path_format = path_cb
} else {
  path_tl = file.path(path_uac, "TIGER-Line")
  if (!dir.exists(path_tl)) dir.create(path_tl)
  path_format = path_tl
}

# Get and save boundaries
uac_year = uac_year1
df = urban_areas(cb = simplify, year = uac_year, class = "sf")
saveRDS(df, file.path(path_format, paste0("uac_", uac_year, ".rds")))

uac_year = uac_year2
df = urban_areas(cb = simplify, year = uac_year, class = "sf")
saveRDS(df, file.path(path_format, paste0("uac_", uac_year, ".rds")))
