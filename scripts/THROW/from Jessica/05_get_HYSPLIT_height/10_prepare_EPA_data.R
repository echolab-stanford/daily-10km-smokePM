source("work/05_get_HYSPLIT_height/00_utils.R")

library(stringr)
library(rgdal)
library(units)
library(sf)
library(tigris)
library(dplyr)

#-------------------------------------------------------------------------------
# Prepare EPA Data
# Written by Jessica
# Last edited July 2021

# Get daily station average PM2.5 when there are multiple measurements. Filter 
# to EPA station-days in California 2020.
#-------------------------------------------------------------------------------
# Read in EPA station-days
dat_epa <- readRDS(paste0(path_dropbox, "PM25/EPA/epa_station_level_pm25_data.rds")) %>% 
  select(id, lon, lat, date, year, month, day, pm25, cbsa_code, cbsa_name) %>% 
  mutate(across(c(month, day), str_pad, 2, "left", 0),
         date = paste(year, month, day, sep = "-")) %>% 
  # Discard duplicate rows
  distinct()

# EPA sometimes records multiple PM2.5 values for the same station-day
# Average station-day PM2.5 values
dat_epa <- dat_epa %>% 
  group_by(across(c(-pm25))) %>% 
  summarize(pm25 = mean(pm25, na.rm = TRUE)) %>% 
  ungroup()

#-------------------------------------------------------------------------------
# Load EPA locations
loc_epa <- readOGR(paste0(path_dropbox, "PM25/epa_station_locations"), "epa_station_locations")
loc_epa <- st_as_sf(loc_epa)

# Load counties
us_counties <- counties()
us_counties <- us_counties %>% 
  st_transform(st_crs(loc_epa)) %>% 
  rename(state_fips = STATEFP, 
         county_fips = COUNTYFP, 
         county_name = NAME) %>% 
  select(state_fips, county_fips, county_name)

# Match EPA stations and counties
epa_counties <- st_join(loc_epa, us_counties, left = TRUE)

# Match EPA station-days to counties
dat_epa <- dat_epa %>% left_join(st_drop_geometry(epa_counties))

# Filter to California 2020
dat_epa <- dat_epa %>% filter(state_fips == "06", year == 2020)

# Save EPA station-days
saveRDS(dat_epa, paste0(path_project, "dat_epa.rds"))
