path_dropbox = "~/BurkeLab Dropbox/Data"
path_project = "~/BurkeLab Dropbox/Projects/smoke_PM_prediction"
path_github = "~/Documents/GitHub/smoke_PM_prediction"

library(lubridate)
library(sf)
library(dplyr)
library(rgee)

ee_Initialize(email = "jessssli@stanford.edu")

# ------------------------------------------------------------------------------
# Extract MODIS fire radiative power at HMS fire points
# Written by: Jessica Li
# Last edited: August 2022
# ------------------------------------------------------------------------------
# Set dates
start_date = "20060419"
end_date = "20060421" #"20220709"

# ------------------------------------------------------------------------------
#### Load fire points ####
fire = readRDS(file.path(path_dropbox, "fire/txt/hms_fires_20030401-20220711.RDS"))
crs_use = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Set bounding box
# Adapted from https://www.ospo.noaa.gov/Products/land/hms.html#about with generous margins
north = 72 + 20
south = 14.6 - 20
east = -50 + 40
west = -170 - 40

# Clean, combine, and crop
clean_fire = function(x) {
  x = x %>% 
    filter(Lon >= west, Lon <= east, Lat >= south, Lat <= north) %>% 
    mutate(ID = row_number(),
           Time = str_pad(Time, 4, "left", 0),
           across(c(Satellite, Method.of.Detect), trimws))
  return(x)
}
if (!file.exists(file.path(path_project, "impute_HYSPLIT_points", "fire_points.rds"))) {
  fire_points = fire %>%
    map_dfr(clean_fire) %>%
    st_as_sf(coords = c("Lon", "Lat"), remove = F, crs = crs_use) %>%
    filter(Time <= "2400", substr(Time, 3, 4) <= "59")
  # Note: Time is formatted HHMM
  
  saveRDS(fire_points, file.path(path_project, "impute_HYSPLIT_points", "fire_points.rds"))
} else {
  fire_points = readRDS(file.path(path_project, "impute_HYSPLIT_points", "fire_points.rds"))
}
rm(fire)

# Limit to unique fire points each day to reduce computation from duplicates
fire_points = fire_points %>% 
  st_drop_geometry() %>% 
  distinct(Lon, Lat, date) %>% 
  st_as_sf(coords = c("Lon", "Lat"), remove = F, crs = crs_use) %>% 
  sf_as_ee(fire_points)

# Load FRP
terra = ee$ImageCollection("MODIS/006/MOD14A1")
terra = terra$filterDate(format(ymd(start_date), "%Y-%m-%d"), format(ymd(end_date) + days(1), "%Y-%m-%d"))
terra = terra$select("MaxFRP")

aqua = ee$ImageCollection("MODIS/006/MYD14A1")
aqua = aqua$filterDate(format(ymd(start_date), "%Y-%m-%d"), format(ymd(end_date) + days(1), "%Y-%m-%d"))
aqua = aqua$select("MaxFRP")

# for each date
dates = format(seq.Date(ymd(start_date), ymd(end_date), by = "day"), "%Y-%m-%d")
for (d in dates) {
  # get fire points locations
  f = fire_points %>% filter(date == gsub("-", "", d))
  
  # get FRP image for that date
  te = terra$filterDate(d)
  
  # extract FRP from image
  r = te$reduceRegion(reducer = ee$Reducer$mean(),
                      geometry = f$geometry(),
                      scale = 1000)
  
  # save fire points that day
  ee_table_to_drive(r,
                    description = paste("terra_", d),
                    fileFormat = "CSV")
}