library(stringr)
library(sf)
library(dplyr)
library(purrr)

# ------------------------------------------------------------------------------
# Code for reading in and combining fire points and HYSPLIT points, respectively
# ------------------------------------------------------------------------------
#### Fire Points ####
fire = readRDS("~/BurkeLab Dropbox/Data/fire/txt/hms_fires_20060101-20220525.RDS")

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
fire_points = fire %>% 
  map_dfr(clean_fire) %>% 
  st_as_sf(coords = c("Lon", "Lat"), remove = F, crs = crs_use) %>% 
  filter(Time <= "2400", substr(Time, 3, 4) <= "59")
# Note: Time is formatted HHMM

# ------------------------------------------------------------------------------
#### HYSPLIT Points ####
# First HYSPLIT points with duration are from Apr 19, 2006
hysplit1 = readRDS("~/BurkeLab Dropbox/Data/hms_hysplit/hms_hysplit_points_20060419-20201231.rds")
# Most of 2021 was not in the archive, so starts at Oct 21, 2021
hysplit2 = readRDS("~/BurkeLab Dropbox/Data/hms_hysplit/hms_hysplit_points_20211021-20220522.rds")

# Combine into sf data frame
hysplit_points = bind_rows(hysplit1 %>% map_dfr(st_drop_geometry),
                           hysplit2 %>% map_dfr(st_drop_geometry)) %>% 
  filter(Lon >= west, Lon <= east, Lat >= south, Lat <= north) %>% 
  # There are 9 rows w/ Duration = "0000"
  filter(Duration.HHMM > "0000") %>% 
  st_as_sf(coords = c("Lon", "Lat"), remove = F, crs = crs_use) %>% 
  filter(Time.HHMM <= "2400", substr(Time.HHMM, 3, 4) <= "59")
# Note: ID is only unique within each date
