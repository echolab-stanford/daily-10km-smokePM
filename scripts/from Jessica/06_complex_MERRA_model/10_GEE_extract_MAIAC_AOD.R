source("work/06_complex_MERRA_model/00_utils.R")

library(tidyverse)
library(sf)
library(nngeo)
library(lubridate)
library(magrittr)
library(tigris)
library(data.table)
library(rgee)

#-------------------------------------------------------------------------------
# Extract MAIAC AOD Using Google Earth Engine
# Written by Marissa
# Executed with slight modification by Jessica
# Last edited August 2021
# 
# Upload epa_station_locations to your GEE Assets first. Modify start_date and 
# end_date before executing this script.
#-------------------------------------------------------------------------------
# Set null value
null_value <- -999999

# GEE extractions ----
ee_Initialize()

# the epa station locations, which are uploaded as a feature collection 
gee_epa_locs <- ee$FeatureCollection("users/jessssli/epa_station_locations")

station_geom <- gee_epa_locs$geometry()

# Set time range; end_date is exclusive
start_date <- "2020-01-01" # "2006-01-01"
end_date <- "2021-01-01" # "2021-01-01"
length_date <- difftime(end_date, start_date, units = "days") %>% as.integer()

# Get MAIAC
maiac <- ee$ImageCollection("MODIS/006/MCD19A2_GRANULES") %>% 
  ee$ImageCollection$select("Optical_Depth_055") %>% 
  ee$ImageCollection$filterBounds(station_geom) %>%
  ee$ImageCollection$filterDate(start_date, end_date)

# the dataset in gee is granules, lets make a daily image collection to make this easier
maiac_daily <- ee$List$sequence(0, length_date - 1)$map(ee_utils_pyfunc(function(x){
  im_date <- ee$Date$fromYMD(year(start_date), month(start_date), day(start_date))$advance(x, "day")
  im_date_end <- im_date$advance(1, "day")
  im <- maiac$filterDate(im_date, im_date_end)$reduce(ee$Reducer$median())
  im <- ee$Algorithms$If(
    ee$Algorithms$IsEqual(im$bandNames()$size(), ee$Number(0)),
    im$addBands(ee$Image$constant(null_value)$rename("Optical_Depth_055_median")), 
    im$unmask(null_value)
  )
  return(im)
})) %>% 
  ee$ImageCollection$fromImages() %>% 
  ee$ImageCollection$toBands()

# maybe should have renamed the bands above to make them easy to find? should 
# name each with AOD_date, don't know what names they'll have now
maiac_daily <- maiac_daily$
  rename(paste0("AOD_", seq(ymd(start_date), 
                            ymd(end_date) - 1, 
                            by = "days") %>% 
                  as.character()))

# extract the maiac data at the stations, looping through the stations to avoid 
# the computation timing out
station_maiac <- gee_epa_locs$map(function(feature) {
  feature$set(
    maiac_daily$reduceRegion(ee$Reducer$mean(), feature$geometry(), 1000)
  ) %>% ee$Feature$setGeometry(NULL) %>%
    return
})

# make a task to save the maiac station data to google drive
maiac_task <- ee_table_to_drive(
  collection = station_maiac,
  description = paste("epa_station_aod", start_date, end_date, sep = "_"),
  fileFormat = "CSV"
)

# start the task
print(paste("Working on start date:", start_date))
get_start_time()
maiac_task$start()
