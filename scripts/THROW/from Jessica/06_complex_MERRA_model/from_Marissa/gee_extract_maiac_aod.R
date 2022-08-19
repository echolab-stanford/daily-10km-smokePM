library(tidyverse)
library(sf)
library(nngeo)
library(lubridate)
library(magrittr)
library(tigris)
library(data.table)
library(rgee)

# GEE extractions ----
ee_Initialize()

# the epa station locations, which are uploaded as a feature collection 
gee_epa_locs <- ee$FeatureCollection("users/marissac/epa_station_locations")

station_geom <- gee_epa_locs$geometry()

maiac <- ee$ImageCollection("MODIS/006/MCD19A2_GRANULES") %>% 
  ee$ImageCollection$select("Optical_Depth_055") %>% 
  ee$ImageCollection$filterBounds(station_geom) %>%
  ee$ImageCollection$filterDate("2006-01-01", "2021-01-01")

# Map$addLayer(maiac$first())

# as.Date("2021-01-01") - as.Date("2006-01-01") # check how many days it is

# the dataset in gee is granules, lets make a daily image collection to make this easier
maiac_daily <- ee$List$sequence(0, 5479)$map(ee_utils_pyfunc(function(x){
  im_date <- ee$Date$fromYMD(2006, 1, 1)$advance(x, "day")
  im_date_end <- im_date$advance(1, "day")
  maiac$filterDate(im_date, im_date_end)$reduce(ee$Reducer$median()) %>% return
})) %>% ee$ImageCollection$fromImages() %>% 
  ee$ImageCollection$toBands()
# maybe should have renamed the bands above to make them easy to find? should name each with AOD_date, don't know what names they'll have now

# extract the maiac data at the stations, looping through teh stations to avoid the computation timing out
station_maiac <- gee_epa_locs$map(function(feature) {
  feature$set(
    maiac_daily$reduceRegion(ee$Reducer$mean(), feature$geometry(), 1000)
  ) %>% ee$Feature$setGeometry(NULL) %>%
    return
})

# make a task to save the maiac station data to google drive
maiac_task <- ee_table_to_drive(
  collection = station_maiac,
  description = "epa_station_aod",
  fileFormat = "CSV"
)

# start the task
maiac_task$start()
