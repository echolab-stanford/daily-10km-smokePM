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
#-------------------------------------------------------------------------------
# GEE extractions ----
ee_Initialize()

# the epa station locations, which are uploaded as a feature collection 
gee_epa_locs <- ee$FeatureCollection("users/jessssli/epa_station_locations")

station_geom <- gee_epa_locs$geometry()

maiac <- ee$ImageCollection("MODIS/006/MCD19A2_GRANULES")$
  filterDate("2016-05-01", "2016-05-02")

Map$addLayer(maiac$first(),
             list(min = 0,
                  max = 500,
                  bands = "Optical_Depth_055"),
             "AOD")

landsat <- ee$ImageCollection("LANDSAT/LC08/C01/T1_TOA")$
  filterDate("2016-05-01", "2016-05-02")

Map$addLayer(landsat$first(),
             list(min = 0,
                  max = 0.2,
                  bands = c("B4", "B3", "B2")),
             "true color")


maiac <- ee$ImageCollection("MODIS/006/MCD19A2_GRANULES") %>% 
  # ee$ImageCollection$filterBounds(station_geom) %>%
  ee$ImageCollection$filterDate("2016-05-01", "2016-05-02") %>% 
  # ee$ImageCollection$filterDate("2006-12-30", "2007-01-01") %>% 
  ee$ImageCollection$select("Optical_Depth_055")
# ee$ImageCollection$filterDate("2006-01-01", "2021-01-01")

Map$addLayer(maiac$first(),
             list(min = 0,
                  max = 500),
             'aod')

# as.Date("2021-01-01") - as.Date("2006-01-01") # check how many days it is

# the dataset in gee is granules, lets make a daily image collection to make this easier
maiac_daily <- ee$List$sequence(0, 5479)$map(ee_utils_pyfunc(function(x){
  im_date <- ee$Date$fromYMD(2006, 12, 30)$advance(x, "day")
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
get_start_time()
maiac_task$start()




























































library(rgee)

ee_clean_pyenv()
ee_Initialize()

modis <- ee$ImageCollection('MODIS/006/MCD19A2_GRANULES')$filterDate('2017-01-01', '2017-01-02')
ee_print(modis)

maiac <- modis$select('Optical_Depth_055')
class(maiac)
ee_print(maiac)

aod <- maiac$select('Optical_Depth_055')$toBands()
class(aod)
ee_print(aod)

aod01jan <- aod$select('MCD19A2_A2017001_h00v08_006_2018114112301_02_Optical_Depth_055')
Map$addLayer(
  maiac$median(),
  list(min = 0,
       max = 500),
  "MODIS MAIAC AOD 2017-01-01"
)





























































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
#-------------------------------------------------------------------------------
# GEE extractions ----
ee_Initialize()

# the epa station locations, which are uploaded as a feature collection 
gee_epa_locs <- ee$FeatureCollection("users/jessssli/epa_station_locations")

station_geom <- gee_epa_locs$geometry()

maiac <- ee$ImageCollection("MODIS/006/MCD19A2_GRANULES") %>% 
  ee$ImageCollection$select("Optical_Depth_055") %>% 
  ee$ImageCollection$filterBounds(station_geom) %>%
  ee$ImageCollection$filterDate("2006-12-30", "2007-01-01")
# ee$ImageCollection$filterDate("2006-01-01", "2021-01-01")

# Map$addLayer(maiac$first())

# as.Date("2021-01-01") - as.Date("2006-01-01") # check how many days it is

# the dataset in gee is granules, lets make a daily image collection to make this easier
maiac_daily <- ee$List$sequence(0, 1)$map(ee_utils_pyfunc(function(x){
  im_date <- ee$Date$fromYMD(2006, 12, 30)$advance(x, "day")
  im_date_end <- im_date$advance(1, "day")
  maiac$filterDate(im_date, im_date_end)$reduce(ee$Reducer$median()) %>% return
})) %>% ee$ImageCollection$fromImages() %>% 
  ee$ImageCollection$toBands()

maiac_daily1 <- maiac_daily$
  rename(paste0("AOD_", seq(as.Date("2006-12-30"), as.Date("2007-01-01") - 1, by = "days") %>% as.character()))
# maybe should have renamed the bands above to make them easy to find? should name each with AOD_date, don't know what names they'll have now

# extract the maiac data at the stations, looping through teh stations to avoid the computation timing out
station_maiac <- gee_epa_locs$map(function(feature) {
  feature$set(
    "col", maiac_daily1$reduceRegion(ee$Reducer$mean(), feature$geometry(), 1000)
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
get_start_time()
maiac_task$start()






















































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
#-------------------------------------------------------------------------------
# GEE extractions ----
ee_Initialize()

# the epa station locations, which are uploaded as a feature collection 
gee_epa_locs <- ee$FeatureCollection("users/jessssli/epa_station_locations")

station_geom <- gee_epa_locs$geometry()

maiac <- ee$ImageCollection("MODIS/006/MCD19A2_GRANULES") %>% 
  ee$ImageCollection$select("Optical_Depth_055") %>% 
  ee$ImageCollection$filterBounds(station_geom) %>%
  ee$ImageCollection$filterDate("2006-12-30", "2007-01-01")
# ee$ImageCollection$filterDate("2006-01-01", "2021-01-01")

# Map$addLayer(maiac$first())

# as.Date("2021-01-01") - as.Date("2006-01-01") # check how many days it is

# the dataset in gee is granules, lets make a daily image collection to make this easier
maiac_daily <- ee$List$sequence(0, 1)$map(ee_utils_pyfunc(function(x){
  im_date <- ee$Date$fromYMD(2006, 12, 30)$advance(x, "day")
  im_date_end <- im_date$advance(1, "day")
  maiac$filterDate(im_date, im_date_end)$reduce(ee$Reducer$median()) %>% return
})) %>% ee$ImageCollection$fromImages() #%>% 
# ee$ImageCollection$toBands()

# maiac_daily1 <- maiac_daily$
#   rename(paste0("AOD_", seq(as.Date("2006-12-30"), as.Date("2007-01-01") - 1, by = "days") %>% as.character()))
# maybe should have renamed the bands above to make them easy to find? should name each with AOD_date, don't know what names they'll have now

# extract the maiac data at the stations, looping through teh stations to avoid the computation timing out
station_maiac <- gee_epa_locs$map(function(feature) {
  feature$set( "col",
               maiac_daily$map(function(image) {
                 image$reduceRegion(ee$Reducer$mean(), feature$geometry(), 1000) %>% return
               })
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
get_start_time()
maiac_task$start()

































































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
#-------------------------------------------------------------------------------
# Set time range; end_date is exclusive
start_date <- "2006-01-01"
end_date <- "2006-02-01" # "2021-01-01"
length_date <- difftime(end_date, start_date, units = "days") %>% as.integer()

# GEE extractions ----
ee_Initialize()

# the epa station locations, which are uploaded as a feature collection 
gee_epa_locs <- ee$FeatureCollection("users/jessssli/epa_station_locations")

station_geom <- gee_epa_locs$geometry()

maiac <- ee$ImageCollection("MODIS/006/MCD19A2_GRANULES") %>% 
  ee$ImageCollection$select("Optical_Depth_055") %>% 
  ee$ImageCollection$filterBounds(station_geom) %>%
  ee$ImageCollection$filterDate(start_date, end_date)

# Map$addLayer(maiac$first())

# as.Date("2021-01-01") - as.Date("2006-01-01") # check how many days it is

# the dataset in gee is granules, lets make a daily image collection to make this easier
maiac_daily <- ee$List$sequence(0, length_date - 1)$map(ee_utils_pyfunc(function(x){
  im_date <- ee$Date$fromYMD(year(start_date), month(start_date), day(start_date))$advance(x, "day")
  im_date_end <- im_date$advance(1, "day")
  maiac$filterDate(im_date, im_date_end)$reduce(ee$Reducer$median()) %>% return
})) %>% ee$ImageCollection$fromImages() %>% 
  ee$ImageCollection$toBands()

# maybe should have renamed the bands above to make them easy to find? should name each with AOD_date, don't know what names they'll have now
maiac_daily <- maiac_daily$
  rename(paste0("AOD_", seq(ymd(start_date), 
                            ymd(end_date) - 1, 
                            by = "days") %>% 
                  as.character()))


# extract the maiac data at the stations, looping through teh stations to avoid the computation timing out
station_maiac <- gee_epa_locs$map(function(feature) {
  feature$set(
    "aod", maiac_daily$reduceRegion(ee$Reducer$mean(), feature$geometry(), 1000)
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
get_start_time()
maiac_task$start()
































































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
#-------------------------------------------------------------------------------
# Set time range; end_date is exclusive
start_date <- "2006-01-01"
end_date <- "2006-02-01" # "2021-01-01"
length_date <- difftime(end_date, start_date, units = "days") %>% as.integer()

# GEE extractions ----
ee_Initialize()

# the epa station locations, which are uploaded as a feature collection 
gee_epa_locs <- ee$FeatureCollection("users/jessssli/epa_station_locations")

station_geom <- gee_epa_locs$geometry()

maiac <- ee$ImageCollection("MODIS/006/MCD19A2_GRANULES") %>% 
  ee$ImageCollection$select("Optical_Depth_055") %>% 
  ee$ImageCollection$filterBounds(station_geom) %>%
  ee$ImageCollection$filterDate(start_date, end_date)

# Map$addLayer(maiac$first())

# as.Date("2021-01-01") - as.Date("2006-01-01") # check how many days it is

# the dataset in gee is granules, lets make a daily image collection to make this easier
maiac_daily <- ee$List$sequence(0, length_date - 1)$map(ee_utils_pyfunc(function(x){
  im_date <- ee$Date$fromYMD(year(start_date), month(start_date), day(start_date))$advance(x, "day")
  im_date_end <- im_date$advance(1, "day")
  maiac$filterDate(im_date, im_date_end)$reduce(ee$Reducer$median()) %>% return
})) %>% ee$ImageCollection$fromImages() %>% 
  ee$ImageCollection$toBands()

# maybe should have renamed the bands above to make them easy to find? should name each with AOD_date, don't know what names they'll have now
maiac_daily <- maiac_daily$
  rename(paste0("AOD_", seq(ymd(start_date), 
                            ymd(end_date) - 1, 
                            by = "days") %>% 
                  as.character()))


# extract the maiac data at the stations, looping through teh stations to avoid the computation timing out
station_maiac <- gee_epa_locs$map(function(feature) {
  aod <-  maiac_daily$reduceRegion(ee$Reducer$mean(), feature$geometry(), 1000)
  aod <- ee$List([aod$get, -9999])$reduce(ee$Reducer$firstNonNull())
  feature$set(aod) %>% ee$Feature$setGeometry(NULL) %>%
    return
})

# make a task to save the maiac station data to google drive
maiac_task <- ee_table_to_drive(
  collection = station_maiac,
  description = "epa_station_aod",
  fileFormat = "CSV"
)

# start the task
get_start_time()
maiac_task$start()





























































test1 <- maiac_daily %>%
  ee$ImageCollection$map(function(x) {
    ee$List(list(x$get("Optical_Depth_055_median"), -9999)) %>% ee$List$reduce(ee$Reducer$firstNonNull()) %>% return})
ee_print(test1)

test2 <- maiac_daily %>%
  ee$ImageCollection$map(function(img) {
    ee$List(list(img$get("Optical_Depth_055_median"), -9999)) %>%
      ee$List$reduce(ee$Reducer$firstNonNull()) %>%
      return
  })
ee_print(test2)

test3 <- maiac_daily %>% 
  ee$ImageCollection$map(function(img) {
    img$unmask(-9999) %>% return
  })
ee_print(test3)

test4 <- test3 %>% 
  ee$ImageCollection$toBands()
ee_print(test4)

test5 <- test4 %>% ee$ImageCollection$toBands()
ee_print(test5)

test6 <- ee$List$sequence(0, length_date - 1)$map(ee_utils_pyfunc(function(x){
  im_date <- ee$Date$fromYMD(year(start_date), month(start_date), day(start_date))$advance(x, "day")
  im_date_end <- im_date$advance(1, "day")
  maiac$filterDate(im_date, im_date_end) %>% ee$List$reduce(ee$Reducer$median()) %>% return
})) %>% ee$ImageCollection$fromImages() %>% 
  ee$ImageCollection$map(ee_utils_pyfunc(function(img) {
    img$unmask(-9999) %>% return
  }))
ee_print(test6)

test7 <- ee$List$sequence(0, length_date - 1)$map(ee_utils_pyfunc(function(x){
  im_date <- ee$Date$fromYMD(year(start_date), month(start_date), day(start_date))$advance(x, "day")
  im_date_end <- im_date$advance(1, "day")
  im <- maiac$filterDate(im_date, im_date_end) %>% 
    ee$List$reduce(ee$Reducer$median())
  
  ee$List(list(im, -9999)) %>% 
    ee$List$reduce(ee$Reducer$firstNonNull()) %>% 
    return
})) %>% ee$ImageCollection$fromImages()
ee_print(test7)

































base <- ee$List$sequence(0, length_date - 1)$map(ee_utils_pyfunc(function(x){
  im_date <- ee$Date$fromYMD(year(start_date), month(start_date), day(start_date))$advance(x, "day")
  im_date_end <- im_date$advance(1, "day")
  maiac$filterDate(im_date, im_date_end)$reduce(ee$Reducer$median()) %>% return
})) %>% ee$ImageCollection$fromImages() %>% 
  ee$ImageCollection$toBands()

im_date <- ee$Date$fromYMD(year(start_date), month(start_date), day(start_date))$advance(4, "day")
im_date_end <- im_date$advance(1, "day")
test1 <- maiac$filterDate(im_date, im_date_end)#$reduce(ee$Reducer$median())
test2 <- maiac$filterDate('2006-03-28', '2006-03-29')





















test1 <- ee$List$sequence(0, length_date - 1)$map(ee_utils_pyfunc(function(x){
  im_date <- ee$Date$fromYMD(year(start_date), month(start_date), day(start_date))$advance(x, "day")
  im_date_end <- im_date$advance(1, "day")
  maiac$filterDate(im_date, im_date_end)$reduce(ee$Reducer$median()) %>% 
    ee$Image$unmask(-999999) %>% 
    return
})) %>% ee$ImageCollection$fromImages() %>% 
  ee$ImageCollection$toBands()
ee_print(test1)

# the dataset in gee is granules, lets make a daily image collection to make this easier
test2 <- ee$List$sequence(0, length_date - 1)$map(ee_utils_pyfunc(function(x){
  im_date <- ee$Date$fromYMD(year(start_date), month(start_date), day(start_date))$advance(x, "day")
  im_date_end <- im_date$advance(1, "day")
  im <- maiac$filterDate(im_date, im_date_end)$reduce(ee$Reducer$median())
  e$Algorithms$If(ee$Algorithms$IsEqual(im$bandNames()$size(), ee$Number(0))) {
    im <- im$addBands(ee$Image$constant(-999999)$rename("Optical_Depth_055_median"))
  }
  im <- im$unmask(-999999)
  return(im)
})) %>% ee$ImageCollection$fromImages()

# the dataset in gee is granules, lets make a daily image collection to make this easier
test3 <- ee$List$sequence(0, length_date - 1)$map(ee_utils_pyfunc(function(x){
  im_date <- ee$Date$fromYMD(year(start_date), month(start_date), day(start_date))$advance(x, "day")
  im_date_end <- im_date$advance(1, "day")
  im <- maiac$filterDate(im_date, im_date_end)$reduce(ee$Reducer$median())
  im <- ee$Algorithms$If(
    ee$Algorithms$IsEqual(im$bandNames()$size(), ee$Number(0)),
    im$addBands(ee$Image$constant(-999999)$rename("Optical_Depth_055_median")), 
    im$unmask(-999999)
  )
  return(im)
})) %>% ee$ImageCollection$fromImages()
ee_print(test3)

test4 <- test3 %>% ee$ImageCollection$toBands()
ee_print(test4)
