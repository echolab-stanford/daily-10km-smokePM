library(tidyverse)
library(magrittr)
library(rgee)
library(lubridate)

ee_Initialize()

# set a null value 
null_value <- -999999

# earth engine location of the subsets of the 10km grid
grid_train <- ee$FeatureCollection("users/marissac/grid_aod_1km/grid_aod_1km_wgs84_training")

# google drive export location 
output_drive <- "maiac_AOD"

# local folder to save output to
output_local <- "./data/2_from_EE/maiac_AOD_training/"

# set timezone, currently using GMT+6 to be consistent with ERA5 extractions
# matches mountain time in summer and central time in winter
timezone <- "Etc/GMT+6"

# Set up time range (end_date is exclusive) with subgrids to run over 
date_grid <- data.frame(start_date = c("2005-12-20", # start from the last 10 days of 2005 for lagging purposes
                                       paste0(2006:2021, "-01-01"))) %>% 
  mutate(end_date = lead(start_date, 1)) %>% 
  drop_na() 

# Get MAIAC ----
maiac <- ee$ImageCollection("MODIS/006/MCD19A2_GRANULES") %>% 
  ee$ImageCollection$select("Optical_Depth_047") 

maiac_proj <- maiac$first()$projection()

pixel_res <- maiac_proj$nominalScale()$getInfo()

# for each date and grid combination, calculate average missingness for the grid cells and export
grid_tasks <- date_grid %>%
  purrr::pmap(function(start_date, end_date){
    
    print(paste("Working on start date", start_date))
    
    # calculate how many days between start and end 
    length_date <- difftime(end_date, start_date, units = "days") %>% as.integer()
    
    # for each day, count available images, then make binary with 1's indicating no obs/missings ----
    maiac_daily <- ee$List$sequence(0, length_date - 1)$map(ee_utils_pyfunc(function(x){
      im_date <- ee$Date$fromYMD(year(start_date), month(start_date), day(start_date), 
                                 timezone)$advance(x, "day") 
      im_date_end <- im_date$advance(1, "day")
      im <- maiac$filterDate(im_date, im_date_end)$reduce(ee$Reducer$median())
      im <- ee$Algorithms$If(
        ee$Algorithms$IsEqual(im$bandNames()$size(), ee$Number(0)),
        im$addBands(ee$Image$constant(null_value)$rename("Optical_Depth_055_median")),
        im$unmask(null_value)
      ) %>% ee$Image$set("start_date", im_date$format("yMMdd"))
      return(im)
      # im$unmask(null_value)$set("start_date", im_date$format("yMMdd")) %>% return
    })) %>%
      ee$ImageCollection$fromImages() 
  
    # for each day, calculate average for each grid cell
    grid_aod <- maiac_daily$map(function(daily_im){
      daily_im$reduceRegions(collection = grid_train, 
                             reducer = ee$Reducer$median(), 
                             crs = maiac_proj,
                             scale = pixel_res) %>% 
        ee$FeatureCollection$map(function(f){
          f$set("start_date", daily_im$get("start_date"))  %>%  # set the date of the feature
            return
        })
    }) %>% ee$FeatureCollection() %>% 
      ee$FeatureCollection$flatten()
    
    # make list of properties to export
    export_properties <- list("grid_id", "median", "start_date")
    
    # make useful file name with grid number and dates (inclusive)
    output_name <- paste("aod_1km_grid_train",
                         as.Date(start_date) %>% format("%Y%m%d"),
                         (as.Date(end_date) + as.difftime(-1, units = "days")) %>% format("%Y%m%d"),
                         sep = "_")
    
    # make a task to save the maiac station data to google drive
    grid_task <- ee_table_to_drive(
      collection = grid_aod,
      folder = output_drive,
      description = output_name,
      fileFormat = "CSV",
      selectors = export_properties
    )
    
    # start the task
    print(paste("saving file as", output_name))
    grid_task$start()
    
    # save the task information for later retrieval of output
    return(grid_task) 
    
  })

# check the status of the tasks
grid_tasks %>% map_chr(function(x) x$status()$state)

# once tasks are "COMPLETED", download them to local folder
grid_tasks %>% map(function(t){
  ee_drive_to_local(t, 
                    paste0(output_local, t$status()$description, ".csv"))
})
