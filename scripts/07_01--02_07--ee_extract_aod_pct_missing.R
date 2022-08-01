library(tidyverse)
library(magrittr)
library(rgee)
library(lubridate)
library(retry)

ee_Initialize()

# earth engine location of the subsets of the 10km grid
asset_folder <- "users/marissac/grid_10km/"

# google drive export location 
output_drive <- "maiac_AODmissings"

# local folder to save output to
output_local <- "./data/2_from_EE/maiac_AODmissings/"

# set timezone, currently using GMT+6 to be consistent with ERA5 extractions
# matches mountain time in summer and central time in winter
timezone <- "Etc/GMT+6"

# Set up time range (end_date is exclusive) with subgrids to run over 
date_grid <- data.frame(start_date = c("2005-12-20", # start from the last 10 days of 2005 for lagging purposes
                                       paste0(2006:2021, "-01-01"))) %>% 
  mutate(end_date = lead(start_date, 1)) %>% 
  drop_na() %>% 
  expand_grid(subgrid_no = 1:10) 

# Get MAIAC ----
maiac <- ee$ImageCollection("MODIS/006/MCD19A2_GRANULES") %>% 
  ee$ImageCollection$select("Optical_Depth_047") 

maiac_proj <- maiac$first()$projection()

pixel_res <- maiac_proj$nominalScale()$getInfo()

# for each date and grid combination, calculate average missingness for the grid cells and export
subgrid_tasks <- date_grid %>%
  purrr::pmap(function(start_date, end_date, subgrid_no){
    
    print(paste("Working on start date", start_date, "for subgrid", subgrid_no))
    
    # calculate how many days between start and end 
    length_date <- difftime(end_date, start_date, units = "days") %>% as.integer()
    
    # identify subgrid to work with
    subgrid <- ee$FeatureCollection(paste0(asset_folder, "grid_10km_wgs84_", subgrid_no)) 
    
    # for each day, count available images, then make binary with 1's indicating no obs/missings ----
    maiac_daily_missing <- ee$List$sequence(0, length_date - 1)$map(ee_utils_pyfunc(function(x){
      im_date <- ee$Date$fromYMD(year(start_date), month(start_date), day(start_date), 
                                 timezone)$advance(x, "day") 
      im_date_end <- im_date$advance(1, "day")
      im <- maiac$filterDate(im_date, im_date_end)$count()$unmask(0) %>% 
        ee$Image$Not() %>%
        ee$Image$set("start_date", im_date$format("yMMdd")) # set date of the image
      return(im)
    })) %>%
      ee$ImageCollection$fromImages() 
    
    # for each day, calculate average for each grid cell
    subgrid_missings <- maiac_daily_missing$map(function(daily_im){
      daily_im$reduceRegions(collection = subgrid, 
                             reducer = ee$Reducer$mean(), 
                             crs = maiac_proj,
                             scale = pixel_res) %>% 
        ee$FeatureCollection$map(function(f){
          f$set("start_date", daily_im$get("start_date"))  %>%  # set the date of the feature
            return
        })
    }) %>% ee$FeatureCollection() %>% 
      ee$FeatureCollection$flatten()
    
    # make list of properties to export
    export_properties <- list("ID", "mean", "start_date")
    
    # make useful file name with grid number and dates (inclusive)
    output_name <- paste("aod_pctMissing_10km_subgrid",
                         subgrid_no,
                         as.Date(start_date) %>% format("%Y%m%d"),
                         (as.Date(end_date) + as.difftime(-1, units = "days")) %>% format("%Y%m%d"),
                         sep = "_")
    
    # make a task to save the maiac station data to google drive
    grid_task <- ee_table_to_drive(
      collection = subgrid_missings,
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
subgrid_tasks %>% map_chr(function(x) x$status()$state)

# once tasks are "COMPLETED", download them to local folder
# this took ~12 hours
retry::wait_until(
  expr = all(map_chr(subgrid_tasks, function(x) x$status()$state) == "COMPLETED"), 
  interval = 60
  )

subgrid_tasks %>% map(function(t){
  ee_drive_to_local(t, 
                    paste0(output_local, t$status()$description, ".csv"))
})
