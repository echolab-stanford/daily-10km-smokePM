library(tidyverse)
library(magrittr)
library(rgee)

ee_Initialize()

elev <- ee$Image("USGS/NED")

# extraction for 10km grid ----
grid_10km <- ee$FeatureCollection("users/marissac/grid_10km/grid_10km_wgs84")

comb_reducer <- ee$Reducer$combine(reducer1 = ee$Reducer$mean(), 
                                   reducer2 = ee$Reducer$stdDev(), 
                                   outputPrefix = "stdDev_", 
                                   sharedInputs = TRUE)
# avg elevation 
grid_10km_elev <- elev$reduceRegions(collection = grid_10km, 
                                     reducer = comb_reducer, 
                                     scale = elev$projection()$nominalScale())

elev_10km_task <- ee_table_to_drive(
  collection = grid_10km_elev,
  description = "elevation_avg_sd_10km_grid",
  fileFormat = "CSV"
)

elev_10km_task$start()

elev_10km_task$status()$state

ee_drive_to_local(elev_10km_task, 
                  paste0("./data/", elev_10km_task$status()$description, ".csv")) 


# extraction for 1km grid ----
subgrid_1km_tasks <- 1:50 %>% 
  purrr::map(function(subgrid_no){
    
    print(paste("Working on subgrid", subgrid_no))
    
    # identify subgrid to work with
    subgrid <- ee$FeatureCollection(paste0("users/marissac/grid_aod_1km/grid_aod_1km_wgs84_", subgrid_no))
    
    # calculate average for each grid cell
    grid_1km_elev <- elev$reduceRegions(collection = subgrid,
                                        reducer = comb_reducer, 
                                        scale = elev$projection()$nominalScale())
    
    # make list of properties to export
    export_properties <- list("grid_id", "mean", "stdDev_stdDev")
    
    # make useful file name with grid number and dates (inclusive)
    output_name <- paste("elevation_avg_sd_1km_subgrid",
                         subgrid_no,
                         sep = "_")
    
    # make a task to save the maiac station data to google drive
    grid_task <- ee_table_to_drive(
      collection = grid_1km_elev,
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


# check for task completion
map_chr(subgrid_1km_tasks, function(x) x$status()$state)

output_local <- "./data/elevation_1km_subgrid/"

subgrid_1km_tasks %>% map(function(t){
  ee_drive_to_local(t, 
                    paste0(output_local, t$status()$description, ".csv"))
})
