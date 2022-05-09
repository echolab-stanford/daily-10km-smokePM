library(tidyverse)
library(magrittr)
library(rgee)
library(sf)

ee_Initialize()

population <- ee$ImageCollection("WorldPop/GP/100m/pop") %>% 
  ee$ImageCollection$filterMetadata("country", "equals", "USA") %>% 
  ee$ImageCollection$filterMetadata("year", "equals", 2013) %>% 
  ee$ImageCollection$first() %>% 
  ee$Image$divide(ee$Image$pixelArea()) %>% # convert to population density
  ee$Image$unmask(0)
pop_scale <- population$projection()$nominalScale()$getInfo()

subgrid_10km_tasks <- 1:10 %>% 
  purrr::map(function(subgrid_no){
    
    print(paste("Working on subgrid", subgrid_no))
    
    # identify subgrid to work with
    subgrid <- ee$FeatureCollection(paste0("users/marissac/grid_10km/grid_10km_wgs84_", subgrid_no))
    
    # calculate population density, then average for each grid cell
    pop_10km <- population %>% #$divide(ee$Image$pixelArea()) %>% 
      ee$Image$reduceRegions( 
      collection = subgrid,
      reducer = ee$Reducer$mean(), 
      scale = pop_scale
    )
    
    # make list of properties to export
    export_properties <- list("ID", "mean")
    
    # make useful file name with grid number and dates (inclusive)
    output_name <- paste("populationDensity_10km_subgrid",
                         subgrid_no,
                         sep = "_")
    
    # make a task to save the maiac station data to google drive
    grid_task <- ee_table_to_drive(
      collection = pop_10km,
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
map_chr(subgrid_10km_tasks, function(x) x$status()$state)

output_local <- "./populationDensity_10km_subgrid/"

subgrid_10km_tasks %>% map(function(t){
  ee_drive_to_local(t, 
                    paste0(output_local, t$status()$description, ".csv"))
})
