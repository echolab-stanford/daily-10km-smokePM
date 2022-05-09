library(tidyverse)
library(magrittr)
library(rgee)

# GEE extractions ----
ee_Initialize()

nlcd <- ee$ImageCollection("USGS/NLCD_RELEASES/2016_REL") %>% 
  ee$ImageCollection$filterDate("2013-01-01", "2014-01-01") %>% # use the 2013 land use classification
  ee$ImageCollection$first() %>% 
  ee$Image$divide(10) %>% # the tens digit has the class we want to use
  ee$Image$floor()
nlcd_crs <- nlcd$projection()
nlcd_res <- nlcd_crs$nominalScale()$getInfo()

# extraction on 10km grid ----
grid_10km <- ee$FeatureCollection("users/marissac/grid_10km/grid_10km_wgs84") 

# extract pct of each land cover in grid cell 
nlcd_10km_areas <- ee$Image$pixelArea()$addBands(nlcd)$reduceRegions( #add a band for pixel area
  collection = grid_10km, 
  reducer = ee$Reducer$sum()$group(groupField = 1, groupName = "landcover"), # sum over pixel areas, grouping by landcover
  scale = nlcd_res,
  crs = nlcd_crs
) %>% 
  ee$FeatureCollection$map(function(feat){
    feat$setGeometry(NULL) %>% return
  })

# in real version, would then drop geometry info from grid_areas to make file size smaller and export to google drive, then download to local
nlcd_10km_task <- ee_table_to_drive(
  collection = nlcd_10km_areas,
  description = "NLCD_areas_10km_grid",
  fileFormat = "CSV"
)

# start the task
nlcd_10km_task$start()

# check for task completion
nlcd_10km_task$status()$state

ee_drive_to_local(nlcd_10km_task, 
                  paste0("./data/2_from_EE/", nlcd_10km_task$status()$description, ".csv"))

# extraction on 1km grid ----
subgrid_1km_tasks <- 1:50 %>% 
  purrr::map(function(subgrid_no){
    
    print(paste("Working on subgrid", subgrid_no))
    
    # identify subgrid to work with
    subgrid <- ee$FeatureCollection(paste0("users/marissac/grid_aod_1km/grid_aod_1km_wgs84_", subgrid_no))
    
    # for each day, calculate average for each grid cell
    nlcd_1km_areas <- ee$Image$pixelArea()$addBands(nlcd)$reduceRegions( #add a band for pixel area
      collection = subgrid, 
      reducer = ee$Reducer$sum()$group(groupField = 1, groupName = "landcover"), # sum over pixel areas, grouping by landcover
      scale = nlcd_res,
      crs = nlcd_crs
    )
    
    # make list of properties to export
    export_properties <- list("grid_id", "groups")
    
    # make useful file name with grid number and dates (inclusive)
    output_name <- paste("NLCD_areas_1km_subgrid",
                         subgrid_no,
                         sep = "_")
    
    # make a task to save the maiac station data to google drive
    grid_task <- ee_table_to_drive(
      collection = nlcd_1km_areas,
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

output_local <- "./data/2_from_EE/NLCD_1km_subgrid/"

subgrid_1km_tasks %>% map(function(t){
  ee_drive_to_local(t, 
                    paste0(output_local, t$status()$description, ".csv"))
})

