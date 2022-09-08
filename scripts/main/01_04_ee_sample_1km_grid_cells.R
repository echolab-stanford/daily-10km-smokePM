# ------------------------------------------------------------------------------
# Written by: Marissa Childs
# Samples 1 km grid cells with population weighting.
# ------------------------------------------------------------------------------
ee_Initialize()

population <- ee$ImageCollection("WorldPop/GP/100m/pop") %>% 
  ee$ImageCollection$filterMetadata("country", "equals", "USA") %>% 
  ee$ImageCollection$filterMetadata("year", "equals", 2013) %>% 
  ee$ImageCollection$first() %>% 
  ee$Image$divide(ee$Image$pixelArea()) %>% # convert to population density
  ee$Image$unmask(0)

pop_scale <- population$projection()$nominalScale()$getInfo()

subgrid_1km_tasks <- 1:50 %>% 
  purrr::map(function(subgrid_no){
    
    print(paste("Working on subgrid", subgrid_no))
    
    # identify subgrid to work with
    subgrid <- ee$FeatureCollection(paste0("users/marissac/grid_aod_1km/grid_aod_1km_wgs84_", subgrid_no))
    
    # calculate average for each grid cell
    pop_1km <- population$reduceRegions( 
      collection = subgrid,
      reducer = ee$Reducer$mean(), 
      scale = pop_scale
    )
    
    # make list of properties to export
    export_properties <- list("grid_id", "mean")
    
    # make useful file name with grid number and dates (inclusive)
    output_name <- paste("populationDensity_1km_subgrid",
                         subgrid_no,
                         sep = "_")
    
    # make a task to save the maiac station data to google drive
    grid_task <- ee_table_to_drive(
      collection = pop_1km,
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

output_local <- "./data/populationDensity_1km_subgrid/"

subgrid_1km_tasks %>% map(function(t){
  ee_drive_to_local(t, 
                    paste0(output_local, t$status()$description, ".csv"))
})


# read in the 1km population density information to do population-weighted sample 
pop_1km <- purrr::map_dfr(list.files(output_local, full.names = TRUE), 
                          function(x){
                            read.csv(x)
                          })


grid_1km <- st_read("./data/1km_aod_grid_wgs84")

set.seed(12345)
sample_ids <- sample(pop_1km$grid_id, size  = 5000, prob = pop_1km$mean)

grid_sample <- grid_1km %>% filter(grid_id %in% sample_ids)

# to check the distribution 
# states <- tigris::states() %>%
#   filter(!(STATEFP %in% c("72", "78", "60", "69", "66", "02", "15")))
# plot(st_geometry(states))
# plot(st_geometry(grid_sample), add = TRUE, border = "red")

st_write(grid_sample, "./data/1km_aod_grid_wgs84_training", 
         driver = "ESRI Shapefile")

# manually upload resulting shapefile to earth engine as "grid_aod_1km/grid_aod_1km_wgs84_training")