# ------------------------------------------------------------------------------
# Written by: Marissa Childs
# Splits 10 km and 1 km grids into chunks.
# ------------------------------------------------------------------------------
ee_Initialize(email = gee_email)

# this folder should be publicly readable by anyone with an earth engine account
# alternatively, you can upload your own 10km grid and replace with your own asset folder
asset_folder_10km <- "users/marissac/grid_10km/" 
asset_folder_1km <- "users/marissac/grid_aod_1km/" 

grid_10km <- ee$FeatureCollection(paste0(asset_folder_10km, "grid_10km_wgs84"))
grid_1km <- ee$FeatureCollection(paste0(asset_folder_1km, "grid_aod_1km_wgs84"))

split_grids_10km <- 
  cut(1:grid_10km$size()$getInfo(), breaks = 10) %>% 
  as.numeric() %>% 
  table %>% 
  as.data.frame() %>% 
  set_colnames(c("group", "size")) %>% 
  mutate(offset = cumsum(lag(size, default = 0))) %>% 
  purrr::pmap(function(group, size, offset){
    subgrid <- grid_10km$toList(count = size, offset = offset) %>% 
      ee$FeatureCollection()
    subgrid_task <- ee_table_to_asset(collection = subgrid, 
                                      description = paste0("grid_10km_wgs84_", group),
                                      assetId = paste0(asset_folder_10km, "grid_10km_wgs84_", group))
    subgrid_task$start()
    return(subgrid_task)
  })


split_grids_1km <- 
  cut(1:grid_1km$size()$getInfo(), breaks = 50) %>% # should make subgrids about 480K obs each
  as.numeric() %>% 
  table %>% 
  as.data.frame() %>% 
  set_colnames(c("group", "size")) %>% 
  mutate(offset = cumsum(lag(size, default = 0))) %>% 
  purrr::pmap(function(group, size, offset){
    subgrid <- grid_1km$toList(count = size, offset = offset) %>% 
      ee$FeatureCollection()
    subgrid_task <- ee_table_to_asset(collection = subgrid, 
                                      description = paste0("grid_aod_1km_wgs84_", group),
                                      assetId = paste0(asset_folder_1km, "grid_aod_1km_wgs84_", group))
    subgrid_task$start()
    return(subgrid_task)
  })

# check all the tasks have a status of "COMPLETED" before proceeding
split_grids_10km %>% purrr::map_chr(function(t) t$status()$state)

split_grids_1km %>% purrr::map_chr(function(t) t$status()$state)
