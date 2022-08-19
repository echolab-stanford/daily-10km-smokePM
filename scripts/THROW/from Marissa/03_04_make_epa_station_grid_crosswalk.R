source("./scripts/0_config.R")
library(sf)

pm_data <- readRDS(paste0(data_path, "/epa_station_level_pm25_data.rds"))
epa_ll <- pm_data %>% 
  select(id, lat, lon) %>% 
  unique() %>% 
  st_as_sf(coords = c("lon", "lat"), 
           crs = 4326, remove = FALSE)

# load the 10km grid 
grid <- read_sf(paste0(data_path, "/1_grids/grid_10km_wgs84"))

epa_to_grid <- st_intersects(epa_ll, grid)

epa_ll$grid_10km <- grid$ID[as.numeric(epa_to_grid)]

# save shapefile of locations
write_sf(epa_ll, paste0(data_path, "/epa_station_locations"), 
         "epa_station_locations",
         driver = "ESRI Shapefile")
