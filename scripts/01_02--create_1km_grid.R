library(tidyverse)
library(magrittr)
library(rgee)
library(sf)
library(raster)

ee_Initialize()

states <- ee$FeatureCollection("TIGER/2016/States") %>% 
  ee$FeatureCollection$filter(ee$Filter$inList('STATEFP',
                                               as.list(c("02","60","66","15","72","78","69")))$Not())
  
maiac_im <- ee$ImageCollection("MODIS/006/MCD19A2_GRANULES") %>% 
  ee$ImageCollection$select("Optical_Depth_047") %>% 
  ee$ImageCollection$filterBounds(states) %>% 
  ee$ImageCollection$first()

maiac_res <- maiac_im$projection()$nominalScale()$getInfo()
# ee_crs <- maiac_im$projection()$getInfo()$crs %>% 
#   ee_utils_get_crs() %>%
#   st_crs # for some reason, the crs we get from this is not right 
maiac_crs <- st_crs(read_file("https://spatialreference.org/ref/sr-org/6842/proj4/"))

# takes ~1min to get the maiac raster to local
maiac_rast <- ee_as_raster(maiac_im,
                         via = "drive") 
crs(maiac_rast) <- maiac_crs # update the crs, since it doesn't quite copy over well

# load the 10km grid to ensure 1km grid covers it
grid_10km <- st_read("./data/1_grids/10km_grid") %>%
  st_transform(maiac_crs)
grid_union <- st_union(grid_10km) %>% st_sf

# extend 1km further just to make sure we get all cells that would overlap the 10km grid
extended_rast <- extend(maiac_rast[[1]],
                        extent(c(xmin = extent(grid_union)@xmin - 1000,
                                 xmax = extent(grid_union)@xmax + 1000,
                                 ymin = extent(grid_union)@ymin - 1000,
                                 ymax = extent(grid_union)@ymax + 1000)) %>%
                          alignExtent(maiac_rast[[1]], snap = "out"),
                        snap = "out",
                        value = NA)
rm(maiac_rast)

extended_points <- coordinates(extended_rast) %>% 
  as.data.frame %>% 
  st_as_sf(coords = c("x", "y"), remove = FALSE) %>%
  st_set_crs(maiac_crs)
rm(extended_rast)

# buffering of points, this takes ~2 hours
extended_polys <- data.frame(start = c(seq(1, 
                                         nrow(extended_points), 
                                         by = 1e6), 
                                       nrow(extended_points) + 1)) %>% 
  mutate(end = lead(start, n = 1) - 1) %>%
  filter(!is.na(end)) %>% 
  purrr::pmap_dfr(function(start, end){
    extended_points[start:end,] %>% 
      st_buffer(dist = maiac_res/2,
                endCapStyle = "SQUARE") %>% 
      return
  })
rm(extended_points)

# filter to grid cells that overlap the 10km grid
# this takes a little over 8 hours, results in shapefile with ~9.6 M grid cells
grid_1km <- extended_polys[grid_union,]
rm(extended_polys)

# add grid IDs 
grid_1km$grid_id <- 1:nrow(grid_1km)

# save the grid in both sinusoidal and wgs projections
# earth engine had trouble loading the non-wgs projection, this is also slow-ish 
st_write(grid_1km, "./data/1km_aod_grid", driver = "ESRI Shapefile")

st_write(grid_1km %>% st_transform(st_crs(4326)), 
         "./data/1km_aod_grid_wgs84", driver = "ESRI Shapefile")

# upload resulting to earth engine, named "grid_aod_1km/grid_aod_1km_wgs84"

# also make a crosswalk between 1 km and 10km grids 
# lets make a data frame with a row for each 1km grid cell that indicates which 10km grid cell its centroid falls in
# the x,y in grid_1km still are the centroid coordinates so lets use them
grid_1km_cent <- grid_1km %>% 
  st_drop_geometry() %>% 
  st_as_sf(coords = c("x", "y"), remove = FALSE) %>%
  st_set_crs(maiac_crs)

cross_list <- st_intersects(grid_1km_cent, 
                            grid_10km)

cross <- data.frame(grid_id_1km = grid_1km_cent$grid_id, 
                    grid_no_10km = as.numeric(cross_list))

cross$grid_id_10km = grid_10km$ID[cross$grid_no_10km]

saveRDS(cross %>% select(contains("grid_id")), "./data/grid_crosswalk_1km_10km.rds")



