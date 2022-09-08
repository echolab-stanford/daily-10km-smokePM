# ------------------------------------------------------------------------------
# Written by: Anne Driscoll
# Adapted by: Marissa Childs
# Creates a 10 km grid across the contiguous US.
# ------------------------------------------------------------------------------
proj = "+proj=utm +zone=19 +datum=NAD83 +units=m +no_defs"
res = 10000

counties = tigris::counties(year = 2019) %>% as(Class = "Spatial")
counties = SpatialPolygonsDataFrame(gSimplify(counties, 0.008, topologyPreserve=F), 
                                    counties@data)
counties = counties[!counties$STATEFP %in%
                      c("02", "15", "60", "66", "69", "72", "78"), ]
counties = spTransform(counties, proj)
c = gBuffer(counties, width = res)
grid = CreateGrid(counties, resolution=res, returnclass="sp")
grid = spTransform(grid, proj)

# remove grid cells that don't overlap a state
keep = over(grid, c)
keep = which(!is.na(keep))
data_ll = grid[keep, ]

# make in to an actual grid rather than points
data_ll = spTransform(data_ll, proj)
data_ll = gBuffer(data_ll, byid=T, width=res/2, capStyle="SQUARE") #slow
data_ll = st_as_sf(data_ll)
st_write(obj=data_ll, 
         dsn=paste0(db_path, "Data/boundaries/10km_grid"),
         layer="10km_grid", driver="ESRI Shapefile")

# transform and save the 10km grid because its doesn't load in EE with original crs
grid <- data_ll %>%
  st_transform(st_crs(4326))

st_write(grid, "grid_10km_wgs84", 
         layer="10km_grid_wgs84",
         driver = "ESRI Shapefile")
# upload resulting to earth engine, named "grid_10km/grid_10km_wgs84"
