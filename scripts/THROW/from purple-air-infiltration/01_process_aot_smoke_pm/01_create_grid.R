source("../00_utils.R")
library(SpatialPosition)

setwd(db_path)

########################################################################################
# Written by: Anne Driscoll
# Last edited by: Anne Driscoll
# Creates a 4km grid across contiguous US
########################################################################################

proj = "+proj=utm +zone=19 +datum=NAD83 +units=m +no_defs"
county_proj = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
res = 10000

# create grid
counties = readRDS("Data/boundaries/all_national_counties.rds")
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
saveRDS(data_ll, paste0(git_path, "/data/grid.RDS"))

# make in to an actual grid rather than points
data_ll = spTransform(data_ll, proj)
data_ll = gBuffer(data_ll, byid=T, width=res/2, capStyle="SQUARE") #slow
data_ll = st_as_sf(data_ll)
st_write(obj=data_ll, dsn=paste0(db_path, "Data/boundaries/10km_grid"),
         layer="10km_grid", driver="ESRI Shapefile")
