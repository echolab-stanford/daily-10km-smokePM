source("../00_init.R")
library(exactextractr)
library(Hmisc)

setwd(db_path)

########################################################################################
# Written by: Anne Driscoll
# Last edited by: Anne Driscoll 
# Get elevation at the grid cell level
########################################################################################

# get grid over which to query for elev
poly_grid = readRDS(paste0(git_path, "/data/grid.RDS"))
poly_grid = gBuffer(poly_grid, byid=T, width=5000, capStyle="SQUARE") #slow

# read in elevation
elev1 = raster("Data/elevation/gmted2010/10N090W_20101117_gmted_mea300.tif")
elev2 = raster("Data/elevation/gmted2010/10N120W_20101117_gmted_mea300.tif")
elev3 = raster("Data/elevation/gmted2010/30N090W_20101117_gmted_mea300.tif")
elev4 = raster("Data/elevation/gmted2010/30N120W_20101117_gmted_mea300.tif")
elev5 = raster("Data/elevation/gmted2010/30N150W_20101117_gmted_mea300.tif")
elev = do.call(merge, list(elev1, elev2, elev3, elev4, elev5))

# extract elevation
poly_grid = spTransform(poly_grid, crs(elev))
elev_df = exact_extract(elev, poly_grid, 
                        function(vals, weights){wtd.mean(vals, weights)})

output = data.frame(id = poly_grid$ID, elev = elev_df)
saveRDS(output, paste0(git_path, "/data/grid_elevation.RDS"))
