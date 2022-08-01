source("../00_utils.R")
library(exactextractr)

setwd(db_path)

########################################################################################
# Written by: Anne Driscoll
# Last edited by: Anne Driscoll 
# Get population at our grid level
########################################################################################

# read in data needed
pop = raster("Data/population/gpw_v4_population_count_rev11_2010_2pt5_min.tif")

# define grid as shapes rather than points
grid = readRDS(paste0(git_path, "/data/grid.RDS"))
grid = gBuffer(grid, byid=T, width=5000, capStyle="SQUARE") #slow
grid = spTransform(grid, crs(pop))

# extract
pop = crop(pop, extent(grid))
grid_pop = exact_extract(pop, grid, 
                          fun=function(val, wts){sum(val, na.rm=T)})
data_out = data.frame(id=grid$ID, pop=grid_pop)
saveRDS(data_out, paste0("Data/10km_grid_data/grid_population.RDS"))
