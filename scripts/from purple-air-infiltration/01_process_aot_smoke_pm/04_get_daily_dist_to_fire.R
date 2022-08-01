source("../00_utils.R")
library(data.table)
library(FNN)

setwd(db_path)

########################################################################################
# Written by: Anne Driscoll
# Last edited by: Anne Driscoll 
# Get distance to nearest fire point for all grid-days
########################################################################################

grid = readRDS(paste0(git_path, "/data/grid.RDS"))
poly_grid = gBuffer(grid, byid=T, width=5000, capStyle="SQUARE") #slow


# loop through years
for (i in 1:length(years)) {
  
  # get year
  year = years[i]
  
  # get list of days in that year
  days = seq(as.Date(paste0(year, "-01-01")), 
             as.Date(paste0(year, "-12-31")), "days")
  days = gsub("-", "", as.character(days))
  out = as.list(rep(NA, length(days)))
  
  # loop through days in the year
  for (j in 1:length(days)) {
    
    #figure out file to open
    file = paste0("hms_fire", days[j], ".shp")
    
    if (file %in% list.files("Data/fire")) {
      fires = readOGR(paste0("Data/fire/hms_fire", days[j], ".shp"), 
                      paste0("hms_fire", days[j]), verbose=F)
      crs(fires) = "+proj=longlat +datum=WGS84 +no_defs"
      fires = fires[fires$Lon > -144 & fires$Lon < -32 & 
                      fires$Lat > -7 & fires$Lat < 70, ] #limit to north america
      fires = spTransform(fires, crs(grid))
      
      dist = get.knnx(coordinates(fires), coordinates(grid), k=1)
      
      df = data.frame(id=grid$ID, date=days[j], km_dist=dist$nn.dist/1000)
    } else {
      df = data.frame(id=grid$ID, date=days[j], km_dist=NA)
    }
    
    out[[j]] = df
    if (j%%50==0) {print(j)}
  }
  
  out = rbindlist(out)
  saveRDS(out, paste0(git_path, "/data/grid_dist_fire/dist_to_fire_", year, ".RDS"))

}