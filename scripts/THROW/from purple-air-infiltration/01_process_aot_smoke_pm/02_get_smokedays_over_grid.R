source("../00_utils.R")
library(tictoc)
library(data.table)
library(raster)

########################################################################################
# Written by: Anne Driscoll
# Gets a count of plumes overhead at the grid level for 2011-2020
########################################################################################

years = 2006:2020

###############################################################
# Read in data
###############################################################

# Original smoke data at: https://www.ospo.noaa.gov/Products/land/hms.html
# here all the individual day files have been processed and combined
smoke = readRDS(paste0(db_path, "/Data/smoke/smoke_plumes_spdf.RDS"))

# get grid over which to query for smoke
poly_grid = readRDS(paste0(git_path, "/data/grid.RDS"))
poly_grid = gBuffer(poly_grid, byid=T, width=5000, capStyle="SQUARE") #slow


###############################################################
# Process smoke
###############################################################

# prepare smoke data 
smoke@data = as.data.frame(smoke@data)
smoke$year = substr(smoke$date, 1, 4)
smoke = smoke[smoke$year < max(as.numeric(years))+1 &
                smoke$year > min(as.numeric(years))-1, ]
smoke$month = as.numeric(substr(smoke$date, 5, 6))
smoke$Density = as.character(smoke$Density)
smoke$Density = gsub(".000", "", smoke$Density)

crs(smoke) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
poly_grid = spTransform(poly_grid, crs(smoke))

#function that for each extract returns number of plumes, used in loop
get_plumes = function(dt) {
  dt = as.data.table(dt)
  dt = dt[, .(light=sum(Density==5), medium=sum(Density==16), 
              dense=sum(Density==27), total=.N), by=date]
  dt = dt[, total := rowSums(.SD), .SDcols=2:4]
  return(dt)
}

get_plumes_pre2011 = function(dt) {
  dt = as.data.table(dt)
  dt = dt[, .(smoke_day=1, total=.N), by=date]
  return(dt)
}

#get the plumes over each grid id for all time
for (i in 1:length(years)) {
  
  print(years[i])
  cur = smoke[smoke$year == years[i], ]
  post = years[i] > 2010
  
  for (j in 1:12) {
    
    cur_month = cur[cur$month == j, ]
    
    tic()
    overlap = over(poly_grid, cur_month, returnList=T) # get the overlaps
    toc()
    
    tic()
    
    if (post) { # get the overlaps depending on year
      overlap = lapply(overlap, get_plumes) # get number of plumes from list 
    } else {  
      overlap = lapply(overlap, get_plumes_pre2011) # get number of plumes from list 
    }
    
    for (k in 1:length(overlap)) { # get the grid ID in there
      temp = overlap[[k]]  
      temp$id = poly_grid$ID[k]
      overlap[[k]] = temp
    }
    overlap = as.data.frame(data.table::rbindlist(overlap)) # make a data frame
    toc()
    
    saveRDS(overlap, paste0(db_path, "Data/smoke/intermediate/smoke_grid_", 
                         years[i], "_", j, ".RDS"))
    print(j)
  }
}


files = list.files(paste0(db_path, "Data/smoke/intermediate"))
combined = as.list(rep(NA, length(files)))

for (i in 1:length(files)) {
  combined[[i]] = readRDS(paste0(db_path, "Data/smoke/intermediate/", files[i]))
}
combined = rbindlist(combined, fill=T)
saveRDS(combined, paste0(db_path, "Data/smoke/sparse_smoke_grid.RDS"))
