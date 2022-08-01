try(log_file <- file(paste0("~/Desktop/get_fire_clusters_", format(Sys.time(), "%Y-%m-%d-%H-%M-%OS"), ".log"), open = "at"))
sink(log_file, type="output", append = TRUE, split = TRUE)
sink(log_file, type="message", append = TRUE)

library(readr)
library(raster)
library(dplyr)
library(sf)
library(sp)
library(rgeos)
library(data.table)
library(lubridate)

setwd("~/BurkeLab Dropbox/Data")

fire = read_rds("fire/hms_fires.RDS")
crs_use = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs_m = "+proj=utm +zone=19 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

# from https://www.ospo.noaa.gov/Products/land/hms.html#about
north = 72 + 20
south = 14.6 - 20
east = -50 + 40
west = -170 - 40

# get data for each grid cell, for each year. 
years = 2006:2020
width_km = 2.9
width = width_km*1000

# loop through years
# Takes ~10-20 minutes per year on average
for (i in 1:length(years)) {
  
  # get the fires that happened during the year of interest
  y = years[i]
  print(y)
  year_fire = grepl(paste0("^", y), names(fire))
  first = max(c(1, Position(function(x){x==T}, year_fire)-3))
  last = max(c(length(year_fire) - Position(function(x){x==T}, rev(year_fire)) + 1))
  year_fire = fire[first:last]
  year_fire_sp = as.list(rep(NA, length(year_fire)))
  start_loop = ifelse(first == 1, 1, 4)
  j = 1
  
  # loop through days in the year
  prog = txtProgressBar(min=0, max=length(year_fire), initial=0, char="-", style=3)
  for (k in start_loop:length(year_fire)) {
    
    print(k)
    
    #sf to sp
    date = names(year_fire)[[k]]
    
    # using fire data for the 3 days previous as well to capture long burning fires. 
    # figure out which days to combine to get the correct set.
    if (k>=4) {
      f = year_fire[(k-3):k]
    } else if (k == 3) {
      f = year_fire[(k-2):k]
    } else if (k == 2) {
      f = year_fire[(k-1):k]
    } else {
      f = year_fire[k]
    }
    
    f_nrow = sapply(f, nrow)
    if (f_nrow[length(f_nrow)] == 0) next # 2014-07-05 should be "not online"
    
    f = f[which(f_nrow > 0)]
    # if (length(f) == 0) next
    f = lapply(f, "[", c("date", "geometry"))
    f = bind_rows(f)
    
    # k-3 gets dates before 3 previous days when at least one is not online
    f_interval = (ymd(date) - days(3)) %--% ymd(date)
    f = f %>% filter(ymd(date) %within% f_interval)
    
    # get rid of points with lonlat that are beyond extent or empty
    st_crs(f) = crs_use
    f = st_crop(f, xmin = west, xmax = east, ymin = south, ymax = north)
    if (nrow(f[which(f$date == date), ])==0) next
    
    # convert f to an SP object
    # f = st_transform(f, crs_m)
    f = tryCatch({
      as_Spatial(f)
    }, error = function(e) {
      f = f[!is.na(st_is_valid(f)),] # & !st_is_empty(f),]
      f = st_cast(f, "POINT")
      as_Spatial(f)
    })
    if (nrow(f[which(f$date == date),]) == 0) next
    f = spTransform(f, crs_m)
    
    #buffer by 'width' and merge adjacent pixels
    f_buf = gBuffer(f, byid=T,  width=width, capStyle="SQUARE", quadsegs=1)
    f_buf = st_cast(st_union(st_as_sf(f_buf)), "POLYGON")
    f_buf = SpatialPolygonsDataFrame(as_Spatial(f_buf), data.frame(id=1:length(f_buf)), match.ID=F)
    
    #save metadata
    f_buf$area = gArea(f_buf, byid=T)/1000/1000
    f_buf$date = date
    num_points = over(f_buf, f, returnList = T)
    num_points = sapply(num_points, nrow)
    f_buf$num_points = num_points
    
    # filter to clusters with at least one fire point from that day
    f_buf = st_as_sf(f_buf)
    f = st_as_sf(f)
    f = f[which(f$date == date), "geometry"]
    f_buf_filtered = st_filter(f_buf, f)
    # if (nrow(f_buf_filtered) == 0) next
    f = as_Spatial(f_buf_filtered)
    
    # transform CRS back
    f = spTransform(f, crs_use)
    
    #create new ids for clustered fires so that they can all be merged at the end
    f$id = paste0(y, "-", j:(nrow(f)+j-1))
    row.names(f) = as.character(f$id)
    j = length(f$id)+j
    year_fire_sp[[k]] = f
    
    if (k %% 15 == 0) {setTxtProgressBar(prog, k)}
  }
  
  w = sapply(year_fire_sp, function(x){"SpatialPolygonsDataFrame" %in%  class(x)})
  year_fire = year_fire_sp[w] #remove the ones that were empty or broken
  year_fire_df = rbindlist(lapply(year_fire, function(x){x@data}), fill=T)
  year_fire = unlist(lapply(year_fire, function(x){x@polygons}))
  
  row.names(year_fire_df) = year_fire_df$id
  year_fire = SpatialPolygonsDataFrame(SpatialPolygons(year_fire), year_fire_df)
  crs(year_fire) = crs_use
  saveRDS(year_fire, paste0("fire/clusters_", y, ".RDS"))
  
  print(y)
}

sink(type = "output")
sink(type = "message")
close(log_file)
