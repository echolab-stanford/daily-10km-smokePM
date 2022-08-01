################################################################################
################################################################################
################################################################################
# Original script written by Anne
# As used in "Exposures and behavioral responses to wildfire smoke"
################################################################################
################################################################################
################################################################################

################################################################################
# load all smoke plume shapefiles into list named by date
################################################################################

setwd("../../Data/smoke")
files <- list.files()
files <- files[grep(x = files, pattern = ".shp")]

# create smoke list to populate
smoke <- list() 
length(smoke) <- length(files)
names(smoke)<-substr(files, 10,17)

# loop through files and save
for (i in 1:length(files)) {
  try(smoke[[i]] <- st_read(files[i]))
  try(smoke[[i]]$date <- as.numeric(substr(files[i],10,17)))
}

saveRDS(smoke, "smoke_plumes.rds")


################################################################################
# Turn the list of plumes into a massive Spatial Polygons Data Frame
################################################################################

# read in data to start allocating plumes to fires
not_NA = sapply(smoke, function(x){nrow(x) > 0})
smoke = smoke[not_NA]

# runs slowly (1.5 hrs)
j = 1
prog = txtProgressBar(min=0, max=length(smoke), initial=0, char="-", style=3)
for (i in 1:length(smoke)) {
  
  # get the date and relevant smoke
  date_str = names(smoke)[[i]]
  date = as.Date(date_str, format="%Y%m%d")
  x = smoke[[i]]
  
  if(nrow(x) == 0) {next}
  
  # convert smoke data to SP
  x = tryCatch({
    as_Spatial(x)
  }, error = function(e) {
    x = x[!is.na(st_is_valid(x)),]
    x = st_cast(x, "POLYGON")
    as_Spatial(x)
  })
  
  #fix ids so that the smoke files can all be merged
  x$ID = j:(nrow(x)+j-1)
  row.names(x) = as.character(j:(length(x$ID)+j-1))
  j = length(x$ID)+j
  smoke[[i]] = x
  
  #print progress
  if (i %% 100 == 0) {setTxtProgressBar(prog, i)}
}


#rbind all the days  of smoke files and merge data back to the polygons
smoke_df = rbindlist(lapply(smoke, function(x){x@data}), fill=T)
smoke = unlist(lapply(smoke, function(x){x@polygons}))
smoke = SpatialPolygonsDataFrame(SpatialPolygons(smoke), smoke_df)

#save the parsed smoke data
crs(smoke) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "
saveRDS(smoke, "smoke_plumes_spdf.RDS")


################################################################################
# load all smoke plume shapefiles into list named by date
################################################################################

setwd("../fire")
files <- list.files()
files <- files[grepl(".shp", files) & grepl("hms_fire", files)]

# create fire list to populate
fire <- list() 
length(fire) <- length(files)
names(fire) <- substr(files, 9, 16)

# loop through files and save
for (i in 1:length(files)) {
  try(fire[[i]] <- st_read(files[i]))
  try(fire[[i]]$date <- as.numeric(substr(files[i],9,16)))
}

saveRDS(fire, "hms_fires.RDS")











################################################################################
################################################################################
################################################################################
# Modified script written by Jessica
# As used in "A tractable model of particulates from wildfire smoke"
################################################################################
################################################################################
################################################################################

library(sp)
library(sf) # v1.0-0 and later don't work! v0.9-8 works; before probably works too
library(lubridate)
library(data.table)
library(raster)
library(dplyr)

################################################################################
################################################################################
# Intermediate version using sp
################################################################################
################################################################################

################################################################################
# load all smoke plume shapefiles into list named by date
################################################################################

setwd("../../Data/smoke")
files <- list.files()
files <- files[grep(x = files, pattern = ".shp")]

# create smoke list to populate
smoke <- list()
length(smoke) <- length(files)
names(smoke)<-substr(files, 10,17)

# loop through files and save
for (i in 1:length(files)) {
  try(smoke[[i]] <- st_read(files[i]))
  try(smoke[[i]]$date <- as.numeric(substr(files[i],10,17)))
}

saveRDS(smoke, "smoke_plumes.rds")


################################################################################
# Turn the list of plumes into a massive Spatial Polygons Data Frame
################################################################################

# read in data to start allocating plumes to fires
years = 2006:2020
all_dates = seq.Date(ymd(paste0(min(years), "-01-01")),
                     ymd(paste0(max(years), "-12-31")),
                     by = "day")
all_dates = gsub("-", "", all_dates)
dates_downloaded = names(smoke)
dates_not_online = setdiff(all_dates, dates_downloaded)
saveRDS(dates_not_online, "smoke_dates_not_online.rds")

not_empty = sapply(smoke, function(x){nrow(x) > 0})
dates_empty_data = dates_downloaded[which(!not_empty)]
saveRDS(dates_empty_data, "smoke_dates_empty_data.rds")

smoke = smoke[not_empty]
smoke_20150125 = smoke[["20150125"]]
smoke_20150125$ID = 1:nrow(smoke_20150125) - 1
smoke_20150125[c("Start", "End", "Density")] = NA
smoke_20150125 = smoke_20150125[c("ID", "Start", "End", "Density", "geometry", "date")]
smoke[["20150125"]] = smoke_20150125

# runs slowly (1.5 hrs)
dates_invalid_geometry = c()
dates_invalid_geometry_count = c()
j = 1
prog = txtProgressBar(min=0, max=length(smoke), initial=0, char="-", style=3)
for (i in 1:length(smoke)) {
  
  # get the date and relevant smoke
  date_str = names(smoke)[[i]]
  date = as.Date(date_str, format="%Y%m%d")
  x = smoke[[i]]
  
  nr = nrow(x)
  if(nr == 0) {next}
  
  # convert smoke data to SP
  x = tryCatch({
    as_Spatial(x)
  }, error = function(e) {
    x = x[!is.na(st_is_valid(x)),]
    x = st_cast(x, "POLYGON")
    as_Spatial(x)
  }, error = function(e) {
    x = x[!is.na(st_is_valid(x)),]
    x = st_cast(x, "POLYGON")
    if (nrow(x) == 0) NA
  })
  
  if (is.na(x)) {
    dates_invalid_geometry = c(dates_invalid_geometry, date_str)
    dates_invalid_geometry_count = c(dates_invalid_geometry_count, nr)
    next
  }
  
  #fix ids so that the smoke files can all be merged
  x$ID = j:(nrow(x)+j-1)
  row.names(x) = as.character(j:(length(x$ID)+j-1))
  j = length(x$ID)+j
  smoke[[i]] = x
  
  #print progress
  if (i %% 100 == 0) {setTxtProgressBar(prog, i)}
}
saveRDS(dates_invalid_geometry, "smoke_dates_invalid_geometry.rds")

saveRDS(data.frame(date = dates_invalid_geometry,
                   num_plumes = dates_invalid_geometry_count),
        "smoke_dates_invalid_geometry_count.rds")

#rbind all the days  of smoke files and merge data back to the polygons
smoke = smoke[which(!(names(smoke) %in% dates_invalid_geometry))]
smoke_df = rbindlist(lapply(smoke, function(x){x@data}), fill=T)
smoke = unlist(lapply(smoke, function(x){x@polygons}))
smoke = SpatialPolygonsDataFrame(SpatialPolygons(smoke), smoke_df)

#save the parsed smoke data
crs(smoke) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "
saveRDS(smoke, "smoke_plumes_spdf.RDS")



################################################################################
################################################################################
# Final version using sf
################################################################################
################################################################################

################################################################################
# load all smoke plume shapefiles into list named by date
################################################################################

setwd("../../Data/smoke")
files <- list.files(pattern = "^hms_smoke")
files = grep("\\.shp$", files, value = T)
dates = gsub("^hms_smoke|\\.shp$", "", files)

# dates not online
years = 2006:2020
all_dates = seq.Date(ymd(paste0(min(years), "-01-01")),
                     ymd(paste0(max(years), "-12-31")),
                     by = "day")
all_dates = gsub("-", "", all_dates)
dates_downloaded = dates
dates_not_online = setdiff(all_dates, dates_downloaded)
saveRDS(dates_not_online, "smoke_dates_not_online.rds")

# create smoke list to populate
smoke <- list()
dates_repaired_geometry = c()

# loop through files and save
print(paste("Started:", Sys.time()))
progress <- txtProgressBar(min = 0, max = length(files), style = 3)
for (i in 1:length(files)) {
  df <- read_sf(files[i])
  if (nrow(df) > 0 & all(is.na(st_is_valid(df)))) {
    for (j in 1:nrow(df)) {
      df[j,]$geometry[[1]][[1]] <- rbind(df[j,]$geometry[[1]][[1]], 
                                         df[j,]$geometry[[1]][[1]][1,])
    }
    dates_repaired_geometry = c(dates_repaired_geometry, dates[i])
  }
  df = df[!is.na(st_is_valid(df)),]
  df = st_cast(df, "POLYGON")
  df$date <- dates[i]
  df = df %>% 
    mutate(id = ifelse(nrow(df) > 0, 1:nrow(df), NA),
           Density = ifelse("Density" %in% names(df), as.character(Density), NA))
  smoke[[dates[i]]] = df
  setTxtProgressBar(progress, i)
}
print(paste("Ended:", Sys.time()))

saveRDS(dates_repaired_geometry, "smoke_dates_repaired_geometry.rds")

# 2015-01-25 is particular
smoke_20150125 = smoke[["20150125"]]
smoke_20150125$ID = 1:nrow(smoke_20150125) - 1
smoke_20150125[c("Start", "End", "Density")] = NA
smoke_20150125 = smoke_20150125[c("ID", "Start", "End", "Density", "geometry", "date", "id")]
smoke[["20150125"]] = smoke_20150125

saveRDS(smoke, "smoke_plumes_list_sf.rds")


# get dates with Density column and dates with empty data
dates_density = vector("list", length(files))
not_empty = vector("logical", length(files))
for (i in 1:length(files)) {
  df = read_sf(files[i])
  dates_density[[i]] = names(df)
  not_empty[i] = nrow(df) > 0
}
dates_density = sapply(dates_density, function(x) "Density" %in% x)
dates_density = dates[dates_density]
dates_empty_data = dates[!not_empty]

saveRDS(dates_density, "smoke_dates_density.rds")
saveRDS(dates_empty_data, "smoke_dates_empty_data.rds")


################################################################################
# Turn the list of plumes into a massive SF data frame
################################################################################

smoke = readRDS("smoke_plumes_list_sf.rds")

# read in data to start allocating plumes to fires
not_NA = sapply(smoke, function(x){nrow(x) > 0})
smoke = smoke[not_NA]

smoke_df = bind_rows(smoke) %>% select(date, Start, End, Density, geometry)
st_crs(smoke_df) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "

#save the parsed smoke data
saveRDS(smoke_df, "smoke_plumes_sfdf.RDS")

# dates with invalid geometry
# dates_invalid_geometry = setdiff(all_dates, c(dates_not_online, dates_empty_data, unique(smoke_df$date)))
# saveRDS(dates_invalid_geometry, "smoke_dates_invalid_geometry.rds")
