library(sp)
library(sf) # v1.0-0 and later don't work! v0.9-8 works; before probably works too
library(lubridate)
library(data.table)
library(raster)
library(dplyr)
library(purrr)

# Combine smoke plumes

################################################################################
# load all smoke plume shapefiles into list named by date
################################################################################

setwd("~/BurkeLab Dropbox/Data/smoke")
files <- list.files(pattern = "^hms_smoke")
files = grep("\\.shp$", files, value = T)
dates = gsub("^hms_smoke|\\.shp$", "", files)

# dates not online
start_date = "20050805"
end_date = "20220711"
all_dates = seq.Date(ymd(start_date), ymd(end_date), by = "day")
all_dates = gsub("-", "", all_dates)
dates_downloaded = dates
dates_not_online = setdiff(all_dates, dates_downloaded)
# saveRDS(dates_not_online, "smoke_dates_not_online.rds")

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
  if (nrow(df) > 0 & (any(is.na(st_is_valid(df)) | any(!st_is_valid(df))))) {
    df = map_dfr(1:nrow(df), function(j) {
      if (length(unlist(df[j,]$geometry)) >= 8) { # need at least 4 points to make polygon
        out = df[j,]
      } else {
        out = NULL
      }
      return(out)
    })
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

# saveRDS(dates_repaired_geometry, "smoke_dates_repaired_geometry.rds")

# 2015-01-25 is particular
smoke_20150125 = smoke[["20150125"]]
smoke_20150125$ID = 1:nrow(smoke_20150125) - 1
smoke_20150125[c("Start", "End", "Density")] = NA
smoke_20150125 = smoke_20150125[c("ID", "Start", "End", "Density", "geometry", "date", "id")]
smoke[["20150125"]] = smoke_20150125

# saveRDS(smoke, "smoke_plumes_list_sf.rds")


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

# saveRDS(dates_density, "smoke_dates_density.rds")
# saveRDS(dates_empty_data, "smoke_dates_empty_data.rds")


################################################################################
# Turn the list of plumes into a massive SF data frame
################################################################################

# smoke = readRDS("smoke_plumes_list_sf.rds")

# read in data to start allocating plumes to fires
not_NA = sapply(smoke, function(x){nrow(x) > 0})
smoke = smoke[not_NA]

smoke_df = bind_rows(smoke) %>% select(date, Start, End, Density, geometry)
st_crs(smoke_df) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "

#save the parsed smoke data
saveRDS(smoke_df, sprintf("smoke_plumes_sfdf_%s_%s.RDS", start_date, end_date))

# dates with invalid geometry
# dates_invalid_geometry = setdiff(all_dates, c(dates_not_online, dates_empty_data, unique(smoke_df$date)))
# saveRDS(dates_invalid_geometry, "smoke_dates_invalid_geometry.rds")
