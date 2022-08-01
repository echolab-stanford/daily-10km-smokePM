library(sp)
library(sf)
library(lubridate)
library(data.table)
library(raster)
library(dplyr)

start_date = "20210101"
end_date = "20220523"

# Combine fire points post-2020
setwd("~/BurkeLab Dropbox/Data/fire")
files <- list.files()
files <- files[grepl(".shp", files) & grepl("hms_fire", files)]
dates = gsub("hms_fire|\\.shp", "", files)
files = files[ymd(dates) %in% seq.Date(ymd(start_date), ymd(end_date), by = "day")]

# create fire list to populate
fire <- list() 
length(fire) <- length(files)
names(fire) <- substr(files, 9, 16)

# loop through files and save
for (i in 1:length(files)) {
  try(fire[[i]] <- st_read(files[i]))
  try(fire[[i]]$date <- as.numeric(substr(files[i],9,16)))
}

saveRDS(fire, sprintf("~/BurkeLab Dropbox/Data/fire/hms_fires_%s-%s.RDS", start_date, end_date))












# ------------------------------------------------------------------------------
# .txt files
start_date = "20030401"
end_date = "20220711"

# Combine fire points post-2020
setwd("~/BurkeLab Dropbox/Data/fire/txt")

files <- list.files()
files <- files[grepl("\\.txt", files) & grepl("hms", files)]
dates = gsub("hms|\\.txt", "", files)
files = files[ymd(dates) %in% seq.Date(ymd(start_date), ymd(end_date), by = "day")]
dates = dates[which(dates >= start_date & dates <= end_date)]

# create fire list to populate
fire <- list() 
length(fire) <- length(files)
names(fire) <- dates

# loop through files and save
p = txtProgressBar(min = 0, max = length(files), style = 3)
for (i in 1:length(files)) {
  try(fire[[i]] <- read.csv(files[i]))
  try(if (nrow(fire[[i]]) > 0) fire[[i]]$date <- as.numeric(substr(files[i],4,11)))
  setTxtProgressBar(p, i)
}

saveRDS(fire, sprintf("hms_fires_%s-%s.RDS", start_date, end_date))
