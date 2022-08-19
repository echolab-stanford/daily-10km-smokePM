source("work/06_complex_MERRA_model/00_utils.R")

library(sf)
library(dplyr)
library(tools)
library(readr)
library(stringr)

#-------------------------------------------------------------------------------
# Combine HMS HYSPLIT Points
# Written by Jessica
# Last edited November 2021
# 
# Aimed to cover Dec 26, 2005 - Dec 31, 2020, including missing dates that we 
# were able to download. Dropped dates before Apr 19, 2006 (due to no 
# initialization time nor smoke production duration) and when shapefile corrupt.
#-------------------------------------------------------------------------------
# Get raw data files
files = list.files(paste0(path_dropbox, "hms_hysplit/"), pattern = "^hysplit|^hmshysplit|^hms_hysplit")
files = grep("\\.txt$|\\.shp$|\\.shx$|\\.dbf$", files, value = T)

# Get dates starting from 20060419
dates = as.character(parse_number(gsub("\\.", "", files)))
dates_run = unique(dates[which(dates >= "20060419")])

# Get dates with non-corrupt shapefile
files_gis = grep("\\.shp$|\\.shx$|\\.dbf$", files, value = T)
dates_gis = unique(as.character(parse_number(gsub("\\.", "", files_gis))))
dates_gis_corrupt = c()
for (d in dates_gis) {
  if (!all(c("shp", "shx", "dbf") %in% file_ext(grep(d, files, value = T)))) dates_gis_corrupt = c(dates_gis_corrupt, d)
}
saveRDS(dates_gis_corrupt, paste0(path_dropbox, "hms_hysplit/hysplit_dates_gis_corrupt.rds"))
dates_run = setdiff(dates_run, dates_gis_corrupt)

# Drop dates before 20060419 or with corrupt shapefile
files = files[which(dates %in% dates_run)]

# Limit to shp extension for shapefiles and get full path
files = grep("\\.txt$|\\.shp$", files, value = T)
files = paste0(path_dropbox, "hms_hysplit/", files)

# Combine into list of dates
dat_hms_hysplit = list()
# Expected warnings OK (occur when converting empty data frame to sf):
#     no non-missing arguments to min; returning Inf
#     no non-missing arguments to max; returning -Inf
# og = options()$warn
# options(warn = 2)
for (file in files) {
  d = as.character(parse_number(gsub("\\.", "", basename(file))))
  if (file_ext(file) == "txt") {
    df = read.csv(file, 
                  colClasses = c(YearMmDd = "character", 
                                 HhMm = "character", 
                                 Dur = "character"))
    if (!("Lon" %in% names(df))) {
      df = read.csv(file, 
                    header = F,
                    col.names = c("Lon", "Lat", "YearMmDd", "HhMm", "Dur"), 
                    colClasses = c(YearMmDd = "character", 
                                   HhMm = "character", 
                                   Dur = "character"))
    }
    df = df %>% 
      rename(Date.YYYYmmdd = YearMmDd,
             Time.HHMM = HhMm,
             Duration.HHMM = Dur) %>% 
      mutate(across(c(Date.YYYYmmdd, Time.HHMM, Duration.HHMM), trimws),
             ID = row_number(), 
             .before = everything()) %>% 
      filter(Time.HHMM != "****",
             Duration.HHMM != "****") %>% 
      st_as_sf(coords = c("Lon", "Lat"),
               remove = F)
  } else if (file_ext(file) == "shp") {
    df = read_sf(file)
    if ("X..ID" %in% names(df)) {
      df = df %>% 
        rename(ID = `X..ID`,
               Lon = `X......LON`,
               Lat = `X....LAT`,
               Date.YYYYmmdd = X.YearMmDd,
               Time.HHMM = X.HhMm,
               Duration.HHMM = X.Dura)
    } else {
      df = df %>% 
        rename(Date.YYYYmmdd = Date,
               Time.HHMM = Time,
               Duration.HHMM = Duration) %>% 
        mutate(ID = row_number(), 
               .before = everything())
    }
    df = df %>% 
      mutate(Date.YYYYmmdd = as.character(Date.YYYYmmdd),
             across(c(Time.HHMM, Duration.HHMM), str_pad, 4, "left", 0))
  }
  dat_hms_hysplit[[d]] = df %>% mutate(Date.YYYYmmdd = d)
}
dat_hms_hysplit = dat_hms_hysplit[sort(names(dat_hms_hysplit))]
# options(warn = og)

# Save
saveRDS(dat_hms_hysplit, paste0(path_dropbox, "hms_hysplit/hms_hysplit_points_20060419-20201231.rds"))
