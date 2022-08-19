source("work/06_complex_MERRA_model/00_utils.R")

library(sf)
library(dplyr)
library(tools)
library(readr)

#-------------------------------------------------------------------------------
# Combine HMS HYSPLIT Points
# Written by Jessica
# Last edited November 2021
# 
# Overall dates: Dec 26, 2005 - May 31, 2010 and missing dates that we were able to download
# 
# EX = actively excluded in this script
# IN = included in this script
# Breakdown of dates:
#   (EX)  Dec 26, 2005 - Apr 18, 2006: cannot initialize trajectories due to no 
#             initialization time nor smoke production duration
#   (IN)  Apr 19, 2006 - May 31, 2010: OK using read.csv
#   (IN)  missing dates from Archive TEXT: OK using read.csv
#   (IN)  missing dates from Backup Archive TEXT: OK using read.csv
#         missing dates from Archive GIS:
#   (IN)      dates in 2008: OK using read_sf
#   (EX)      20141122, 20150325: missing dbf, perhaps corrupt shx
#   (IN)      all of Oct 2018, 20191007: empty data
#                 *likely data issue, as there are fire and smoke these dates
#   (IN)  missing dates from Backup Archive GIS: none were downloaded
#-------------------------------------------------------------------------------
# Get new HYSPLIT points
new_files = list.files(paste0(path_dropbox, "hms_hysplit/"), pattern = "^hms_hysplit")
files = grep("\\.txt$|\\.shp$", new_files, value = T)
dates = as.character(parse_number(files))
dates_20051226.20100531 = readRDS(paste0(path_dropbox, "hms_hysplit/hysplit_dates_20051226-20100531.rds"))
dates_missing_archive_text = readRDS(paste0(path_dropbox, "hms_hysplit/hysplit_dates_missing_archive_text.rds"))
dates_missing_archive_gis = readRDS(paste0(path_dropbox, "hms_hysplit/hysplit_dates_missing_archive_gis.rds"))
dates_missing_backup_text = readRDS(paste0(path_dropbox, "hms_hysplit/hysplit_dates_missing_backup_text.rds"))
dates_missing_backup_gis = readRDS(paste0(path_dropbox, "hms_hysplit/hysplit_dates_missing_backup_gis.rds"))
dates_run = c(dates_20051226.20100531, 
              dates_missing_archive_text, dates_missing_archive_gis, 
              dates_missing_backup_text, dates_missing_backup_gis)
dates_run = dates_run[which(dates_run >= "20060419")]

dates_missing_gis = c(dates_missing_archive_gis, dates_missing_backup_gis)
dates_missing_gis_corrupt = c()
for (d in dates_missing_gis) {
  if (!all(c("shp", "shx", "dbf") %in% file_ext(grep(d, new_files, value = T)))) dates_missing_gis_corrupt = c(dates_missing_gis_corrupt, d)
}
dates_run = setdiff(dates_run, dates_missing_gis_corrupt)

files = files[which(dates %in% dates_run)]
files = paste0(path_dropbox, "hms_hysplit/", files)

dat_hms_hysplit = list()
# Expected warnings OK (occur when converting empty data frame to sf):
#     no non-missing arguments to min; returning Inf
#     no non-missing arguments to max; returning -Inf
# og = options()$warn
# options(warn = 2)
for (file in files) {
  d = as.character(parse_number(basename(file)))
  if (file_ext(file) == "txt") {
    df = read.csv(file) %>% 
      mutate(ID = row_number(), 
             .before = everything()) %>% 
      rename(Date = YearMmDd,
             Time = HhMm,
             Duration = Dur) %>% 
      filter(Time != "****",
             Duration != "****") %>% 
      st_as_sf(coords = c("Lon", "Lat"),
               remove = F) %>% 
      mutate(across(c(Time, Duration), as.integer))
  } else if (file_ext(file) == "shp") {
    df = read_sf(file)
    if (d <= "20081231") {
      df = df %>% rename(ID = `X..ID`,
                         Lon = `X......LON`,
                         Lat = `X....LAT`,
                         Date = X.YearMmDd,
                         Time = X.HhMm,
                         Duration = X.Dura)
    } else {
      df = df %>% mutate(ID = row_number(), .before = everything())
    }
  }
  dat_hms_hysplit[[d]] = df %>% mutate(Date = as.integer(d))
}
# options(warn = og)

saveRDS(dat_hms_hysplit, paste0(path_dropbox, "hms_hysplit/hms_hysplit_points_20051226-20100531-missing.rds"))
