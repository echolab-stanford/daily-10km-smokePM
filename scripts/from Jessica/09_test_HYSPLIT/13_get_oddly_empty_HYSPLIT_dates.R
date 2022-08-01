path_dropbox = "~/BurkeLab Dropbox/Data/"

library(dplyr)
library(sf)

#-------------------------------------------------------------------------------
# Get Oddly Empty HYSPLIT Dates
# Written by Jessica
# Last edited December 2021
# 
# Dates where HYSPLIT file is available for download but empty and should be 
# treated as missing data. For each date, criteria are:
#     1. HYSPLIT data file is available online
#     2. HYSPLIT data file is empty
#     3. At least one of the following:
#            a. Number of smoke plumes >= 1
#            b. Smoke data are not available online
# Note: for version 1 covering 2006-2020, smoke data are always available on
# empty HYSPLIT dates. For future versions, if 3.b needs to be invoked, confirm 
# with team first that 3.b should be followed.
#-------------------------------------------------------------------------------
# set criteria
min_num_smoke_plumes = 1

# get empty HYSPLIT dates
hysplit = readRDS(paste0(path_dropbox, "hms_hysplit/hms_hysplit_points_20060419-20201231.rds"))
dates_empty = sapply(hysplit, nrow)
dates_empty = names(hysplit)[which(dates_empty == 0)]
dates_not_online = readRDS(paste0(path_dropbox, "hms_hysplit/hysplit_dates_not_online.rds"))
stopifnot(length(intersect(dates_not_online, dates_empty)) == 0)
dates_gis_corrupt = readRDS(paste0(path_dropbox, "hms_hysplit/hysplit_dates_gis_corrupt.rds"))
stopifnot(length(intersect(dates_gis_corrupt, dates_empty)) == 0)

# looks like we have smoke data files on all the empty HYSPLIT dates
smoke = readRDS(paste0(path_dropbox, "smoke/smoke_plumes_list_sf.rds"))
dates_empty_missing_smoke = setdiff(dates_empty, names(smoke))
for (d in dates_empty_missing_smoke) smoke[[d]] = data.frame()
smoke = smoke[dates_empty]

# filter by smoke plume count
df = data.frame(empty_date = dates_empty,
                num_hysplit_points = 0) %>% 
  mutate(num_smoke_plumes = ifelse(empty_date %in% dates_empty_missing_smoke, NA, sapply(smoke, nrow)))

# subset to oddly empty dates
dates_oddly_empty = df %>% 
  filter((num_smoke_plumes >= min_num_smoke_plumes) | is.na(num_smoke_plumes)) %>% 
  pull(empty_date)

saveRDS(dates_oddly_empty, paste0(path_dropbox, "hms_hysplit/hysplit_dates_oddly_empty.rds"))
