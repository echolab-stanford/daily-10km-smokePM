library(lubridate)
library(rvest)
library(R.utils)
library(tools)
library(readr)

setwd("~/BurkeLab Dropbox/Data/hms_hysplit/")

start_date = "20210523"
end_date = "20220709"

url = "https://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/TXT_ARCHIVE/"
files = url %>% 
  read_html %>% 
  html_nodes("a") %>% 
  html_attr("href")
files = grep("^hmshysplit", files, value = T)
files = grep("\\.txt$", files, value = T)
dates = gsub("hmshysplit|\\.txt", "", files)
files = files[which(ymd(dates) %within% interval(ymd(start_date), ymd(end_date)))]
for (f in files) {
  url_file = paste0(url, f)
  download.file(url_file, f)
}




















source("work/06_complex_MERRA_model/00_utils.R")

library(lubridate)
library(rvest)
library(R.utils)
library(tools)
library(readr)

setwd(paste0(path_dropbox, "hms_hysplit/"))

#-------------------------------------------------------------------------------
# Download HMS HYSPLIT Points
# Written by Jessica
# Last edited April 2022
# 
# Dec 26, 2005 - May 31, 2010 and missing dates
#-------------------------------------------------------------------------------
update_year = 2021

#### Download in 2021 ####
# Set range of dates to download
start_date = paste0(update_year, "0101")
end_date = paste0(update_year, "1231")
all_dates = seq.Date(ymd(start_date), ymd(end_date), by = "day")
all_dates_str = format(all_dates, "%Y%m%d")
years = unique(year(all_dates))
years = years[years < 2020] # HMS Archive does not contain 2020 and after

if (length(years) > 0) {
  
  # Set up URL parts
  archive_url = "https://satepsanone.nesdis.noaa.gov/pub/volcano/FIRE/HMS_ARCHIVE/"
  data_format = "TEXT"
  dates0 = c()
  
  start_time = get_start_time()
  for (y in years) {
    # Get file names
    dates_y = grep(paste0("^", y), all_dates_str, value = T)
    url = sprintf("%s%s/%s/", archive_url, y, data_format)
    files = url %>% 
      read_html %>% 
      html_nodes("a") %>% 
      html_attr("href")
    
    # Limit to HYSPLIT points
    files = grep("^hmshysplit", files, value = T)
    files = grep("\\.txt\\.gz$", files, value = T)
    dates = gsub("^hmshysplit|\\.txt\\.gz$", "", files)
    files = files[which(dates %in% dates_y)]
    
    for (f in files) {
      dates0 = c(dates0, parse_number(f))
      url_file = paste0(url, f)
      dest_file = gsub("hmshysplit", "hms_hysplit", f)
      download.file(url_file, dest_file)
      gunzip(dest_file)
    }
  }
  print_time(start_time)
  
  dates0 = as.character(dates0)
  saveRDS(dates0, paste0(path_dropbox, "hms_hysplit/hysplit_dates_2021.rds"))
}

#-------------------------------------------------------------------------------
#### Download missing HYSPLIT dates from HMS Archive if possible ####
# Find missing HYSPLIT dates
dates_downloaded = list.files(paste0(path_dropbox, "hms_hysplit/"), pattern = "^hysplit|^hmshysplit|^hms_hysplit")
dates_downloaded = grep("\\.shp$|\\.txt$", dates_downloaded, value = T)
dates_downloaded = gsub("^hysplit\\.|^hmshysplit|^hms_hysplit|\\.shp$|\\.txt$", "", dates_downloaded)
dates_not_online = setdiff(all_dates_str, dates_downloaded)

years = unique(substr(dates_not_online, 1, 4))
years = years[years < 2020] # HMS Archive does not contain 2020 and after

if (length(years) > 0) {
  # Try TEXT first
  data_format = "TEXT"
  dates_text = c()
  
  start_time = get_start_time()
  for (y in years) {
    # Get file names
    dates_y = grep(paste0("^", y), dates_not_online, value = T)
    url = sprintf("%s%s/%s/", archive_url, y, data_format)
    files = url %>% 
      read_html %>% 
      html_nodes("a") %>% 
      html_attr("href")
    
    # Limit to HYSPLIT points
    files = grep("^hmshysplit", files, value = T)
    files = grep(paste(dates_y, collapse = "|"), files, value = T)
    if (length(files) == 0) next
    
    for (f in files) {
      dates_text = c(dates_text, parse_number(f))
      url_file = paste0(url, f)
      dest_file = gsub("hmshysplit", "hms_hysplit", f)
      download.file(url_file, dest_file)
      if (file_ext(dest_file) == "gz") gunzip(dest_file)
    }
  }
  print_time(start_time)
  
  dates_text = as.character(dates_text)
  saveRDS(dates_text, paste0(path_dropbox, "hms_hysplit/hysplit_dates_missing_archive_text_2021.rds"))
}

# Try GIS next
dates_downloaded = list.files(paste0(path_dropbox, "hms_hysplit/"), pattern = "^hysplit|^hmshysplit|^hms_hysplit")
dates_downloaded = grep("\\.shp$|\\.txt$", dates_downloaded, value = T)
dates_downloaded = gsub("^hysplit\\.|^hmshysplit|^hms_hysplit|\\.shp$|\\.txt$", "", dates_downloaded)
dates_not_online = setdiff(all_dates_str, dates_downloaded)

years = unique(substr(dates_not_online, 1, 4))
years = years[years < 2020] # HMS Archive does not contain 2020 and after

if (length(years) > 0) {
  
  data_format = "GIS"
  dates_gis = c()
  
  start_time = get_start_time()
  for (y in years) {
    # Get file names
    dates_y = grep(paste0("^", y), dates_not_online, value = T)
    url = sprintf("%s%s/%s/HYSPLIT/", archive_url, y, data_format)
    error = tryCatch(
      {files = url %>% 
        read_html %>% 
        html_nodes("a") %>% 
        html_attr("href")}, 
      error = function(e) e
    )
    if(inherits(error, "error")) next
    
    # Limit to missing dates
    files = grep(paste(dates_y, collapse = "|"), files, value = T)
    if (length(files) == 0) next
    
    for (f in files) {
      dates_gis = c(dates_gis, parse_number(f))
      url_file = paste0(url, f)
      download.file(url_file, f)
      if (file_ext(f) == "gz") {
        gunzip(f, overwrite = T)
        f = file_path_sans_ext(f)
      }
      if (file_ext(f) == "zip") {
        zip_file = f
        f = unzip(zip_file, overwrite = T, junkpaths = T)
        f = basename(f)
        unlink(zip_file)
      }
      new_names = gsub("^hysplit|^hysplit\\.|^hmshysplit", "hms_hysplit", f)
      file.rename(f, new_names)
    }
  }
  print_time(start_time)
  
  dates_gis = as.character(unique(dates_gis))
  saveRDS(dates_gis, paste0(path_dropbox, "hms_hysplit/hysplit_dates_missing_archive_gis_2021.rds"))
}

#-------------------------------------------------------------------------------
#### Download missing HYSPLIT dates from HMS Backup Archive if possible ####
# Set up URL parts
archive_url = "https://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/hms_backup/"

# Find missing HYSPLIT dates
dates_downloaded = list.files(paste0(path_dropbox, "hms_hysplit/"), pattern = "^hysplit|^hmshysplit|^hms_hysplit")
dates_downloaded = grep("\\.shp$|\\.txt$", dates_downloaded, value = T)
dates_downloaded = gsub("^hysplit\\.|^hmshysplit|^hms_hysplit|\\.shp$|\\.txt$", "", dates_downloaded)
dates_not_online = setdiff(all_dates_str, dates_downloaded)

years = unique(substr(dates_not_online, 1, 4))

# Try TEXT first
data_format = "TEXT"
dates_text = c()

start_time = get_start_time()
for (y in years) {
  # Get file names
  dates_y = grep(paste0("^", y), dates_not_online, value = T)
  url = sprintf("%s%s/%s/", archive_url, y, data_format)
  files = url %>% 
    read_html %>% 
    html_nodes("a") %>% 
    html_attr("href")
  
  # Limit to HYSPLIT points
  files = grep("^hmshysplit", files, value = T)
  files = grep(paste(dates_y, collapse = "|"), files, value = T)
  if (length(files) == 0) next
  
  for (f in files) {
    dates_text = c(dates_text, parse_number(f))
    url_file = paste0(url, f)
    dest_file = gsub("hmshysplit", "hms_hysplit", f)
    download.file(url_file, dest_file)
    if (file_ext(dest_file) == "gz") gunzip(dest_file)
  }
}
print_time(start_time)

dates_text = as.character(dates_text)
saveRDS(dates_text, paste0(path_dropbox, "hms_hysplit/hysplit_dates_missing_backup_text_2021.rds"))

# Try GIS next
# Note: this did not download anything further
dates_downloaded = list.files(paste0(path_dropbox, "hms_hysplit/"), pattern = "^hysplit|^hmshysplit|^hms_hysplit")
dates_downloaded = grep("\\.shp$|\\.txt$", dates_downloaded, value = T)
dates_downloaded = gsub("^hysplit\\.|^hmshysplit|^hms_hysplit|\\.shp$|\\.txt$", "", dates_downloaded)
dates_not_online = setdiff(all_dates_str, dates_downloaded)

years = unique(substr(dates_not_online, 1, 4))

data_format = "GIS"
dates_gis = c()

start_time = get_start_time()
for (y in years) {
  # Get file names
  dates_y = grep(paste0("^", y), dates_not_online, value = T)
  url = sprintf("%s%s/%s/HYSPLIT/", archive_url, y, data_format)
  error = tryCatch(
    {files = url %>% 
      read_html %>% 
      html_nodes("a") %>% 
      html_attr("href")}, 
    error = function(e) e
  )
  if(inherits(error, "error")) next
  
  # Limit to missing dates
  files = grep(paste(dates_y, collapse = "|"), files, value = T)
  if (length(files) == 0) next
  
  for (f in files) {
    dates_gis = c(dates_gis, parse_number(f))
    url_file = paste0(url, f)
    download.file(url_file, f)
    if (file_ext(f) == "gz") {
      gunzip(f, overwrite = T)
      f = file_path_sans_ext(f)
    }
    if (file_ext(f) == "zip") {
      zip_file = f
      f = unzip(zip_file, overwrite = T, junkpaths = T)
      f = basename(f)
      unlink(zip_file)
    }
    new_names = gsub("^hysplit|^hysplit\\.|^hmshysplit", "hms_hysplit", f)
    file.rename(f, new_names)
  }
}
print_time(start_time)

dates_gis = as.character(unique(dates_gis))
saveRDS(dates_gis, paste0(path_dropbox, "hms_hysplit/hysplit_dates_missing_backup_gis_2021.rds"))

#-------------------------------------------------------------------------------
# Keep track of dates not online
dates_downloaded = list.files(paste0(path_dropbox, "hms_hysplit/"), pattern = "^hysplit|^hmshysplit|^hms_hysplit")
dates_downloaded = grep("\\.shp$|\\.txt$", dates_downloaded, value = T)
dates_downloaded = gsub("^hysplit\\.|^hmshysplit|^hms_hysplit|\\.shp$|\\.txt$", "", dates_downloaded)
dates_not_online = setdiff(all_dates_str, dates_downloaded)

# Save final set of dates not online
saveRDS(dates_not_online, paste0(path_dropbox, "hms_hysplit/hysplit_dates_not_online_2021.rds"))
