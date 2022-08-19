library(lubridate)
library(rvest)
library(R.utils)
library(tools)
library(readr)

setwd("~/BurkeLab Dropbox/Data/fire/")

start_date = "20210101"
end_date = "20220523"

year_months = unique(substr(seq.Date(ymd(start_date), ymd(end_date), by = "day"), 1, 7))
for (year_month in year_months) {
  year = substr(year_month, 1, 4)
  month = substr(year_month, 6, 7)
  url = sprintf("https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Fire_Points/Shapefile/%s/%s/", year, month)
  files = url %>% 
    read_html() %>% 
    html_nodes("a") %>% 
    html_attr("href")
  files = grep("^hms_fire202[1-2][0-1][0-9][0-3][0-9]\\.zip$", files, value = T)
  dates = gsub("hms_fire|\\.zip", "", files)
  files = files[which(ymd(dates) %within% interval(ymd(start_date), ymd(end_date)))]
  for (f in files) {
    url_file = paste0(url, f)
    download.file(url_file, f)
    unzip(f)
    unlink(f)
  }
}








# ------------------------------------------------------------------------------
# .txt files
setwd("~/BurkeLab Dropbox/Data/fire/txt/")

start_date = "20030401"
end_date = "20220711"

year_months = unique(substr(seq.Date(ymd(start_date), ymd(end_date), by = "day"), 1, 7))
for (year_month in year_months) {
  year = substr(year_month, 1, 4)
  month = substr(year_month, 6, 7)
  url = sprintf("https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Fire_Points/Text/%s/%s/", year, month)
  files = url %>% 
    read_html() %>% 
    html_nodes("a") %>% 
    html_attr("href")
  files = grep("^hms20[0-2][0-9][0-1][0-9][0-3][0-9]\\.txt$", files, value = T)
  dates = gsub("hms|\\.txt", "", files)
  files = files[which(ymd(dates) %within% interval(ymd(start_date), ymd(end_date)))]
  for (f in files) {
    url_file = paste0(url, f)
    download.file(url_file, f)
  }
}
