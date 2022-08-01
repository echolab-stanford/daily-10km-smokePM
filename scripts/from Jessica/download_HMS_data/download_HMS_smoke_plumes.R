library(lubridate)
library(rvest)
library(R.utils)
library(tools)
library(readr)

# ------------------------------------------------------------------------------
# Download NOAA HMS Smoke Plumes
# Written by: Jessica Li
# Last edited: July 2022 (in preparation for change to Fire and Smoke Product/
# discontinuation of HYSPLIT Points)
# ------------------------------------------------------------------------------
# Set dates
start_date = "20050101"
end_date = "20220711"

# ------------------------------------------------------------------------------
base_url = "https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS"
year_months = unique(substr(seq.Date(ymd(start_date), ymd(end_date), by = "day"), 1, 7))

# Download as shapefiles
setwd("~/BurkeLab Dropbox/Data/smoke/")

for (year_month in year_months) {
  year = substr(year_month, 1, 4)
  month = substr(year_month, 6, 7)
  url = sprintf("%s/Smoke_Polygons/Shapefile/%s/%s/", base_url, year, month)
  files = url %>% 
    read_html() %>% 
    html_nodes("a") %>% 
    html_attr("href")
  files = grep("^hms_smoke20[0-2][0-9][0-1][0-9][0-3][0-9]\\.zip$", files, value = T)
  dates = gsub("hms_smoke|\\.zip", "", files)
  files = files[which(ymd(dates) %within% interval(ymd(start_date), ymd(end_date)))]
  if (length(files) > 0) {
    for (f in files) {
      url_file = paste0(url, f)
      download.file(url_file, f)
      unzip(f)
      file.remove(f)
    }
  }
}
