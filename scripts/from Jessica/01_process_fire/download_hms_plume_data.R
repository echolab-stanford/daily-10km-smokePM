library(RCurl)
library(R.utils)
library(XML)
library(rgdal)

setwd("~/BurkeLab Dropbox/Data/smoke")

url_base <- "https://satepsanone.nesdis.noaa.gov/pub/volcano/FIRE/HMS_ARCHIVE/"
yrs <- 2019


for (y in 1:length(yrs)) {
  
  url_info <- getURL(paste(url_base, yrs[y], "/GIS/SMOKE/", sep = ""))
  
  url <- htmlTreeParse(url_info)
  url <- url[[1]]
  url <- url[[2]]
  url <- url[[2]]
  url <- url[4:length(url)]
  
  files <- rep(NA, length(url))
  
  for (i in 1:length(files)) {
    files[i] <- as.character(url[i]$tr[2]$td[1]$a)[2]
  }
  
  if (yrs[y] > 2010) {
    files <- files[grep(x = files, pattern = ".zip")]
  }
  
  for (i in 1:length(files)) {
    try(download.file(
      url = paste(url_base, yrs[y], "/GIS/SMOKE/", files[i], sep = ""),
      destfile = files[i]
    ))
    try(zip::unzip(files[i], junkpaths=T))
    file.remove(files[i])
    
  }
}