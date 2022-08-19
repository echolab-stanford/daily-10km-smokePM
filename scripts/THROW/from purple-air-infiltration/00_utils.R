

db_path = "~/BurkeLab Dropbox/"
git_path = "~/Documents/GitHub/purple-air-infiltration"


library(sp)
library(sf)
library(raster)
library(rgdal)
library(rgeos)


library(dplyr)

years = 2006:2020


print_time = function(t, message="Time elapsed:") {
  t = difftime(Sys.time(), t, units="min")
  t = round(t, digits=2)
  
  message = paste(message, t, "minutes")
  print(message)
}
