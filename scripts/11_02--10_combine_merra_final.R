source("../00_utils.R")
library(stringr)
library(tidyr)
library(data.table)
library(exactextractr)

setwd(db_path)

########################################################################################
# Written by: Anne Driscoll
# Last edited by: Anne Driscoll 
# Combine grid cell day data with smoke data and make easily filterable for easy use
########################################################################################

########################################################################################
# combine daily aot, 5 year mean aot and smoke days
########################################################################################

smoke = readRDS(paste0(db_path, "Data/smoke/sparse_smoke_grid.RDS"))
smoke$year = as.numeric(substr(smoke$date, 1, 4))
smoke$month = as.numeric(substr(smoke$date, 5, 6))
names(smoke)[3] = "grid_id"

elevation = readRDS(paste0(git_path, "/data/grid_elevation.RDS"))

for (i in 1:length(years)) {
  
  year = years[i]
  print(year)
  
  # read in the climate variables 
  temp = readRDS(paste0("Data/ERA5/2m_temp/grid_temperature/grid_temperature_", 
                        year, ".rds"))
  precip = readRDS(paste0("Data/ERA5/precip/grid_precipitation/grid_precipitation_", 
                          year, ".rds"))
  pbl = readRDS(paste0("Data/ERA5/pbl/grid_PBL/grid_PBL_", year, ".rds"))
  pbl$date = as.Date(pbl$date)
  fire_dist = readRDS(paste0("Data/10km_grid_data/dist_to_fire/dist_to_fire_", 
                             year, ".RDS"))
  
  # extract the month for climate variables to filter later
  temp$month = month(temp$date)
  precip$month = month(precip$date)
  pbl$month = month(pbl$date)
  fire_dist$month = month(fire_dist$date)
  
  
  for (j in 1:12) {
    
    # read in the daily data
    daily_aot = paste0("Data/MERRA-2/us_grid_daily_aot/daily_grid_aot_", 
                       year, "_", j, ".RDS")
    daily_aot = readRDS(daily_aot)
    
    # read in the 5 year means for background aot
    monthly_means = paste0("Data/MERRA-2/us_grid_3yr_arnd_means/3yr_mean_grid_aot_", 
                           year, "_", j, ".RDS")
    monthly_means = readRDS(monthly_means)
    
    # figure out what smoke data to use
    month_smoke = smoke$year == year & smoke$month == j
    month_smoke = smoke[month_smoke, ]
    month_smoke$smoke_day = 1
    
    # figure out what climate data to use
    month_pbl = pbl[pbl$month == j, ] %>% select(-month)
    month_temp = temp[temp$month == j, ] %>% select(-month)
    month_precip = precip[precip$month == j, ] %>% select(-month)
    month_fire_dist = fire_dist[fire_dist$month == j, ] %>% select(-month)
    month_climate = merge(month_precip, month_temp, by=c("id_grid", "date"), all=T)
    month_climate = merge(month_climate, month_pbl, by=c("id_grid", "date"), all=T)
    month_climate = merge(month_climate, month_fire_dist, by=c("id_grid", "date"), all=T)
    month_climate$date = gsub("-", "", as.character(month_climate$date))
    
    # combine all the data
    full = merge(daily_aot, monthly_means[, c("grid_id", "mean_aot")], 
                 by="grid_id", all=T)
    full = merge(full, month_smoke[, 1:7], by=c("grid_id", "date"), all.x=T)
    full = merge(full, month_climate, 
                 by.x=c("grid_id", "date"), by.y=c("id_grid", "date"), all.x=T)
    full = merge(full, elevation, by.x="grid_id", by.y="id", all.x=T)
    full$smoke_day[is.na(full$smoke_day)] = 0
    saveRDS(full, paste0("Data/MERRA-2/us_grid_combined/combined_", 
                         year, "_", j, ".RDS"))
    print(j)
  }
}

########################################################################################
# get grid level info on population and county so people can filter grid
########################################################################################

# read in the counties and grid cells so we can assign
counties = readRDS("Data/boundaries/all_national_counties.rds")
grid_poly = readOGR(paste0(db_path, "Data/boundaries/10km_grid"), "10km_grid")
grid_poly = spTransform(grid_poly, crs(counties)) #quite slow

# get the overlap and coerce into shapes that are useful
grid_county = over(grid_poly, counties, returnList=T) #get raw overlaps
for (i in 1:length(grid_county)) { #add the grid id and overlap area
  x = grid_county[[i]]
  x$grid_id = grid_poly$ID[i]
  x$area = gArea(gIntersection(grid_poly[i,], 
                               counties[counties$GEOID %in% x$GEOID,], byid=T), 
                 byid=T)
  grid_county[[i]] = x %>% select(grid_id, STATEFP, GEOID, area) %>%
    rename(state=STATEFP, county=GEOID)
  if (i %% 5000 == 0) {print(i)}
}
grid_county = rbindlist(grid_county)

# get the population from a gridded dataset for each grid cell
gpw = raster("Data/population/gpw_v4_population_count_rev11_2015_2pt5_min.tif")
grid_poly = spTransform(grid_poly, crs(gpw))
gpw = crop(gpw, extent(grid_poly))
pop_extract = exact_extract(x=gpw, y=grid_poly, fun=weighted.mean)
pop = data.frame(grid_id = grid_poly$ID, population = pop_extract)

#combine
grid_county = merge(grid_county, pop, by="grid_id", all.x=T)
names(grid_county)[5] = "grid_population"
saveRDS(grid_county, "Data/MERRA-2/filters_for_grid.RDS")


########################################################################################
# make yearly grid AOT/smoke for easier loading
########################################################################################

for (i in 1:length(years)) {
  
  year = years[i]
  print(year)
  
  out = as.list(rep(NA, 12))
  
  for (j in 1:12) {
    month = readRDS(paste0("Data/MERRA-2/us_grid_combined/combined_", 
                           year, "_", j, ".RDS"))
    out[[j]] = month
    print(j)
  }
  
  out = rbindlist(out)
  saveRDS(out, paste0("Data/MERRA-2/us_grid_final/grid_aot_", year, ".RDS"))
}
