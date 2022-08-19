source("../00_utils.R")
source("../../../pat.R")
setwd(db_path)
library(FNN)
library(data.table)
library(tidyr)
library(tidycensus)

########################################################################################
# Written by: Anne Driscoll
# Last edited by: Anne Driscoll 
# Creates EPA data in daily form
########################################################################################

epa = readRDS("Data/PM25/EPA/epa_station_level_pm25_data.rds")

# create a panel of all date-id combos
dates = seq(as.Date("2006-01-01"), as.Date("2020-12-31"), by="days")
panel = expand.grid(unique(epa$id), dates)
names(panel) = c("id", "date")

# get epa ready to merge to panel 
epa$date = as.Date(paste0(epa$year, "-", epa$month, "-", epa$day))
start_date = as.Date("2005-12-31")
epa = epa %>% 
  filter(date > start_date) %>%
  group_by(id, date) %>%
  summarise(pm25 = mean(pm25, na.rm=T))

# merge to panel
epa = merge(epa, panel, by=c("id", "date"), all=T)

# figure out the grid_id for each epa id
grid = readRDS(paste0(git_path, "/data/grid.RDS"))
epa_ll = readOGR("Data/PM25/epa_station_locations", "epa_station_locations")
epa_ll = spTransform(epa_ll, crs(grid))
knn_ids = get.knnx(coordinates(grid), coordinates(epa_ll), k=1)$nn.index
matches = data.frame(id = epa_ll$id, grid_id = grid$ID[knn_ids]) 

# add the NA cols to fill in loop
epa = epa %>%
  mutate(smoke_day=as.numeric(NA), light=as.numeric(NA), medium=as.numeric(NA), 
         dense=as.numeric(NA), total_smoke=as.numeric(NA), 
         precipitation=as.numeric(NA), temperature=as.numeric(NA), 
         elevation=as.numeric(NA), pbl=as.numeric(NA), pbl_min=as.numeric(NA), 
         pbl_max=as.numeric(NA))

for (i in 1:length(years)) {
  year = years[i]
  merra = readRDS(paste0("Data/MERRA-2/us_grid_final/grid_aot_", year, ".RDS"))
  merra$date = as.Date(merra$date, format="%Y%m%d")
  
  merra = merra %>% 
    merge(matches, by="grid_id") %>%
    dplyr::select(id, date, smoke_day, light, medium, dense, total,
                  precipitation, temperature, elev, pbl, pbl_min, pbl_max)
  
  cur = epa[year(epa$date) == year, 1:3]
  cur = merge(cur, merra, by=c("id", "date"))
  epa[year(epa$date) == year, ] = cur
  print(year)
}

saveRDS(epa, "Data/PM25/panel_station_pm_smoke_day.RDS")


########################################################################################
# get the epa stuff connected to county geo
########################################################################################

# get the counties
counties = readRDS("Data/boundaries/counties.RDS")
epa_ll = spTransform(epa_ll, crs(counties))
over_c = over(epa_ll, counties)
matches = data.frame(id = epa_ll$id, 
                     county = over_c$GEOID, 
                     state = over_c$STATEFP) 

# for monitors that didn't match directly, take nearest county (unless far)
knn_ids = get.knnx(coordinates(counties), 
                   coordinates(epa_ll[is.na(matches$county), ]), k=1)
knn_ids$nn.index[knn_ids$nn.dist > 1] = NA
knn_ids = knn_ids$nn.index

w = is.na(matches$county)
matches[is.na(matches$county), ] = data.frame(id = epa_ll$id[w], 
                                              county = counties$GEOID[knn_ids], 
                                              state = counties$STATEFP[knn_ids])

epa_c = merge(epa, matches, by = "id", all.x=T)
saveRDS(epa_c, "Data/PM25/panel_station_pm_smoke_day_w_county.RDS")



########################################################################################
# aggregate to county level and add county data
########################################################################################

epa_c = readRDS("Data/PM25/panel_station_pm_smoke_day_w_county.RDS")

# PM MEANS CODE FROM MARISSA!! thank youuu
pm_means = epa_c %>% 
  mutate(month = month(date), year = year(date)) %>%
  group_by(county, month, year) %>% 
  summarise(pm25 = list(pm25[which(!is.na(pm25) & smoke_day == 0)])) %>% # grab the non NA and non-smoke day PM obs
  rowwise() %>%
  mutate(nobs = length(pm25)) %>% # for each station-month year, how many obs meet criteria?
  ungroup %>%
  arrange(county, month, year) %>%
  group_by(county, month) %>%
  # add leads and lags
  mutate(pm25_lead = lead(pm25, n = 1, default = list(NA)), # add NAs if there is no previous year which we can ignore while taking medians
         pm25_lag = lag(pm25, n = 1, default = list(NA)), # add NAs if there is no future year which we can ignore while taking medians
         nobs_lead = lead(nobs, n = 1, default = 0), 
         nobs_lag = lag(nobs, n = 1, default = 0))  %>% 
  ungroup %>%
  rowwise %>%
  mutate(pm25_3yr = list(c(pm25, pm25_lag, pm25_lead)), # combine pm from current, lag, and lead
         nobs_3yr = nobs + nobs_lead + nobs_lag) %>% # add up the obs
  rowwise() %>%
  mutate(pm25_med_3yr = median(unlist(pm25_3yr), na.rm = T)) %>%
  select(county, year, month, pm25_med_3yr)

# aggregate to county-day level and calc smokePM
epa_c %<>% 
  group_by(county, date) %>%
  summarise(n=n(), nas=sum(is.na(pm25)), pm25=mean(pm25, na.rm=T), 
            smoke_day=max(smoke_day), temperature=mean(temperature), 
            elevation=mean(elevation), precipitation=mean(precipitation)) %>%
  mutate(month=month(date), year=year(date)) %>%
  merge(pm_means, all.x=T, by=c("county", "year", "month")) %>%
  mutate(smokePM=pmax(if_else(smoke_day == 1, pm25 - pm25_med_3yr, 0), 0))

# get Census data to merge
county_income = get_acs("county", survey="acs5", 
                        variables=c("B19013_001", "B01003_001"), 
                        year=2019, key=census_api_key) %>%
    select(GEOID, variable, estimate) %>%
    spread(variable, estimate) %>%
    rename(county=GEOID, median_household_income=B19013_001, 
           population=B01003_001)
  
# merge the county level panel
epa_c = merge(epa_c, county_income, by="county", all.x=T)
epa_c = epa_c %>% select(county, date, n, nas, pm25, pm25_med_3yr, smokePM, 
                         smoke_day, temperature, elevation, precipitation, 
                         median_household_income, population) %>%
  filter(!is.na(epa_c$county))
saveRDS(epa_c, "Data/PM25/panel_county_pm_smoke_day.RDS")
