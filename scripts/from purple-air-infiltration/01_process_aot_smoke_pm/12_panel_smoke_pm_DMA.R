source("../00_utils.R")
source("../../../pat.R")
setwd(db_path)

library(FNN)
library(data.table)
library(tidyr)
library(magrittr)
library(Hmisc)

########################################################################################
# Written by: Anne Driscoll
# Last edited by: Anne Driscoll 
# Creates EPA data in daily form
########################################################################################

epa = readRDS("Data/PM25/panel_station_pm_smoke_day.RDS")


########################################################################################
# get the epa stuff connected to county geo
########################################################################################

# get the DMA's
epa_ll = readOGR("Data/PM25/epa_station_locations", "epa_station_locations")
epa_ll = epa_ll[epa_ll$lon >= -125.5 & epa_ll$lon <= -66.5 &
                  epa_ll$lat >= 24.5 & epa_ll$lat <= 50, ]

dma = readOGR("Data/boundaries/DMA_Shapefile", "DMA_Shapefile")
dma_coords = coordinates(dma)
epa_ll = spTransform(epa_ll, crs(dma))
over_d = over(epa_ll, dma)
matches = data.frame(id = epa_ll$id, 
                     dma = over_d$DMA) 

# for monitors that didn't match directly, take nearest county (unless far)
knn_ids = get.knnx(coordinates(dma), 
                   coordinates(epa_ll[is.na(matches$dma), ]), k=1)
knn_ids$nn.index[knn_ids$nn.dist > 100000] = NA # more than 100km away
knn_ids = knn_ids$nn.index

w = is.na(matches$dma)
matches[is.na(matches$dma), ] = data.frame(id = epa_ll$id[w], 
                                           dma = dma$DMA[knn_ids])

epa_d = merge(epa, matches, by = "id")
saveRDS(epa_d, "Data/PM25/panel_station_pm_smoke_day_w_dma.RDS")


########################################################################################
# aggregate to dma-week level
########################################################################################

# PM MEANS CODE FROM MARISSA!! thank youuu
pm_means = epa_d %>% 
  mutate(month = month(date), year = year(date)) %>%
  group_by(dma, month, year) %>% 
  summarise(pm25 = list(pm25[which(!is.na(pm25) & smoke_day == 0)])) %>% # grab the non NA and non-smoke day PM obs
  rowwise() %>%
  mutate(nobs = length(pm25)) %>% # for each station-month year, how many obs meet criteria?
  ungroup %>%
  arrange(dma, month, year) %>%
  group_by(dma, month) %>%
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
  select(dma, year, month, pm25_med_3yr)


weeks = seq(as.Date("2006-01-01", tz="GMT"), 
            as.Date("2020-12-31", tz="GMT"), by="7 days")
match_week = function(x) {
  diff = x-weeks
  w = which(diff >= 0)
  if (length(w) == 0) {return(NA)}
  w2 = which(diff[w] == min(diff[w]))
  return(as.Date(weeks[w][w2]))

}

# aggregate to dma-day level and calc smoke PM
epa_d %<>%  
  group_by(dma, date) %>%
  summarise(n=n(), 
            nas=sum(is.na(pm25)), 
            pm25=mean(pm25, na.rm=T), 
            smoke_day=max(smoke_day), 
            temperature=mean(temperature), 
            elevation=mean(elevation), 
            precipitation=mean(precipitation)) %>%
  mutate(month=month(date), 
         year=year(date), 
         week=sapply(date, match_week),
         week=as.Date(week, origin=as.Date("1970-01-01"))) %>%
  merge(pm_means, all.x=T, by=c("dma", "year", "month")) %>%
  mutate(smokePM=pmax(if_else(smoke_day == 1, pm25 - pm25_med_3yr, 0), 0)) %>%
  ungroup() %>% 
  group_by(dma, week) %>%
  summarise(total_obs=sum(n), 
            nas=sum(nas),
            days=n(), 
            pm25=mean(pm25, na.rm=T), 
            smokePM=mean(smokePM, na.rm=T),
            smoke_day=max(smoke_day, na.rm=T),
            temperature=mean(temperature), 
            elevation=mean(elevation), 
            precipitation=mean(precipitation))


########################################################################################
# get county level data and aggregate to DMA
########################################################################################

counties = readRDS("Data/boundaries/all_national_counties.RDS")
dma = spTransform(dma, crs(counties))
over_d = over(dma, counties, returnList=T)
for (i in 1:length(over_d)) {
  cur = over_d[[i]]
  cur %<>% 
    rename(county=GEOID) %>% select(county) %>% mutate(dma=dma$DMA[i])
  over_d[[i]] = cur
}
matches = rbindlist(over_d)

county_data = readRDS("Data/PM25/panel_county_pm_smoke_day.RDS") 
county_data %<>%
  select(county, median_household_income, population) %>%
  unique() %<>% 
  merge(matches, by="county") %>%
  group_by(dma) %>%
  summarise(median_household_income=wtd.mean(median_household_income, 
                                             weights=population), 
            population=mean(population, na.rm=T))


########################################################################################
# combine county and dma data
########################################################################################

final = merge(epa_d, county_data, by="dma", all.x=T) %>%
  drop_na(dma)

saveRDS(final, "Data/PM25/panel_dma_pm_smoke_day_weekly.RDS")
