source("work/06_complex_MERRA_model/00_utils.R")

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

#-------------------------------------------------------------------------------
# Characterize Missings in AOD
# Written by Jessica
# Last edited August 2021
#-------------------------------------------------------------------------------
# Read in merged panel
dat_merged <- readRDS(paste0(path_dropbox, "PM25/epa_station_smokePM_full_panel_extendedCovariates_AOD.rds")) %>% 
  drop_na(pm25, smokePM) %>% 
  filter(smoke_day == 1) %>% 
  select(epa_id, date, 
         smokePM, fold, month, temperature, precipitation, 
         aot_anom, aot_anom_lag1, aot_anom_lag2, aot_anom_lag3, 
         elevation, lat, lon, km_dist, pbl_min, 
         wind_u, wind_v, wind, elevation_stdDev,
         mean_sea_level_pressure, surface_pressure, 
         dewpoint_2m_temperature, pbl_max, pbl, 
         aod) %>% 
  drop_na(fold) %>% 
  mutate(year = year(date))

# Count number of smoke days missing AOD
count(dat_merged, is.na(aod))

# Plot smoke days missing AOD by year
ggplot(data = dat_merged, mapping = aes(x = year)) + 
  geom_bar(aes(fill = is.na(aod)))

# Plot proportion of smoke days missing AOD by month
ggplot(data = dat_merged, mapping = aes(x = month)) + 
  geom_bar(aes(fill = is.na(aod)), position = "fill")

# Plot proportion of smoke days missing AOD by smoke PM level > 50
ggplot(data = dat_merged, mapping = aes(x = smokePM > 50)) + 
  geom_bar(aes(fill = is.na(aod)), position = "fill")

# Plot distribution of monitors over percent of smoke days missing AOD
ggplot(data = dat_merged %>% 
         group_by(epa_id) %>% 
         summarize(missing = sum(is.na(aod))/nrow(cur_data())) %>% 
         ungroup(),
       mapping = aes(x = missing)) + 
  geom_histogram()

# Plot smoke days missing AOD by fold
ggplot(data = dat_merged, mapping = aes(fold)) + 
  geom_bar(aes(fill = is.na(aod)))
