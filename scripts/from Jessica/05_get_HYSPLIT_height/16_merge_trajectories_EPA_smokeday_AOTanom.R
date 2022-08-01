source("work/05_get_HYSPLIT_height/00_utils.R")

library(dplyr)
library(tidyr)

#-------------------------------------------------------------------------------
# Merge Matched EPA Station-Day-PM2.5 and Trajectory Points to Smoke Day and AOT
# Written by Jessica
# Last edited July 2021
#-------------------------------------------------------------------------------
# Read in aggregated data
dat_epa_traj <- readRDS(paste0(path_dropbox, "hms_hysplit/trajectories/trajectories_72hr_CA_2020_overlap_48hr_EPA_aggregated.rds"))

# Reshape data
# This takes several minutes
dat_epa_traj <- dat_epa_traj %>% 
  pivot_longer(contains("height") | contains("pressure") | 
                 contains("hour_along") | starts_with("n_"),
               names_to = "vbl_agg_inj_cut",
               values_to = "val") %>% 
  separate(vbl_agg_inj_cut,
           into = c("vbl_agg_inj", "cutoff"),
           sep = "_(?=[^_]+$)") %>% 
  # Convert to km
  mutate(cutoff = as.numeric(cutoff)/1000) %>% 
  separate(vbl_agg_inj,
           into = c("vbl_agg", "injection_heights"),
           sep = "_(?=[^_]+$)") %>% 
  mutate(injection_heights = gsub(".", "+", injection_heights, fixed = TRUE)) %>% 
  separate(vbl_agg,
           into = c("vbl", "agg_method"),
           sep = "_(?=[^_]+$)") %>% 
  mutate(agg_method = ifelse(agg_method %in% c("min", "max"), "extr", agg_method)) %>% 
  pivot_wider(names_from = vbl,
              values_from = val) %>% 
  rename(date_station = date,
         n_traj_points = n)

# Save
saveRDS(dat_epa_traj, paste0(path_project, "reshaped_aggregated_72hr_traj_matched.rds"))

# Read in smoke day and AOT (daily and background) grid
dat_smoke_aot <- readRDS(paste0(path_dropbox, "MERRA-2/us_grid_final/grid_aot_2020.RDS")) %>% 
  mutate(date = paste(substr(date, 1, 4), substr(date, 5, 6), substr(date, 7, 8), sep = "-"),
         aot_anom = aot - mean_aot) %>% 
  rename(date_grid = date)

# Read in grid and EPA crosswalk
crosswalk <- readRDS(paste0(path_dropbox, "boundaries/10km_grid_EPA_crosswalk.RDS"))

#-------------------------------------------------------------------------------
# Join grid ID to trajectory-EPA data on EPA ID
dat_merged <- dat_epa_traj %>% left_join(crosswalk, by = c("id_epa" = "epa_id"))

# Join smoke-AOT data to trajectory-EPA data on grid ID
nc <- 80
chunks <- split_chunks(1:nrow(dat_merged), nc)
chunk_list <- vector("list", nc)
start_time <- get_start_time("Started merging with grid:")
for (i in 1:nc) {
  print(paste("Working on chunk:", i))
  chunk_list[[i]] <- dat_merged[chunks[[i]],] %>% 
    left_join(dat_smoke_aot, by = "grid_id") %>% 
    filter(date_station == date_grid)
}
print_time(start_time, message = "Finished merging with grid:")

# Clean up merged data frame
dat_merged <- chunk_list %>% 
  bind_rows() %>% 
  select(-date_grid) %>% 
  rename(date = date_station) %>% 
  # Rearrange order of columns
  select(date, id_epa, lon_epa, lat_epa, pm25, 
         state_fips, county_fips, county_name, cbsa_code, cbsa_name, 
         grid_id, dist, aot, mean_aot, aot_anom, 
         smoke_day, light, medium, dense, total,
         precipitation, temperature, pbl, pbl_min, pbl_max, elev,
         agg_method, injection_heights, cutoff, height, var_height, 
         pressure, var_pressure, hour_along, var_hour_along, n_traj_points) %>% 
  arrange(date, id_epa, grid_id, agg_method, cutoff, injection_heights)

rm(chunk_list, chunks, dat_epa_traj, dat_smoke_aot, crosswalk)

#-------------------------------------------------------------------------------
# Read in EPA data that has background PM2.5 and get anomalous PM2.5
dat_covariates <- readRDS(paste0(path_dropbox, "PM25/epa_station_covariates.rds")) %>% 
  mutate(smokePM = pmax(if_else(smoke_day == 1, pm25 - pm25_med_3yr, 0), 0), 
         date = strftime(date)) %>% 
  select(epa_id, date, pm25_med_3yr, smokePM)
  
# Join smoke PM2.5 into merged data
dat_merged <- dat_merged %>% 
  left_join(dat_covariates, by = c("id_epa" = "epa_id", "date")) %>% 
  rename(elevation = elev)

# Save merged file
saveRDS(dat_merged, paste0(path_dropbox, "hms_hysplit/trajectories/trajectories_72hr_CA_2020_overlap_48hr_EPA_smoke_AOT_covariates.rds"))
