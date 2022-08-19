source("work/get_HYSPLIT_height/00_utils.R")

library(dplyr)
library(tidyr)

#-------------------------------------------------------------------------------
# Merge Matched EPA Station-Day-PM2.5 and Trajectory Points to Smoke Day and AOT
# Written by Jessica
# Last edited June 2021
#-------------------------------------------------------------------------------
# Read in aggregated data
dat_epa_traj <- readRDS(paste0(path_dropbox, "hms_hysplit/trajectories/trajectories_EPA_CA_2020_aggregated48.rds"))

# Reshape data
dat_epa_traj <- dat_epa_traj %>% 
  pivot_longer(contains("height") | contains("pressure") | 
                 contains("hour_along") | starts_with("n_"),
               names_to = "vbl_agg_cut",
               values_to = "val") %>% 
  separate(vbl_agg_cut,
           into = c("vbl_agg", "cutoff"),
           sep = "_(?=[^_]+$)") %>% 
  # Convert to km
  mutate(cutoff = as.numeric(cutoff)/1000) %>% 
  separate(vbl_agg,
           into = c("vbl", "agg_method"),
           sep = "_(?=[^_]+$)") %>% 
  mutate(agg_method = ifelse(agg_method %in% c("min", "max"), "extr", agg_method)) %>% 
  pivot_wider(names_from = vbl,
              values_from = val) %>% 
  rename(date_station = date,
         n_traj_points = n)

# Read in smoke day and AOT (daily and background) grid
# CHANGE TO ALL YEARS LATER WHEN HAVE FULL DOMAIN'S TRAJS?
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
nc <- 20
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
         county, county_code, cbsa_code, cbsa_name, 
         grid_id, dist, aot, mean_aot, aot_anom, 
         smoke_day, light, medium, dense, total,
         agg_method, cutoff, height, var_height, pressure, var_pressure, 
         hour_along, var_hour_along, n_traj_points) %>% 
  arrange(date, id_epa, grid_id, agg_method, cutoff)

# Save merged file
saveRDS(dat_merged, paste0(path_dropbox, "hms_hysplit/trajectories/trajectories_EPA_smoke_AOT_CA_2020_48hour.rds"))
