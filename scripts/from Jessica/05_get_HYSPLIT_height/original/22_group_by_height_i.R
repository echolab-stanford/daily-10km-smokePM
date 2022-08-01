source("work/get_HYSPLIT_height/00_utils.R")

library(Hmisc)
library(dplyr)
library(tidyr)
library(purrr)
library(foreach)
library(doParallel)

num_cores <- 6

#-------------------------------------------------------------------------------
# Group Trajectory-EPA-10km AOT+Smoke Grid Data by Initial Trajectory Height
# Written by Jessica
# Last edited June 2021
#-------------------------------------------------------------------------------
#### Append initial trajectory height to matched data ####
# Read in matched data (post-48 hour plume overlap filtering)
dat_matched <- readRDS(paste0(path_dropbox, "hms_hysplit/trajectories/trajectories_EPA_CA_2020_matched48.rds"))

# Read in full trajectory data
dat_linked <- readRDS(paste0(path_dropbox, "hms_hysplit/trajectories/trajectories_CA_2020_linked.rds"))

# Join injection height to matched data by trajectory ID
dat_matched <- left_join(dat_matched, 
                         dat_linked %>% select(id_traj, height_i) %>% distinct())
rm(dat_linked)

#-------------------------------------------------------------------------------
#### Aggregate ####
# Set radius (meters) if smaller than radius of matching
cutoffs <- 1000*c(10, 30, 50, 80, 100)

# Set parameters for averaging matched trajectory point values
wavg <- TRUE
idw_pwr <- 2

# Set number of nearest neighbors k
k_nn <- 10

# Drop each injection height once
injection_heights <- sort(unique(dat_matched$height_i))
height_pairs <- expand.grid(injection_heights, injection_heights) %>% 
  rename(height_i_low = Var1, 
         height_i_high = Var2) %>% 
  filter(height_i_low < height_i_high)

#-------------------------------------------------------------------------------
# Set up data frame
dat_agg0 <- dat_matched %>% 
  select(id_epa, lon_epa, lat_epa, date, pm25, 
         county, county_code, cbsa_code, cbsa_name) %>% 
  distinct()

lc <- length(cutoffs)
registerDoParallel(ifelse(lc < num_cores, lc, num_cores))
start_time <- get_start_time()
# Takes ~ 20 minutes
dat_agg <- foreach(i = 1:lc) %:% foreach(j = 1:nrow(height_pairs)) %dopar% {
  dat_agg_i <- vector("list", 4)
  cutoff <- cutoffs[i]
  height_pair <- height_pairs[j,] %>% as.numeric()
  low_high <- sprintf("%s.%s", height_pair[1], height_pair[2])
  dat_matched1 <- dat_matched %>% 
    filter(dist <= cutoff, height_i %in% height_pair) %>% 
    mutate(wavg = wavg,
           # Let dist = 1 m in case dist = 0
           dist = ifelse(!dist, 1, dist), 
           wgt = ifelse(wavg, dist^-idw_pwr, 1))
  
  # Nearest neighbor within radius (average if multiple minima)
  dat_agg_i[[1]] <- dat_matched1 %>% 
    group_by(id_epa, date) %>% 
    slice_min(order_by = dist) %>% 
    summarize(across(c(height, pressure, hour_along), mean, na.rm = TRUE)) %>% 
    rename_with(~ sprintf("%s_nn_%s_%s", ., low_high, cutoff), .cols = c(-id_epa, -date))
  
  # IDW average within radius
  dat_agg_i[[2]] <- dat_matched1 %>% 
    group_by(id_epa, date) %>% 
    summarize(n = n(),
              across(c(height, pressure, hour_along), wtd.var, 
                     weights = wgt, normwt = TRUE, .names = "var_{.col}"),
              across(c(height, pressure, hour_along), weighted.mean, w = wgt, na.rm = TRUE)) %>% 
    mutate(across(starts_with("var_"), ~ ifelse(is.na(.), 0, .))) %>% 
    rename_with(~ sprintf("%s_idw_%s_%s", ., low_high, cutoff), .cols = c(-id_epa, -date))
  
  # IDW average up to k nearest neighbors within radius
  dat_agg_i[[3]] <- dat_matched1 %>% 
    group_by(id_epa, date) %>% 
    slice_min(order_by = dist, n = k_nn) %>% 
    summarize(across(c(height, pressure, hour_along), wtd.var, 
                     weights = wgt, normwt = TRUE, .names = "var_{.col}"),
              across(c(height, pressure, hour_along), weighted.mean, w = wgt, na.rm = TRUE)) %>% 
    mutate(across(starts_with("var_"), ~ ifelse(is.na(.), 0, .))) %>% 
    rename_with(~ sprintf("%s_k_%s_%s", ., low_high, cutoff), .cols = c(-id_epa, -date))
  
  # IDW average of trajectory extrema within radius
  dat_agg_i[[4]] <- dat_matched1 %>% 
    group_by(id_epa, date, id_traj) %>% 
    summarize(wgt_h = mean(wgt[height == min(height)]),
              wgt_p = mean(wgt[pressure == max(pressure)]),
              height = min(height),
              pressure = max(pressure)) %>% 
    ungroup(id_traj) %>% 
    summarize(var_height = wtd.var(height, weights = wgt_h, normwt = TRUE),
              var_pressure = wtd.var(pressure, weights = wgt_p, normwt = TRUE),
              height = weighted.mean(height, w = wgt_h, na.rm = TRUE),
              pressure = weighted.mean(pressure, w = wgt_p, na.rm = TRUE)) %>% 
    mutate(across(starts_with("var_"), ~ ifelse(is.na(.), 0, .))) %>% 
    rename_with(~ sprintf("%s_%s_%s_%s", ., c("min", "max"), low_high, cutoff), .cols = c(-id_epa, -date))
  
  # Return list of aggregated data frames
  dat_agg_i
}
print_time(start_time)
beep_alert()
stopImplicitCluster()

# Consolidate to one data frame
dat_agg <- c(list(dat_agg0), unlist(unlist(dat_agg, recursive = FALSE), recursive = FALSE)) %>% 
  reduce(full_join, by = c("id_epa", "date"))

#-------------------------------------------------------------------------------
#### Merge with 10 km grid ####
dat_epa_traj <- dat_agg
rm(dat_agg)

# Reshape data
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
         agg_method, injection_heights, cutoff, height, var_height, 
         pressure, var_pressure, hour_along, var_hour_along, n_traj_points) %>% 
  arrange(date, id_epa, grid_id, agg_method, cutoff, injection_heights)

# Save merged data
saveRDS(dat_merged, paste0(path_dropbox, "hms_hysplit/trajectories/trajectories_EPA_smoke_AOT_CA_2020_48hour_iheight.rds"))
