source("work/05_get_HYSPLIT_height/00_utils.R")

library(Hmisc)
library(dplyr)
library(purrr)
library(foreach)
library(doParallel)

num_cores <- 6

#-------------------------------------------------------------------------------
# Aggregate HYSPLIT Trajectory Points Matched to EPA Station-Days
# Written by Jessica
# Last edited July 2021
# 
# Variables of interest: 
#     height of trajectory point in meters above ground level
#     pressure of trajectory point in hPa
#     hour_along the trajectory (methods #1-3)
#     n of matched trajectory points (method #2)
# 
# Methods for aggregating:
# 1. Nearest neighbor within radius (average if multiple NN at same distance)
# 2. IDW average within radius
# 3. IDW average up to k nearest neighbors within radius
# 4. IDW average of within-trajectory extrema within radius
#     * height - minima
#     * pressure - maxima
#-------------------------------------------------------------------------------
# Set radius (meters) if smaller than radius of matching
cutoffs <- 1000*c(10, 30, 50, 80, 100)

# Set parameters for averaging matched trajectory point values
wavg <- TRUE
idw_pwr <- 2

# Set number of nearest neighbors k
k_nn <- 10

# Read in matched data
dat_matched <- readRDS(paste0(path_dropbox, "hms_hysplit/trajectories/trajectories_72hr_CA_2020_overlap_48hr_EPA_100km.rds"))

# Read in full trajectory data
dat_linked <- readRDS(paste0(path_dropbox, "hms_hysplit/trajectories/trajectories_72hr_CA_2020_linked.rds"))

# Join injection height to matched data by trajectory ID
dat_matched <- left_join(dat_matched, 
                         dat_linked %>% select(id_traj, height_i) %>% distinct())
rm(dat_linked)

#-------------------------------------------------------------------------------
# Drop each injection height once
injection_heights <- sort(unique(dat_matched$height_i))
height_pairs <- expand.grid(injection_heights, injection_heights) %>% 
  filter(Var1 < Var2)
height_combos <- split(height_pairs, seq(nrow(height_pairs))) %>%
  lapply(t) %>%
  lapply(c)
height_combos[[length(height_combos) + 1]] <- injection_heights

# Set up data frame
dat_agg0 <- dat_matched %>% 
  select(id_epa, lon_epa, lat_epa, date, pm25, 
         state_fips, county_fips, county_name, cbsa_code, cbsa_name) %>% 
  distinct()

lc <- length(cutoffs)
registerDoParallel(ifelse(lc < num_cores, lc, num_cores))
start_time <- get_start_time()
# Takes ~ 20 minutes
dat_agg <- foreach(i = 1:lc) %:% foreach(j = 1:length(height_combos)) %dopar% {
  dat_agg_i <- vector("list", 4)
  cutoff <- cutoffs[i]
  height_combo <- height_combos[[j]]
  height.combo <- paste(height_combo, collapse = ".")
  dat_matched1 <- dat_matched %>% 
    filter(dist <= cutoff, height_i %in% height_combo) %>% 
    mutate(wavg = wavg,
           # Let dist = 1 m in case dist = 0
           dist = ifelse(!dist, 1, dist), 
           wgt = ifelse(wavg, dist^-idw_pwr, 1))
  
  # Nearest neighbor within radius (average if multiple minima)
  dat_agg_i[[1]] <- dat_matched1 %>% 
    group_by(id_epa, date) %>% 
    slice_min(order_by = dist) %>% 
    summarize(across(c(height, pressure, hour_along), mean, na.rm = TRUE)) %>% 
    rename_with(~ sprintf("%s_nn_%s_%s", ., height.combo, cutoff), .cols = c(-id_epa, -date))
  
  # IDW average within radius
  dat_agg_i[[2]] <- dat_matched1 %>% 
    group_by(id_epa, date) %>% 
    summarize(n = n(),
              across(c(height, pressure, hour_along), wtd.var, 
                     weights = wgt, normwt = TRUE, .names = "var_{.col}"),
              across(c(height, pressure, hour_along), weighted.mean, w = wgt, na.rm = TRUE)) %>% 
    mutate(across(starts_with("var_"), ~ ifelse(is.na(.), 0, .))) %>% 
    rename_with(~ sprintf("%s_idw_%s_%s", ., height.combo, cutoff), .cols = c(-id_epa, -date))
  
  # IDW average up to k nearest neighbors within radius
  dat_agg_i[[3]] <- dat_matched1 %>% 
    group_by(id_epa, date) %>% 
    slice_min(order_by = dist, n = k_nn) %>% 
    summarize(across(c(height, pressure, hour_along), wtd.var, 
                     weights = wgt, normwt = TRUE, .names = "var_{.col}"),
              across(c(height, pressure, hour_along), weighted.mean, w = wgt, na.rm = TRUE)) %>% 
    mutate(across(starts_with("var_"), ~ ifelse(is.na(.), 0, .))) %>% 
    rename_with(~ sprintf("%s_k_%s_%s", ., height.combo, cutoff), .cols = c(-id_epa, -date))
  
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
    rename_with(~ sprintf("%s_%s_%s_%s", ., c("min", "max"), height.combo, cutoff), .cols = c(-id_epa, -date))
  
  # Return list of aggregated data frames
  return(dat_agg_i)
}
print_time(start_time)
beep_alert()
stopImplicitCluster()

# Consolidate to one data frame
dat_agg <- c(list(dat_agg0), unlist(unlist(dat_agg, recursive = FALSE), recursive = FALSE)) %>% 
  reduce(full_join, by = c("id_epa", "date"))

# Save data frame
saveRDS(dat_agg, paste0(path_dropbox, "hms_hysplit/trajectories/trajectories_72hr_CA_2020_overlap_48hr_EPA_aggregated.rds"))
