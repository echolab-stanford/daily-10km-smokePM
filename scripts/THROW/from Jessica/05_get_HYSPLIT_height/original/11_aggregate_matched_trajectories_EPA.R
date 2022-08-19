source("work/get_HYSPLIT_height/00_utils.R")

library(Hmisc)
library(dplyr)
library(purrr)
library(foreach)
library(doParallel)

num_cores <- 6

#-------------------------------------------------------------------------------
# Aggregate HYSPLIT Trajectory Points Matched to EPA Station-Days
# Written by Jessica
# Last edited June 2021
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
dat_matched <- readRDS(paste0(path_dropbox, "hms_hysplit/trajectories/trajectories_EPA_CA_2020_matched.rds")) %>% 
  # Create a unique identifier for each EPA station-day and PM2.5 combination
  # EPA says that a station-day can have multiple PM2.5 values, so we preserve
  # multiplicity of PM2.5 values per station-day
  mutate(id_epa_pm = as.integer(as.factor(paste(id_epa, date, pm25))))

#-------------------------------------------------------------------------------
# Set up data frame
dat_agg0 <- dat_matched %>% 
  select(id_epa, lon_epa, lat_epa, date, id_epa_pm, pm25, 
         county, county_code, cbsa_code, cbsa_name) %>% 
  distinct()

lc <- length(cutoffs)
registerDoParallel(ifelse(lc < num_cores, lc, num_cores))
start_time <- get_start_time()
dat_agg <- foreach (i = 1:lc) %dopar% {
  dat_agg_i <- vector("list", 4)
  cutoff <- cutoffs[i]
  dat_matched1 <- dat_matched %>% 
    filter(dist <= cutoff) %>% 
    mutate(wavg = wavg,
           # Let dist = 1 m in case dist = 0
           dist = ifelse(!dist, 1, dist), 
           wgt = ifelse(wavg, dist^-idw_pwr, 1))
  
  # Nearest neighbor within radius (average if multiple minima)
  dat_agg_i[[1]] <- dat_matched1 %>% 
    group_by(id_epa_pm) %>% 
    slice_min(order_by = dist) %>% 
    summarize(across(c(height, pressure, hour_along), mean, na.rm = TRUE)) %>% 
    rename_with(~ paste0(., "_nn_", cutoff), .cols = -id_epa_pm)
  
  # IDW average within radius
  dat_agg_i[[2]] <- dat_matched1 %>% 
    group_by(id_epa_pm) %>% 
    summarize(n = n(),
              across(c(height, pressure, hour_along), wtd.var, 
                     weights = wgt, normwt = TRUE, .names = "var_{.col}"),
              across(c(height, pressure, hour_along), weighted.mean, w = wgt, na.rm = TRUE)) %>% 
    mutate(across(starts_with("var_"), ~ ifelse(is.na(.), 0, .))) %>% 
    rename_with(~ paste0(., "_idw_", cutoff), .cols = -id_epa_pm)
  
  # IDW average up to k nearest neighbors within radius
  dat_agg_i[[3]] <- dat_matched1 %>% 
    group_by(id_epa_pm) %>% 
    slice_min(order_by = dist, n = k_nn) %>% 
    summarize(across(c(height, pressure, hour_along), wtd.var, 
                     weights = wgt, normwt = TRUE, .names = "var_{.col}"),
              across(c(height, pressure, hour_along), weighted.mean, w = wgt, na.rm = TRUE)) %>% 
    mutate(across(starts_with("var_"), ~ ifelse(is.na(.), 0, .))) %>% 
    rename_with(~ paste0(., "_k_", cutoff), .cols = -id_epa_pm)
  
  # IDW average of trajectory extrema within radius
  dat_agg_i[[4]] <- dat_matched1 %>% 
    group_by(id_epa_pm, id_traj) %>% 
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
    rename_with(~ paste0(., "_", c("min", "max"), "_", cutoff), .cols = -id_epa_pm)
  
  # Return list of aggregated data frames
  dat_agg_i
}
print_time(start_time)
beep_alert()
stopImplicitCluster()

# Consolidate to one data frame
dat_agg <- c(list(dat_agg0), unlist(dat_agg, recursive = FALSE)) %>% 
  reduce(full_join, by = "id_epa_pm")

# Save data frame
saveRDS(dat_agg, paste0(path_dropbox, "hms_hysplit/trajectories/trajectories_EPA_CA_2020_aggregated.rds"))
