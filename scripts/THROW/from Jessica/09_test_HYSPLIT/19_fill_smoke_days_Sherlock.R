# Timers
print_time <- function(start, unit = "auto", message = "Time elapsed:") {
  end <- Sys.time()
  d <- difftime(time1 = end, time2 = start, units = unit)
  t <- round(d, digits = 1)
  u <- units(d)
  
  print(paste("Start time:", start))
  print(paste("End time:", end))
  message <- paste(message, t, u)
  print(message)
  return(d)
}

get_start_time <- function(message = "Time started:") {
  t <- Sys.time()
  print(paste(message, t))
  return(t)
}

path_group_home = paste0(Sys.getenv("GROUP_HOME"), "/")
path_data = paste0(path_group_home, "smoke_PM_prediction/predict_AOD/data/")

library(dplyr)
library(purrr)
library(lubridate)
library(stringr)
library(foreach)
library(doParallel)

num_cores = as.integer(Sys.getenv("SLURM_CPUS_PER_TASK")) - 1

#-------------------------------------------------------------------------------
# Fill in Smoke Days
# Written by Jessica
# Last edited December 2021
# 
# Add smoke days based on trajectory point counts where cloudy. Fill in missing 
# smoke dates based on temporal nearest neighboring non-missing smoke dates.
#-------------------------------------------------------------------------------
# Set thresholds
min_num_traj_points = as.integer(commandArgs(trailingOnly = T)) # minimum traj point count if HYSPLIT points
min_perc_aod_missing = 0.75

# Get time period and trajectory duration
duration_days = 6
start_date = "20060101"
end_date = "20201231"
all_dates = seq.Date(ymd(start_date), ymd(end_date), by = "day")
all_dates_str = format(all_dates, "%Y%m%d")
year_months = unique(format(all_dates, "%Y_%m"))

aod_missing_files = list.files(paste0(path_data, "2_from_EE/maiac_AODmissings/"))
aod_missing_years = sapply(aod_missing_files, function(x) substr(strsplit(x, "_")[[1]][6], 1, 4))

registerDoParallel(num_cores)
start_time = get_start_time()
filled_smoke_days = foreach(m = 1:length(year_months)) %dopar% {
  year_month = year_months[m]
  y_str = substr(year_month, 1, 4)
  m_str = substr(year_month, 6, 7)
  dates_m = grep(paste0("^", y_str, m_str), all_dates_str, value = T)
  prev_m_str = as.integer(m_str) - 1
  prev_m_str = ifelse(prev_m_str == 0, "12", str_pad(prev_m_str, 2, "left", 0))
  prev_y_str = ifelse(prev_m_str == "12", as.character(as.integer(y_str) - 1), y_str)
  prev_ym_str = paste0(prev_y_str, "_", prev_m_str)
  prev_dates_m = grep(paste0("^", prev_y_str, prev_m_str), all_dates_str, value = T)
  foll_m_str = as.integer(m_str) + 1
  foll_m_str = ifelse(foll_m_str == 13, "01", str_pad(foll_m_str, 2, "left", 0))
  foll_y_str = ifelse(foll_m_str == "01", as.character(as.integer(y_str) + 1), y_str)
  foll_ym_str = paste0(foll_y_str, "_", foll_m_str)
  foll_dates_m = grep(paste0("^", foll_y_str, foll_m_str), all_dates_str, value = T)
  
  # Get trajectory-based smoke days
  traj = paste0(path_data, "HYSPLIT_trajectories/grid_trajectory_points_", year_month, ".rds")
  traj = readRDS(traj) %>% 
    mutate(smoke_day_traj = ifelse(num_traj_points_height_1 >= min_num_traj_points, 1, 0)) %>% 
    select(grid_id_10km = id_grid, date, smoke_day_traj)
  
  # Get cloudy indicator
  aod_missing = aod_missing_files[which(aod_missing_years == y_str)]
  aod_missing = paste0(path_data, "2_from_EE/maiac_AODmissings/", aod_missing)
  aod_missing = aod_missing %>% 
    map_dfr(read.csv, colClasses = c(start_date = "character")) %>% 
    select(grid_id_10km = ID, date = start_date, perc_aod_missing = mean) %>% 
    filter(date %in% dates_m) %>% 
    mutate(cloudy = perc_aod_missing > min_perc_aod_missing)
  
  # Smoke days from plumes
  smoke_days = paste0(path_data, "smoke_days/grid_smoke_day_", year_month, ".rds")
  smoke_days = readRDS(smoke_days) %>% 
    mutate(date = format(date, "%Y%m%d")) %>% 
    select(grid_id_10km = id_grid, date, smoke_day, note_smoke_date_not_online)
  
  # Read in adjacent months if necessary to fill missing dates
  has_missing_date = anyNA(smoke_days$smoke_day)
  if (has_missing_date) {
    if (min(dates_m) != start_date) {
      prev_smoke_days = paste0(path_data, "smoke_days/grid_smoke_day_", prev_ym_str, ".rds")
      prev_smoke_days = readRDS(prev_smoke_days) %>% 
        mutate(date = format(date, "%Y%m%d")) %>% 
        select(grid_id_10km = id_grid, date, smoke_day, note_smoke_date_not_online)
      smoke_days = bind_rows(smoke_days, prev_smoke_days)
      
      prev_traj = paste0(path_data, "HYSPLIT_trajectories/grid_trajectory_points_", prev_ym_str, ".rds")
      prev_traj = readRDS(prev_traj) %>% 
        mutate(smoke_day_traj = ifelse(num_traj_points_height_1 >= min_num_traj_points, 1, 0)) %>% 
        select(grid_id_10km = id_grid, date, smoke_day_traj)
      traj = bind_rows(traj, prev_traj)
      
      prev_aod_missing = aod_missing_files[which(aod_missing_years == prev_y_str)]
      prev_aod_missing = paste0(path_data, "2_from_EE/maiac_AODmissings/", prev_aod_missing)
      prev_aod_missing = prev_aod_missing %>% 
        map_dfr(read.csv, colClasses = c(start_date = "character")) %>% 
        select(grid_id_10km = ID, date = start_date, perc_aod_missing = mean) %>% 
        filter(date %in% prev_dates_m) %>% 
        mutate(cloudy = perc_aod_missing > min_perc_aod_missing)
      aod_missing = bind_rows(aod_missing, prev_aod_missing)
    }
    if (max(dates_m) != end_date) {
      foll_smoke_days = paste0(path_data, "smoke_days/grid_smoke_day_", foll_ym_str, ".rds")
      foll_smoke_days = readRDS(foll_smoke_days) %>% 
        mutate(date = format(date, "%Y%m%d")) %>% 
        select(grid_id_10km = id_grid, date, smoke_day, note_smoke_date_not_online)
      smoke_days = bind_rows(smoke_days, foll_smoke_days)
      
      foll_traj = paste0(path_data, "HYSPLIT_trajectories/grid_trajectory_points_", foll_ym_str, ".rds")
      foll_traj = readRDS(foll_traj) %>% 
        mutate(smoke_day_traj = ifelse(num_traj_points_height_1 >= min_num_traj_points, 1, 0)) %>% 
        select(grid_id_10km = id_grid, date, smoke_day_traj)
      traj = bind_rows(traj, foll_traj)
      
      foll_aod_missing = aod_missing_files[which(aod_missing_years == foll_y_str)]
      foll_aod_missing = paste0(path_data, "2_from_EE/maiac_AODmissings/", foll_aod_missing)
      foll_aod_missing = foll_aod_missing %>% 
        map_dfr(read.csv, colClasses = c(start_date = "character")) %>% 
        select(grid_id_10km = ID, date = start_date, perc_aod_missing = mean) %>% 
        filter(date %in% foll_dates_m) %>% 
        mutate(cloudy = perc_aod_missing > min_perc_aod_missing)
      aod_missing = bind_rows(aod_missing, foll_aod_missing)
    }
  }
  
  # Fill in trajectory-based smoke days
  smoke_days = list(smoke_days, traj, aod_missing) %>% 
    reduce(left_join, by = c("grid_id_10km", "date")) %>% 
    mutate(date = ymd(date),
           smoke_day = ifelse(
             (!smoke_day | is.na(smoke_day)) & 
               (ifelse(is.na(smoke_day_traj), F, smoke_day_traj)) &
               cloudy, 1, smoke_day)) %>% 
    select(grid_id_10km, date, smoke_day, note_smoke_date_not_online)
  
  # Fill in remaining missing smoke dates
  if (has_missing_date) {
    # Smoke dates not online do not include dates at beginning and end of time period
    # so no need to address how to fill those dates for now
    smoke_days = smoke_days %>% 
      arrange(grid_id_10km, date) %>% 
      group_by(grid_id_10km) %>% 
      mutate(smoke_day_lag1 = lag(smoke_day, 1),
             smoke_day_lag2 = lag(smoke_day, 2),
             smoke_day_lead1 = lead(smoke_day, 1),
             smoke_day_lead2 = lead(smoke_day, 2)) %>% 
      ungroup() %>% 
      filter(year(date) == as.integer(y_str),
             month(date) == as.integer(m_str)) %>% 
      mutate(smoke_day = ifelse(
        note_smoke_date_not_online & is.na(smoke_day), 
        ifelse(
          !is.na(smoke_day_lag1) & !is.na(smoke_day_lead1),
          as.integer((smoke_day_lag1 == 1) & (smoke_day_lead1 == 1)),
          # Smoke day cells can only be NA due to smoke date not online
          # Longest string of consecutive smoke dates not online is length 2
          # so extending 2 days out ensures we have at least 1 lag and at least 1 lead
          as.integer(
            ifelse(is.na(smoke_day_lag1), T, smoke_day_lag1 == 1) &
              ifelse(is.na(smoke_day_lead1), T, smoke_day_lead1 == 1) &
              (smoke_day_lag2 == 1) & 
              (smoke_day_lead2 == 1))),
        smoke_day)) %>% 
      select(-smoke_day_lag1, -smoke_day_lag2, -smoke_day_lead1, -smoke_day_lead2)
  }
  
  # Filter to (plume-based, trajectory-based, and temporal NN-filled) smoke day grid cells
  smoke_days = smoke_days %>% filter(smoke_day == 1)
  return(smoke_days)
}
print_time(start_time)
stopImplicitCluster()

filled_smoke_days = bind_rows(filled_smoke_days) %>% arrange(date, grid_id_10km)

saveRDS(filled_smoke_days, paste0(path_data, "3_intermediate/all_smoke_days_incl_cloudy.rds"))

#-------------------------------------------------------------------------------
# How many additional grid cell-smoke days do we get from cloudy trajectory-based
# smoke days undetected by plumes?
filled_smoke_days_wo_cloudy = readRDS(paste0(path_data, "3_intermediate/all_smoke_days.rds"))
print(nrow(setdiff(filled_smoke_days, filled_smoke_days_wo_cloudy)))
