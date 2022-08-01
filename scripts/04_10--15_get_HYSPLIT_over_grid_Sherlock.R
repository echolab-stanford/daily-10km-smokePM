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

path_scratch = Sys.getenv("SCRATCH")
run_dir = Sys.getenv("SLURM_JOB_NAME")
path_run = paste0(path_scratch, "/", run_dir, "/")

library(rgeos)
library(sf)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(data.table)
library(ggplot2)
library(foreach)
library(doParallel)

num_cores = as.integer(Sys.getenv("SLURM_CPUS_PER_TASK")) - 1

#-------------------------------------------------------------------------------
# Get HYSPLIT Trajectories Over Daily 10 km Grid
# Written by Jessica
# Last edited December 2021
#-------------------------------------------------------------------------------
duration_days = 6

# Set time period
start_date = "20060101"
end_date = "20201231"
all_dates = seq.Date(ymd(start_date), ymd(end_date), by = "day")
all_dates_str = format(all_dates, "%Y%m%d")
year_months = unique(substr(all_dates, 1, 7))
year_months = gsub("-", "_", year_months)

# Get non-NA dates
dates_without_start_duration = format(seq.Date(ymd(start_date), ymd("20060418"), by = "day"), "%Y%m%d")
dates_not_online = readRDS(paste0(path_run, "hysplit_dates_not_online.rds"))
dates_gis_corrupt = readRDS(paste0(path_run, "hysplit_dates_gis_corrupt.rds"))
dates_oddly_empty = readRDS(paste0(path_run, "hysplit_dates_oddly_empty.rds"))
na_dates_original = c(dates_without_start_duration, dates_not_online, dates_gis_corrupt, dates_oddly_empty)
na_dates = unique(unlist(lapply(na_dates_original, function(x) seq.Date(ymd(x), ymd(x) + days(duration_days), by = "day"))))
class(na_dates) = "Date"
na_dates_str = format(na_dates, "%Y%m%d")
dates_affected = setdiff(na_dates_str, na_dates_original)
nna_dates = setdiff(all_dates, na_dates)
class(nna_dates) = "Date"
nna_dates_str = format(nna_dates, "%Y%m%d")

# Get trajectory points files
traj_points_files = list.files(paste0(path_run, "trajectory_points/"), pattern = "^trajectory_points.*\\.rds$")
traj_points_dates = gsub("trajectory_points", "", traj_points_files)
traj_points_dates = gsub("\\.rds", "", traj_points_dates)
traj_points_files = traj_points_files[which(traj_points_dates %in% nna_dates_str)]
traj_points_files = paste0(path_run, "trajectory_points/", traj_points_files)

# Plot distribution of heights at trajectory points
registerDoParallel(num_cores)
start_time = get_start_time("Getting heights:")
heights = foreach(file = traj_points_files) %dopar% readRDS(file)$height
print_time(start_time)
heights = unlist(heights)
saveRDS(heights, paste0(path_run, "misc/heights.rds"))
hist(heights, xlab = "mAGL")
file.rename("Rplots.pdf", paste0(path_run, "misc/height_hist.rds"))
saveRDS(quantile(heights, seq(0, 1, 0.01)), paste0(path_run, "misc/height_percentiles.rds"))

# Choose height bins
height_bins = quantile(heights, seq(0, 1, 0.2))
saveRDS(height_bins, paste0(path_run, "misc/height_bin_cutoffs.rds"))
height_bins = height_bins[-c(1, length(height_bins))]
empty_height_bins_df = 1:(length(height_bins) + 1) %>% 
  lapply(function(x) data.frame(0) %>% setNames(paste0("num_traj_points_height_", x))) %>% 
  bind_cols()

# Set up empty aggregates
vbls = c("height", "pressure", "hour_along")
msrs = c("mean", "median", "sd")
empty_aggregates_df = expand.grid(msr = msrs, vbl = vbls) %>% 
  unite(vbl_msr, vbl, msr) %>% 
  bind_cols(value = NA) %>% 
  pivot_wider(names_from = vbl_msr)
msrs_list = sprintf("%s = as.numeric(%s(x))", msrs, msrs)
msrs_list = paste(msrs_list, collapse = ", ")
msrs_list = sprintf("function(x) list(%s)", msrs_list)
msrs_list = parse(text = msrs_list)
msrs_list = eval(msrs_list)

# Read in project grid
project_grid = readRDS(paste0(path_run, "10km_grid_HYSPLIT_crs.rds"))

# Takes ~7 minutes per month
start_time = get_start_time("Getting trajectory point counts:")
for (year_month in year_months) {
  print(paste("Started", year_month, "at", Sys.time(), "-------------------------------------------------------------------------------"))
  dates_m = grep(paste0("^", gsub("_", "", year_month)), all_dates_str, value = T)
  traj_points_grid = foreach(d = dates_m) %dopar% {
    # Get trajectory points for the day
    traj_points_file = grep(d, traj_points_files, value = T)
    if (length(traj_points_file) > 0) {
      traj_points = readRDS(traj_points_file)
      
      # Get trajectory points over grid cells
      traj_points_grid_d = project_grid %>% 
        st_join(traj_points) %>% 
        st_drop_geometry() %>% 
        mutate(date = d)
      
      # Get count of trajectory points by height bin (cumulative)
      height_bins_df = 1:length(height_bins) %>% 
        lapply(function(x) data.frame(traj_points_grid_d$height < height_bins[x]) %>% setNames(paste0("num_traj_points_height_", x))) %>% 
        bind_cols()
      last_height_bin = ifelse(is.na(height_bins_df[length(height_bins_df)]), NA, T) %>% 
        data.frame() %>% 
        setNames(paste0("num_traj_points_height_", length(height_bins) + 1))
      height_bins_df = height_bins_df %>% bind_cols(last_height_bin)
      
      # Aggregate to the grid cell-day
      traj_points_grid_d = traj_points_grid_d %>% bind_cols(height_bins_df)
      traj_points_grid_d = as.data.table(traj_points_grid_d)
      num_traj_points_total = traj_points_grid_d[
        , .(num_traj_points_total  = sum(!is.na(height))), 
        by = .(id_grid, date)
      ] %>% as.data.frame()
      num_traj_points_height_bins = traj_points_grid_d[
        , lapply(.SD, sum, na.rm = T), 
        by = .(id_grid, date), 
        .SDcols = grep("^num_traj_points_height", names(traj_points_grid_d), value = T)
      ] %>% as.data.frame()
      vbl_msr = traj_points_grid_d[
        , unlist(lapply(.SD, msrs_list), recursive = F), 
        by = .(id_grid, date), 
        .SDcols = grep(paste0("^", paste(vbls, collapse = "|")), names(traj_points_grid_d), value = T)
      ] %>% as.data.frame()
      traj_points_grid_d = reduce(list(num_traj_points_total, num_traj_points_height_bins, vbl_msr), full_join)
      names(traj_points_grid_d) = gsub("\\.", "_", names(traj_points_grid_d))
      
      # De-cumulate trajectory point counts in height bins
      height_bins_cols = traj_points_grid_d %>% select(starts_with("num_traj_points_height"))
      for (j in length(height_bins_cols):1) {
        if (j == 1) next
        height_bins_cols[j] = height_bins_cols[j] - height_bins_cols[j - 1]
      }
      traj_points_grid_d = traj_points_grid_d %>% 
        select(-starts_with("num_traj_points_height")) %>% 
        bind_cols(height_bins_cols) %>% 
        select(id_grid, date, num_traj_points_total, starts_with(c("num_traj_points_height", vbls)))
    } else if (d %in% na_dates_str) {
      # Empty set-up when no trajectory points file for the day
      traj_points_grid_d = data.frame(id_grid = project_grid$id_grid,
                                      date = d,
                                      num_traj_points_total = 0) %>% 
        bind_cols(empty_height_bins_df) %>% 
        bind_cols(empty_aggregates_df) %>% 
        mutate(across(-c(id_grid, date), ~NA))
    }
    return(traj_points_grid_d %>%
             mutate(note_hysplit_date_without_start_duration = date %in% dates_without_start_duration,
                    note_hysplit_date_not_online = date %in% dates_not_online,
                    note_hysplit_date_gis_corrupt = date %in% dates_gis_corrupt,
                    note_hysplit_date_oddly_empty = date %in% dates_oddly_empty,
                    note_hysplit_date_affected = date %in% dates_affected))
  }
  # Save by month
  traj_points_grid = traj_points_grid %>% bind_rows() %>% arrange(id_grid, date)
  traj_points_grid_file = paste0(path_run, "10km_grid_2006-2020/grid_trajectory_points_", year_month, ".rds")
  saveRDS(traj_points_grid, traj_points_grid_file)
}
print_time(start_time)
stopImplicitCluster()
