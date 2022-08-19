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

path_scratch = paste0(Sys.getenv("SCRATCH"), "/")
path_group_home = paste0(Sys.getenv("GROUP_HOME"), "/")

library(dplyr)
library(purrr)
library(lubridate)
library(ggplot2)
library(foreach)
library(doParallel)

num_cores = as.integer(Sys.getenv("SLURM_CPUS_PER_TASK")) - 1
registerDoParallel(num_cores)

#-------------------------------------------------------------------------------
# Compare Smoke Day According Different Trajectory Point Counts
# Written by Jessica
# Last edited December 2021
#-------------------------------------------------------------------------------
# Set thresholds
min_num_traj_points = c(1, 2, 3, 5, 7, 10, 12, 15, 20, 24, 27, 30, 35, 40, 50, 60)
min_pm25_anom = 50 # from previous evaluations
min_aot_anom = 0.1 # 5th percentile value of anom AOT among station-days w/ anom PM > 50 and plume overhead

# Get time period and trajectory duration
duration_days = 6
start_date = "20060101"
end_date = "20201231"
all_dates = seq.Date(ymd(start_date), ymd(end_date), by = "day")
all_dates_str = format(all_dates, "%Y%m%d")
year_months = unique(format(all_dates, "%Y_%m"))

# Get dates where plume data and trajectory point counts not missing
na_dates_plume = readRDS("data/smoke_dates_not_online.rds")
na_dates_traj = c(format(seq.Date(ymd(start_date), ymd("20060418"), by = "day"), "%Y%m%d"),
                  readRDS(paste0(path_scratch, "get_HYSPLIT_over_grid/hysplit_dates_not_online.rds")),
                  readRDS(paste0(path_scratch, "get_HYSPLIT_over_grid/hysplit_dates_gis_corrupt.rds")),
                  readRDS(paste0(path_scratch, "get_HYSPLIT_over_grid/hysplit_dates_oddly_empty.rds"))) %>% 
  lapply(function(x) format(seq.Date(ymd(x), ymd(x) + days(duration_days), by = "day"), "%Y%m%d")) %>% 
  unlist() %>% 
  unique() %>% 
  sort()
na_dates = sort(unique(c(na_dates_plume, na_dates_traj)))
nna_dates = setdiff(all_dates_str, na_dates)

# Read in anomalous PM
anom_pm = readRDS("data/epa_pm25_anom_all_days.rds") %>% 
  mutate(date = format(date, "%Y%m%d")) %>% 
  filter(date %in% nna_dates)

#-------------------------------------------------------------------------------
# Do trajectories tell us the presence of smoke when plumes are undetected due 
# to interfering cloud cover?
min_perc_aod_missing = 0.75
aod_missing_files = list.files(paste0(path_group_home, "smoke_PM_prediction/predict_AOD/data/2_from_EE/maiac_AODmissings/"))
aod_missing_years = sapply(aod_missing_files, function(x) substr(strsplit(x, "_")[[1]][6], 1, 4))

start_time = get_start_time()
anom_pm_SD = foreach(year_month = year_months) %dopar% {
  y_str = substr(year_month, 1, 4)
  m_str = substr(year_month, 6, 7)
  nna_dates_m = grep(paste0("^", gsub("_", "", year_month)), nna_dates, value = T)
  if (length(nna_dates_m) == 0) return(NULL)
  
  # Subset anomalous PM for the month
  anom_pm_m = anom_pm %>% 
    filter(date %in% nna_dates_m) %>% 
    select(epa_id = id, grid_id_10km, date, pm25_anom)
  if (nrow(anom_pm_m) == 0) return(NULL)
  
  # Read in smoke plumes
  plume_m = readRDS(paste0("data/grid_smoke_day_", year_month, ".rds")) %>% 
    mutate(date = format(date, "%Y%m%d")) %>% 
    filter(date %in% nna_dates_m) %>% 
    select(grid_id_10km = id_grid, date, smoke_day_plume = smoke_day)
  if (nrow(plume_m) == 0) return(NULL)
  
  # Read in trajectory point counts
  traj_m = readRDS(paste0(path_scratch, "get_HYSPLIT_over_grid/10km_grid_2006-2020/grid_trajectory_points_", year_month, ".rds")) %>% 
    filter(date %in% nna_dates_m) %>% 
    select(grid_id_10km = id_grid, date, num_traj_points_height_1)
  if (nrow(traj_m) == 0) return(NULL)
  
  # Read in percent missing AOD
  aod_missing_m = aod_missing_files[which(aod_missing_years == y_str)]
  aod_missing_m = paste0(path_group_home, "smoke_PM_prediction/predict_AOD/data/2_from_EE/maiac_AODmissings/", aod_missing_m)
  aod_missing_m = aod_missing_m %>% 
    map_dfr(read.csv, colClasses = c(start_date = "character")) %>% 
    select(grid_id_10km = ID, date = start_date, perc_aod_missing = mean) %>% 
    filter(date %in% nna_dates_m)
  if (nrow(aod_missing_m) == 0) return(NULL)
  
  # Combine grids
  stations_m = reduce(list(anom_pm_m, plume_m, traj_m, aod_missing_m), left_join, by = c("grid_id_10km", "date"))
  
  anom_pm_notSDplume_SDtraj_cloudy = vector("list", length(min_num_traj_points))
  for (i in 1:length(min_num_traj_points)) {
    x = min_num_traj_points[i]
    anom_pm_notSDplume_SDtraj_cloudy[[i]] = stations_m %>% 
      mutate(smoke_day_traj = ifelse(num_traj_points_height_1 >= x, 1, 0)) %>% 
      filter(smoke_day_plume == 0,
             smoke_day_traj == 1,
             perc_aod_missing > min_perc_aod_missing) %>% 
      pull(pm25_anom)
  }
  names(anom_pm_notSDplume_SDtraj_cloudy) = paste0("min_num_traj_points = ", min_num_traj_points)
  
  anom_pm_SD = list(
    anom_pm_SDplume = stations_m %>% 
      filter(smoke_day_plume == 1) %>% 
      pull(pm25_anom),
    anom_pm_notSDplume_SDtraj_cloudy = anom_pm_notSDplume_SDtraj_cloudy
  )
  
  # Get counts
  return(anom_pm_SD)
}
print_time(start_time)
stopImplicitCluster()

anom_pm_SDplume = lapply(anom_pm_SD, "[[", "anom_pm_SDplume")
saveRDS(anom_pm_SDplume, "anom_pm_SDplume.rds")

anom_pm_notSDplume_SDtraj_cloudy = vector("list", length(min_num_traj_points))
for (i in 1:length(min_num_traj_points)) {
  x = min_num_traj_points[i]
  val = lapply(anom_pm_SD, function(m) {
    m[["anom_pm_notSDplume_SDtraj_cloudy"]][[paste0("min_num_traj_points = ", x)]]
  })
  saveRDS(val, paste0("anom_pm_notSDplume_SDtraj_", x, "_cloudy_", min_perc_aod_missing*100, ".rds"))
  anom_pm_notSDplume_SDtraj_cloudy[[i]] = unlist(val)
}
names(anom_pm_notSDplume_SDtraj_cloudy) = paste0("min_num_traj_points = ", min_num_traj_points)

anom_pm_SDplume = unlist(anom_pm_SDplume)
anom_pm_SDplume = data.frame(smoke_day_plume = 1,
                             anom_pm = anom_pm_SDplume)
anom_pm_notSDplume_SDtraj_cloudy = lapply(1:length(anom_pm_notSDplume_SDtraj_cloudy), function(i) {
  x = names(anom_pm_notSDplume_SDtraj_cloudy)[i]
  x = as.integer(gsub("min_num_traj_points = ", "", x))
  return(data.frame(smoke_day_plume = 0,
                    min_perc_aod_missing = min_perc_aod_missing,
                    min_num_traj_points = x,
                    anom_pm = anom_pm_notSDplume_SDtraj_cloudy[[i]]))
})
anom_pm_notSDplume_SDtraj_cloudy = bind_rows(anom_pm_notSDplume_SDtraj_cloudy)
df = anom_pm_notSDplume_SDtraj_cloudy %>% 
  group_by(min_num_traj_points) %>% 
  mutate(sample_size = n()) %>% 
  ungroup() %>% 
  mutate(min_num_traj_points.N = paste0(min_num_traj_points, "\n", "N = ", sample_size),
         min_num_traj_points.N = factor(min_num_traj_points.N, levels = unique(min_num_traj_points.N[order(min_num_traj_points)])))

# Plot distribution of anomalous PM on station-days that are smoke day according
# to trajectory point count, do not have plume overhead, and are cloudy
p = ggplot(df, aes(x = min_num_traj_points.N, y = anom_pm, group = min_num_traj_points.N)) + 
  geom_boxplot() + 
  stat_summary(fun = mean, colour = "darkred", geom = "point", show.legend = F) + 
  stat_summary(fun = mean, colour = "red", geom = "text", show.legend = F, 
               vjust = -0.7, aes(label = round(..y.., digits = 1))) + 
  coord_cartesian(ylim = c(-20, 30))
ggsave("anom_pm_SD.png", plot = p, width = 12, height = 6)
