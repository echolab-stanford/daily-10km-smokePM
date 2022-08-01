library(dplyr)
library(stringr)
library(lubridate)
library(foreach)
library(doParallel)
library(splitr)
library(ff)

#-------------------------------------------------------------------------------
# Parallelize splitr
# Written by Jessica
# Last edited August 2021
# 
# Define function for running splitr in parallel to generate HYSPLIT trajectories
# more efficiently:
#    Create meteorology directory if does not exist.
#    Create working directory for each trajectory. Prevents mix-up of intermediate
#       files that would render output inaccurate.
#    Feed HYSPLIT starting points as irregular parameters.
#    Generate HYSPLIT trajectories using multiple cores simultaneously.
#    Gather generated trajectories in one output directory.
#    Delete working directories when done generating all trajectories.
# 
# Issues remaining:
#    Specify minutes as a parameter.
#       Most likely need to fork splitr and modify source code.
#-------------------------------------------------------------------------------
# Define function for running splitr in parallel
hysplit_trajectory_parallel <- 
  function(out_dir = NULL, exec_rm = FALSE,
           lat = 49.263, lon = -123.25, height = 50, duration = 24, days = NULL, 
           daily_hours = 0, direction = "forward", met_type = "reanalysis", 
           vert_motion= 0, model_height = 20000, extended_met = FALSE, 
           config = NULL, ascdata = NULL, traj_name = NULL, binary_path = NULL,
           met_dir = NULL, exec_dir = NULL) {
    
    # Create meteorology directory
    if (!dir.exists(met_dir)) dir.create(met_dir)
    
    # Create a working directory
    if (!dir.exists(exec_dir)) dir.create(exec_dir)
    nc <- nchar(exec_dir)
    if (substr(exec_dir, nc, nc) != "/") exec_dir <- paste0(exec_dir, "/")
    
    # Create output directory
    if (!dir.exists(out_dir)) dir.create(out_dir)
    nc <- nchar(out_dir)
    if (substr(out_dir, nc, nc) != "/") out_dir <- paste0(out_dir, "/")
    
    # Create folder per start date-time
    path_raw <- paste0(out_dir, "raw/")
    path_rds <- paste0(out_dir, "rds/")
    if (!dir.exists(path_raw)) dir.create(path_raw)
    if (!dir.exists(path_rds)) dir.create(path_rds)
    
    yyyy <- days %>% substr(1, 4) %>% unique()
    mm <- days %>% substr(1, 7) %>% unique()
    dd <- days %>% unique()
    hh <- paste(days, str_pad(daily_hours, 2, "left", 0), sep = "-") %>% unique()
    
    for (y in yyyy) {
      paste0(path_raw, y) %>% dir.create()
      paste0(path_rds, y) %>% dir.create()
      for (m in mm) {
        if (y == substr(m, 1, 4)) {
          paste0(path_raw, y, "/", m) %>% dir.create()
          paste0(path_rds, y, "/", m) %>% dir.create()
          for (d in dd) {
            if (m == substr(d, 1, 7)) {
              sprintf("%s%s/%s/%s", path_raw, y, m, d) %>% dir.create()
              sprintf("%s%s/%s/%s", path_rds, y, m, d) %>% dir.create()
              for (h in hh) {
                if (d == substr(h, 1, 10)) {
                  sprintf("%s%s/%s/%s/%s", path_raw, y, m, d, h) %>% dir.create()
                  sprintf("%s%s/%s/%s/%s", path_rds, y, m, d, h) %>% dir.create()
                }
              }
            }
          }
        }
      }
    }
    
    # List trajectory end date-times that would cut short
    # Not robust to different start date-time & duration combinations
    # years <- days %>% substr(1, 4) %>% unique()
    # leap <- leap_year(years %>% as.integer())
    # leap_years <- years[which(leap)]
    # common_years <- years[which(!leap)]
    #     cut_short <- bind_rows(
    #   expand.grid(1:12, c(7, 14, 21, 28)),
    #   expand.grid(2, 29),
    #   expand.grid(c(4, 6, 9, 11), 30),
    #   expand.grid(c(1, 3, 5, 7, 8, 10, 12), 31)
    # ) %>% 
    #   rename(month = Var1, day = Var2) %>% 
    #   mutate(across(everything(), str_pad, 2, "left", 0)) %>% 
    #   unite("md", month, day, sep = "-") %>% 
    #   pull(md)
    #     cut_short <- bind_rows(
    #   expand.grid(leap_years, cut_short),
    #   expand.grid(common_years, setdiff(cut_short, "02-29"))
    # ) %>% 
    #   rename(year = Var1, md = Var2) %>% 
    #   unite(ymd, year, md, sep = "-")
    
    foreach(i = 1:length(lat)) %dopar% {
      # Create working subdirectory
      exec_dir_i <- paste0(exec_dir, i, "/")
      dir.create(exec_dir_i)
      
      # Extend duration if needed
      # duration_i <- duration
      # if (met_type == "gdas1") {
        # dt_start <- ymd_h(paste(days[i], daily_hours[i]), tz = "UTC")
        # dt_end <- dt_start + hours(duration)
      #   extend <- cut_short %>% 
      #     sapply(function(dt1_dt2, dt_i) return(dt_i %within% dt1_dt2), dt_end) %>% 
      #     which()
      #   if (length(extend) > 0) {
      #     correction <- cut_short[[extend]] %>% 
      #       int_end() %>% 
      #       difftime(dt_end, units = "hours") %>% 
      #       as.integer()
      #     duration_i <- duration + correction + 1
      #   }
      # }
      
      # Generate HYSPLIT trajectory
      traj <- hysplit_trajectory(
        lat = lat[i], 
        lon = lon[i], 
        height = height[i], 
        duration = duration, 
        day = days[i],
        daily_hours = daily_hours[i], 
        direction = direction, 
        met_type = met_type, 
        vert_motion = vert_motion, 
        model_height = model_height, 
        extended_met = extended_met, 
        config = config, 
        ascdata = ascdata, 
        traj_name = if (is.null(traj_name)) "traj" else traj_name,
        binary_path = binary_path, 
        met_dir = met_dir, 
        exec_dir = exec_dir_i, 
        clean_up = FALSE
      )
      
      # If cut short
      if (max(traj$hour_along) < duration) {
        # Re-create temporary working subdirectory
        unlink(exec_dir_i, recursive = TRUE)
        dir.create(exec_dir_i)
        
        # Extend duration based on end date-time
        extension <- 24
        dt_start <- ymd_h(paste(days[i], daily_hours[i]), tz = "UTC")
        dt_end <- dt_start + hours(duration)
        leap <- year(dt_end) %>% leap_year()
        feb <- month(dt_end) == 2
        feb28 <- (day(dt_end) == 28) & (hour(dt_end) %in% 22:23)
        feb29 <- (day(dt_end) == 29) & (hour(dt_end) %in% 0:21)
        if (leap & feb & (feb28 | feb29)) extension <- 48
        
        # Re-generate trajectory with duration extended
        traj <- hysplit_trajectory(
          lat = lat[i], 
          lon = lon[i], 
          height = height[i], 
          duration = duration + extension, 
          day = days[i],
          daily_hours = daily_hours[i], 
          direction = direction, 
          met_type = met_type, 
          vert_motion = vert_motion, 
          model_height = model_height, 
          extended_met = extended_met, 
          config = config, 
          ascdata = ascdata, 
          traj_name = if (is.null(traj_name)) "traj" else traj_name,
          binary_path = binary_path, 
          met_dir = met_dir, 
          exec_dir = exec_dir_i, 
          clean_up = FALSE
        )
      }
      # File by start date-time
      file_year <- traj$traj_dt_i[1] %>% year()
      file_month <- traj$traj_dt_i[1] %>% month() %>% str_pad(2, "left", 0)
      file_day <- traj$traj_dt_i[1] %>% day() %>% str_pad(2, "left", 0)
      file_hour <- traj$traj_dt_i[1] %>% hour() %>% str_pad(2, "left", 0)
      path_dt <- file_year %>% 
        paste(paste(file_year, file_month, sep = "-"), sep = "/") %>% 
        paste(paste(file_year, file_month, file_day, sep = "-"), sep = "/") %>% 
        paste(paste(file_year, file_month, file_day, file_hour, sep = "-"), sep = "/") %>% 
        paste0("/")
      
      # Move trajectory file to output directory
      file_name <- paste0(exec_dir_i, "1") %>% 
        list.files(full.names = TRUE, recursive = TRUE)
      file.move(file_name, paste0(path_raw, path_dt))
      
      # Save RDS version
      saveRDS(traj, paste0(path_rds, path_dt, basename(file_name), ".rds"))
      
      # Discard temporary working subdirectory
      unlink(exec_dir_i, recursive = TRUE)
   
      # Return NULL to main process
      return(NULL)
    }
    # Discard working directory
    if (exec_rm) unlink(exec_dir, recursive = TRUE)
  }
