library(dplyr)
# library(foreach)
# library(doParallel)
library(splitr)
library(ff)

#-------------------------------------------------------------------------------
# Parallelize splitr
# Written by Jessica
# Last edited May 2021
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
           met_dir = NULL, exec_dir = NULL, clean_up = FALSE) {
    
    # Create meteorology directory
    if (!dir.exists(met_dir)) dir.create(met_dir)
    
    # Create a working directory
    if (!dir.exists(exec_dir)) dir.create(exec_dir)
    nced <- nchar(exec_dir)
    if (substr(exec_dir, nced, nced) != "/") exec_dir <- paste0(exec_dir, "/")
    
    # Create output directory
    if (!dir.exists(out_dir)) dir.create(out_dir)
    
    for (i in 1:length(lat)) {
      # Create working subdirectory
      exec_dir_i <- paste0(exec_dir, i, "/")
      dir.create(exec_dir_i)
      
      # Generate HYSPLIT trajectory
      hysplit_trajectory(
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
        clean_up = clean_up
      )
      
      # Move trajectory file to output directory
      paste0(exec_dir_i, "1") %>% 
        list.files(full.names = TRUE, recursive = TRUE) %>% 
        file.move(out_dir)
      
      # Discard temporary working subdirectory
      unlink(exec_dir_i, recursive = TRUE)
      
      # Return NULL to main process
      NULL
    }
    # Discard working directory
    if (exec_rm) unlink(exec_dir, recursive = TRUE)
  }
