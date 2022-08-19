library(splitr)
library(ff)

#-------------------------------------------------------------------------------
# Save Raw Trajectory Files as RDS
# Written by Jessica
# Last edited July 2021
#-------------------------------------------------------------------------------
scratch <- Sys.getenv("SCRATCH")

# Set folder where trajectory files are located
path_traj <- paste0(scratch, "/trajectories/")

# Get file names and full paths of raw trajectory files
path_raw <- paste0(path_traj, "raw/")
stopifnot(dir.exists(path_raw))
traj_names <- list.files(path_raw)
stopifnot(length(traj_names) >= 1)
traj_paths <- paste0(path_raw, traj_names)

# Copy raw trajectory files to temporary directories
tmp_dirs <- paste0(traj_paths, "_")
lapply(tmp_dirs, function(d) if (!dir.exists(d)) dir.create(d))
mapply(file.copy, traj_paths, tmp_dirs)

# Create RDS trajectory files in rds directory
path_rds <- paste0(path_traj, "rds/")
if (!dir.exists(path_rds)) dir.create(path_rds)
for (i in 1:length(tmp_dirs)) {
 tmp_dir <- tmp_dirs[i]
 traj_name <- traj_names[i]
 saveRDS(trajectory_read(tmp_dir), paste0(path_rds, traj_name, ".rds"))
}

# Discard temporary directories
lapply(tmp_dirs, unlink, recursive = TRUE)

# Save as one RDS file
rds_list <- paste0(path_rds, traj_names, ".rds") %>% lapply(readRDS)
saveRDS(rds_list, paste0(path_traj, "trajectories_72hr_CA_2020.rds"))