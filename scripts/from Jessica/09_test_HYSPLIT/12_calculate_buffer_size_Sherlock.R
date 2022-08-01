path_scratch = Sys.getenv("SCRATCH")
path_run = paste0(path_scratch, "/", Sys.getenv("SLURM_JOB_NAME"), "/")

#-------------------------------------------------------------------------------
# Calculate Buffer Size
# Written by Jessica
# Last edited December 2021
# 
# Get summary statistics about consecutive distance to inform decision about size
# of buffer around each 10 km grid centroid for counting trajectory points in
# each height bin.
#-------------------------------------------------------------------------------
# Combine consecutive distances
folders = list.files(path_scratch, 
                     pattern = "^prepare_trajectories_20",
                     full.names = T)
files = paste0(folders, "/data/misc/consecutive_distances.rds")
consecutive_distances_list = lapply(files, readRDS)
consecutive_distances = unlist(consecutive_distances_list)
saveRDS(consecutive_distances, paste0(path_run, "consecutive_distances.rds"))

# Calculate percentiles
consecutive_distance_percentiles = quantile(consecutive_distances, seq(0, 1, 0.01), na.rm = T)
saveRDS(consecutive_distance_percentiles, paste0(path_run, "consecutive_distance_percentiles.rds"))
hist(consecutive_distances)
file.rename(paste0(path_run, "Rplots.pdf"), paste0(path_run, "consecutive_distances.pdf"))

# Calculate max consecutive distance
max_consecutive_distance = max(consecutive_distances, na.rm = T)
saveRDS(max_consecutive_distance, paste0(path_run, "max_consecutive_distance.rds"))
