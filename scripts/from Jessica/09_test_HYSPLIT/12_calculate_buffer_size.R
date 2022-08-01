path_seagate = "/Volumes/Seagate PD JBL/"
path_results = "~/Documents/GitHub/smoke_PM_prediction/work/09_test_HYSPLIT/results/"

#-------------------------------------------------------------------------------
# Calculate Buffer Size
# Written by Jessica
# Last edited December 2021
# 
# Get summary statistics about consecutive distance to inform decision about size
# of buffer around each 10 km grid centroid for counting trajectory points in
# each height bin.
# Note: RAM insufficient on local machine. Run on Sherlock.
#-------------------------------------------------------------------------------
# Combine consecutive distances
# files = list.files(paste0(path_seagate, "HYSPLIT/miscellaneous/grid_trajectories/6-day/misc/"),
#                    pattern = "^consecutive_distances_20.*\\.rds$",
#                    full.names = T)
# consecutive_distances_list = lapply(files, readRDS)
# saveRDS(consecutive_distances_list, paste0(path_seagate, "HYSPLIT/miscellaneous/grid_trajectories/6-day/misc/consecutive_distances_list.rds"))
# consecutive_distances_list = readRDS(paste0(path_seagate, "HYSPLIT/miscellaneous/grid_trajectories/6-day/misc/consecutive_distances_list.rds"))
# consecutive_distances = unlist(consecutive_distances_list)
# saveRDS(consecutive_distances, paste0(path_seagate, "HYSPLIT/miscellaneous/grid_trajectories/6-day/misc/consecutive_distances.rds"))
consecutive_distances = readRDS(paste0(path_seagate, "HYSPLIT/miscellaneous/grid_trajectories/6-day/misc/consecutive_distances.rds"))

# Calculate summary statistics
summary(consecutive_distances)
consecutive_distance_percentiles = quantile(consecutive_distances, seq(0, 1, 0.01), na.rm = T)
saveRDS(consecutive_distance_percentiles, paste0(path_seagate, "HYSPLIT/miscellaneous/grid_trajectories/6-day/misc/consecutive_distance_percentiles.rds"))
pdf(paste0(path_results, "consecutive_distances.pdf"), width = 6, height = 6)
hist(consecutive_distances)
dev.off()

# Calculate max consecutive distance
max_consecutive_distances = unlist(lapply(consecutive_distances_list, max, na.rm = T))
max_consecutive_distance = max(max_consecutive_distances)
saveRDS(max_consecutive_distance, paste0(path_results, "max_consecutive_distance.rds"))
