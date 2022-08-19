path_fire_hysplit_points = "fire_hysplit_points_date"

library(readr)
library(lubridate)
library(stringr)
library(sf)
library(dplyr)
library(tidyr)
library(purrr)
library(foreach)
library(doParallel)

num_cores = as.integer(Sys.getenv("SLURM_CPUS_PER_TASK")) - 1

fire_hysplit_points_files = list.files(path_fire_hysplit_points)

start_time = Sys.time()
registerDoParallel(num_cores)
fire_hysplit_dists = foreach(i = seq_along(fire_hysplit_points_files)) %dopar% {
  fire_hysplit_points_file = fire_hysplit_points_files[i]
  d = as.character(parse_number(fire_hysplit_points_file))
  fire_hysplit_points = readRDS(file.path(path_fire_hysplit_points, fire_hysplit_points_file))
  fire_hysplit_dist = fire_hysplit_points %>% st_distance(by_element = F)
  fire_hysplit_dist = min(fire_hysplit_dist[as.numeric(fire_hysplit_dist) > 0])
  return(fire_hysplit_dist)
}
stopImplicitCluster()
end_time = Sys.time()
end_time - start_time
saveRDS(fire_hysplit_dists, "fire_hysplit_dists.rds")

min_dist = min(unlist(fire_hysplit_dists)) #meters
saveRDS(min_dist, "min_dist.rds")
