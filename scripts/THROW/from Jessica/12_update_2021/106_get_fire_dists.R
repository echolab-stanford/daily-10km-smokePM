path_fire_points = "fire_points_date"

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

fire_points_files = list.files(path_fire_points)

start_time = Sys.time()
registerDoParallel(num_cores)
fire_dists = foreach(i = seq_along(fire_points_files)) %dopar% {
  fire_points_file = fire_points_files[i]
  d = as.character(parse_number(fire_points_file))
  fire_points = readRDS(file.path(path_fire_points, fire_points_file))
  fire_dist = fire_points %>% st_distance(by_element = F)
  fire_dist = min(fire_dist[as.numeric(fire_dist) > 0])
  return(fire_dist)
}
stopImplicitCluster()
end_time = Sys.time()
end_time - start_time
saveRDS(fire_dists, "fire_dists.rds")

min_dist = min(unlist(fire_dists)) #meters
saveRDS(min_dist, "min_dist.rds")
