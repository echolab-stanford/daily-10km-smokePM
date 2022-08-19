source("work/05_get_HYSPLIT_height/00_utils.R")
path_github <- "~/Documents/GitHub/purple-air-infiltration/" # delete once move repos

library(data.table)
library(FNN)
library(sp)
library(rgeos)
library(raster)
library(dplyr)
library(lubridate)

#-------------------------------------------------------------------------------
# Get Distance to Nearest Fire Cluster Over Daily Grid
# Modified from Anne's code by Jessica
# Last edited October 2021
# 
# Get distance (km) from each grid cell centroid to nearest fire cluster centroid
# each day for each month each year. Get area and number of fire points composing
# each fire cluster over daily grid as well (for details, see 
# ~/BurkeLab Dropbox/Projects/data_maintence/fire/get_fire_clusters.R)
#-------------------------------------------------------------------------------
# Read in project grid
project_grid <- readRDS(paste0(path_github, "data/grid.RDS"))
project_grid <- gBuffer(project_grid, byid = T, width = 5000, capStyle = "SQUARE")

# Set cutoff for minimum fire cluster size
files <- list.files(file.path(path_dropbox, "fire"), pattern = "^clusters", full.names = TRUE)
stopifnot(length(files) <= year(now()) - 2006 + 1)
fire_clusters <- readRDS(files[1])
min_size <- min(fire_clusters$area, na.rm = T) * 3

# Dates that are empty files
all_dates = seq.Date(ymd("20060101"), ymd("20201231"), by = "day")
fire_list = readRDS(paste0(path_dropbox, "fire/hms_fires.RDS"))
dates_empty_data = names(fire_list)[which(sapply(fire_list, nrow) == 0)]
dates_empty_data = as.Date(dates_empty_data, format = "%Y%m%d")
dates_empty_data
saveRDS(dates_empty_data, paste0(path_dropbox, "fire/fire_dates_empty_data.rds"))

# Dates not online
dates_not_online = setdiff(format(all_dates, "%Y%m%d"), names(fire_list))
dates_not_online = as.Date(dates_not_online, format = "%Y%m%d")
dates_not_online
saveRDS(dates_not_online, paste0(path_dropbox, "fire/fire_dates_not_online.rds"))

for (file in files) {
  # Read in fire clusters
  fire_clusters <- readRDS(file)
  
  # Format as date
  fire_clusters$date <- ymd(fire_clusters$date)
  dates_downloaded <- unique(fire_clusters$date)
  
  # Limit to large enough fire clusters
  fire_clusters <- fire_clusters[fire_clusters$area > min_size,  ]
  dates_clusters_large_enough <- unique(fire_clusters$date)
  dates_clusters_too_small <- setdiff(dates_downloaded, dates_clusters_large_enough)
  
  # Get fire clusters into same CRS
  # crs(fire_clusters) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  fire_clusters <- spTransform(fire_clusters, crs(project_grid))
  
  year_months <- fire_clusters$date %>% format("%Y-%m") %>% unique()
  
  # Takes ~1 minute per low-fire month and ~5-15 minutes per high-fire month
  for (year_month in year_months) {
    print(paste(year_month, "---------------------------------------------------------"))
    start_time <- get_start_time()
    
    # Get year, month, and days
    y <- substr(year_month, 1, 4)
    m <- substr(year_month, 6, 7)
    dm <- days_in_month(as.numeric(m))
    if (leap_year(as.numeric(y)) & (m == "02")) dm <- dm + 1
    days <- seq.Date(ymd(paste0(year_month, "-01")), 
                     ymd(paste0(year_month, "-", dm)), 
                     "day")

    # Get distance to nearest fire, area, and number of fire points by day
    out_month <- vector("list", length(days))
    for (d in 1:length(days)) {
      # Initialize output
      fire_distance <- data.frame(id_grid = project_grid$ID,
                                  date = days[d], 
                                  km_dist = NA, 
                                  area = NA,
                                  num_points = NA, 
                                  note_fire_date_not_online = days[d] %in% dates_not_online, 
                                  note_fire_date_empty_data = days[d] %in% dates_empty_data, 
                                  # there are no fire dates that go missing due to bad geometry, 
                                  # so we do not include in grid here, but for
                                  # future updates could potentially be relevant,
                                  # in which case need to modify code to save
                                  # bad geometry dates properly
                                  note_fire_date_clusters_too_small = days[d] %in% dates_clusters_too_small)
      
      # Limit to fire clusters that day
      cur_fire_clusters <- fire_clusters[fire_clusters$date == days[d], ]
      
      if (nrow(cur_fire_clusters) > 0) {
        # Find nearest fire cluster
        knn_dist <- get.knnx(coordinates(cur_fire_clusters), 
                             coordinates(project_grid), 
                             k = 1)
        
        # Get distance converted to km
        fire_distance$km_dist <- c(knn_dist$nn.dist)/1000
        
        # Set distance to 0 for grid cells intersecting a fire cluster
        fire_intersects <- over(project_grid, cur_fire_clusters)
        fire_intersects <- which(!is.na(fire_intersects$id))
        fire_distance$km_dist[fire_intersects] <- 0
        
        # Get area and number of fire points
        fire_distance$area <- cur_fire_clusters$area[knn_dist$nn.index]
        fire_distance$num_points <- cur_fire_clusters$num_points[knn_dist$nn.index]
      }
      # Append to list for the month
      out_month[[d]] <- fire_distance
    }
    # Save
    out_month <- out_month %>% bind_rows() %>% arrange(id_grid, date)
    saveRDS(out_month, paste0(path_dropbox, "10km_grid_data/distance_to_fire_cluster/", 
                              sprintf("grid_distance_to_fire_cluster_%s_%s.rds", y, m)))
    
    print_time(start_time)
  }
}

#-------------------------------------------------------------------------------
# Get other missing fire dates
# Dates where all clusters too small
files = list.files(paste0(path_dropbox, "10km_grid_data/distance_to_fire_cluster/"), full.names = T)
dates_clusters_too_small = c()
dates_no_issues = c()
for (file in files) {
  df = readRDS(file)
  dates_clusters_too_small = c(dates_clusters_too_small,
                               df %>% filter(note_fire_date_clusters_too_small == T) %>% pull(date) %>% unique())
  dates_no_issues = c(dates_no_issues,
                      df %>% filter(note_fire_date_not_online == F,
                                    note_fire_date_empty_data == F,
                                    note_fire_date_clusters_too_small == F) %>% 
                        pull(date) %>% unique())
}
class(dates_clusters_too_small) = "Date"
class(dates_no_issues) = "Date"
dates_clusters_too_small
saveRDS(dates_clusters_too_small, paste0(path_dropbox, "fire/fire_dates_clusters_too_small.rds"))

# Dates where all empty points or too far beyond extent
files = list.files(paste0(path_dropbox, "fire/"), pattern = "^clusters", full.names = T)
dates_nonempty = c()
for (file in files) {
  df = readRDS(file)
  dates_nonempty = c(dates_nonempty, unique(df$date))
}
dates_bad_geometry = setdiff(as.Date(dates_nonempty, "%Y%m%d"), c(dates_clusters_too_small, dates_no_issues))
class(dates_bad_geometry) = "Date"
dates_bad_geometry
# saveRDS(dates_bad_geometry, paste0(path_dropbox, "fire/fire_dates_bad_geometry.rds"))
