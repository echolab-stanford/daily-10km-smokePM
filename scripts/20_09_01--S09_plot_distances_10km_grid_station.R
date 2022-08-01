path_dropbox = "~/BurkeLab Dropbox/Data/"
path_project = "~/BurkeLab Dropbox/Projects/smoke_PM_prediction/"

library(stringr)
library(lubridate)
library(sf)
library(tigris)
library(FNN)
library(dplyr)
library(ggplot2)

# Get CONUS
conus = states() %>% 
  filter(!(NAME %in% c("Alaska", 
                       "American Samoa",
                       "Guam",
                       "Hawaii",
                       "Commonwealth of the Northern Mariana Islands",
                       "Puerto Rico",
                       "United States Virgin Islands")))

# Load PM observed at stations
epa_coords = read_sf(file.path(path_dropbox, "PM25/epa_station_locations/")) %>% 
  select(epa_id = id) %>% 
  st_transform(st_crs(conus)) %>% 
  # Excludes exactly Alaska and Hawaii EPA stations and doesn't exclude any 
  # CONUS stations even if they are near water or border
  st_filter(conus)
epa = readRDS(file.path(path_dropbox, "PM25/EPA/epa_station_level_pm25_data.rds")) %>% 
  # Limit to CONUS
  filter(id %in% epa_coords$epa_id) %>% 
  mutate(date = mdy(str_pad(date, 8, "left", 0))) %>% 
  select(epa_id = id, date)

# Load smoke PM predictions
# I believe these are only smoke day grid cells
grid = readRDS(file.path(path_project, "output/smokePM_predictions_2006_2020.rds")) %>% 
  select(grid_id_10km, date)
grid_coords = read_sf(file.path(path_project, "data/1_grids/10km_grid/")) %>% 
  st_centroid() %>% 
  select(grid_id_10km = ID) %>% 
  st_transform(st_crs(epa_coords))

# Drop dates where there are no smoke PM observations at stations
choose = grid %>% filter(date %in% epa$date)

# Randomly choose 10 km grid cell-days
set.seed(123)
choose = choose[sample(1:nrow(choose), 10000),]
dates = sort(unique(choose$date))

# For each day, match chosen 10 km grid cells to the nearest neighboring station
# with observation recorded that day
out = vector("list", length(dates))
i = 1
p = txtProgressBar(max = length(dates), style = 3)
for (d in dates) {
  preds = choose %>% filter(date == d)
  preds = grid_coords %>% right_join(preds, by = "grid_id_10km")
  obs = epa %>% filter(date == d)
  obs = epa_coords %>% right_join(obs, by = "epa_id")
  
  nn = get.knnx(st_coordinates(obs),
                st_coordinates(preds),
                k = 1)
  df = bind_cols(st_drop_geometry(preds) %>% select(date, grid_id_10km),
                 st_drop_geometry(obs[nn$nn.index,]) %>% select(epa_id),
                 dist_deg = nn$nn.dist[,1])
  out[[i]] = df
  setTxtProgressBar(p, i)
  i = i + 1
}
out = bind_rows(out)
saveRDS(out, file.path(path_project, "10km_grid_station_distances.rds"))

ggplot(out) + 
  geom_histogram(aes(x = dist_deg*100))

ggplot(out) + 
  geom_histogram(aes(x = dist_deg*100)) + 
  coord_cartesian(xlim = c(0, 500))
