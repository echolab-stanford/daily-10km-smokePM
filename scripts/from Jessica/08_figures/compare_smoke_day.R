path_dropbox = "~/BurkeLab Dropbox/Data/"
path_project = "~/BurkeLab Dropbox/Projects/smoke_PM_prediction/"

library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(ggplot2)
library(sp)
library(rgeos)
library(sf)
library(tigris)

chosen_date = "20181111"
chosen_year = substr(chosen_date, 1, 4)
chosen_month = substr(chosen_date, 5, 6)
chosen_day = substr(chosen_date, 7, 8)

# Get smoke plume shapes
smoke = readRDS(paste0(path_dropbox, "smoke/smoke_plumes_sfdf.rds"))
smoke = smoke %>% filter(date == chosen_date)

# Load 10 km grid
project_grid = readRDS("~/Documents/GitHub/purple-air-infiltration/data/grid.RDS")
project_grid = gBuffer(project_grid, byid = T, width = 5000, capStyle = "SQUARE")
project_grid = st_as_sf(project_grid) %>% st_transform(st_crs(smoke)) %>% select(id_grid = ID)

# Get gridded smoke day
smoke_day = readRDS(paste0(path_dropbox, "smoke/10km_grid/grid_smoke_day_", chosen_year, "_", chosen_month, ".rds"))
smoke_day = smoke_day %>% 
  filter(date == ymd(chosen_date)) %>% 
  mutate(smoke_day = factor(smoke_day, levels = 0:1))
smoke_day = project_grid %>% left_join(smoke_day, by = "id_grid")

# Get filled smoke day grid
filled_smoke_day = readRDS(paste0(path_project, "data/3_intermediate/all_smoke_days_incl_cloudy.rds"))
filled_smoke_day = filled_smoke_day %>% 
  filter(date == ymd(chosen_date)) %>% 
  select(id_grid = grid_id_10km, date, smoke_day)
filled_smoke_day = project_grid %>% 
  left_join(filled_smoke_day, by = "id_grid") %>% 
  replace_na(list(date = ymd(chosen_date), smoke_day = 0)) %>% 
  mutate(smoke_day = factor(smoke_day, levels = 0:1))

# Compare smoke day
diff_smoke_day = full_join(st_drop_geometry(smoke_day), 
                           st_drop_geometry(filled_smoke_day), 
                           by = c("id_grid", "date")) %>% 
  select(id_grid, date, smoke_day_plume = smoke_day.x, smoke_day_plume_traj_aod = smoke_day.y)
