source("work/08_figures/00_utils.R")

library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(ggplot2)
library(sp)
library(rgeos)
library(sf)
library(tigris)

#-------------------------------------------------------------------------------
# Plot smoke plumes
# Written by Jessica
# Last edited January 2022
#-------------------------------------------------------------------------------
# Get smoke plume shapes
# df = read_sf(paste0(path_dropbox, "smoke/hms_smoke", chosen_date, ".shp"))
smoke = readRDS(paste0(path_dropbox, "smoke/smoke_plumes_sfdf.rds"))
smoke = smoke %>% filter(date == chosen_date)

# Get CONUS background
usa = states()
conus = usa %>% filter(!(STUSPS %in% c("AK", "AS", "GU", "HI", "MP", "PR", "VI")))

# Plot plumes
p = ggplot() + 
  geom_sf(data = conus) +
  geom_sf(data = smoke, fill = "orange", alpha = 0.3) + 
  theme_void()

# Save
ggsave(paste0(path_github, "figures/hms_smoke", chosen_date, "_shapes.png"),
       plot = p, width = 12, height = 8)

# Load 10 km grid
project_grid = readRDS("~/Documents/GitHub/purple-air-infiltration/data/grid.RDS")
project_grid = gBuffer(project_grid, byid = T, width = 5000, capStyle = "SQUARE")
project_grid = st_as_sf(project_grid) %>% st_transform(st_crs(smoke)) %>% select(id_grid = ID)

#-------------------------------------------------------------------------------
# Get gridded smoke day
smoke_day = readRDS(paste0(path_dropbox, "smoke/10km_grid/grid_smoke_day_", chosen_year, "_", chosen_month, ".rds"))
smoke_day = smoke_day %>% 
  filter(date == ymd(chosen_date)) %>% 
  mutate(smoke_day = factor(smoke_day, levels = 0:1))
smoke_day = project_grid %>% left_join(smoke_day, by = "id_grid")

# Plot gridded smoke day
p = ggplot(smoke_day, aes(fill = smoke_day)) + 
  geom_sf(color = NA) +  
  theme_void() + 
  scale_fill_manual(values = c("gray50", "orange")) + 
  labs(fill = "Smoke Day")

# Save
ggsave(paste0(path_github, "figures/hms_smoke", chosen_date, "_gridded.png"),
       plot = p, width = 12, height = 8)

#-------------------------------------------------------------------------------
# Choose a date that got repaired
dates_repaired_geometry = readRDS(paste0(path_dropbox, "smoke/smoke_dates_repaired_geometry.rds"))
repaired_date = gsub("-", "", dates_repaired_geometry[9])

# Get incomplete linestring shape
before = read_sf(paste0(path_dropbox, "smoke/hms_smoke", repaired_date, ".shp"))
before = before %>% st_cast("LINESTRING") %>% select(geometry)

# Get repaired polygon shape
repaired = readRDS(paste0(path_dropbox, "smoke/smoke_plumes_sfdf.rds"))
repaired = repaired %>% filter(date == repaired_date) %>% select(geometry)

# Choose a plume
i = 25

# Plot line string not closed
p = ggplot(bind_rows(before[i,] %>% mutate(status = "Before Repair"),
                 repaired[i,] %>% mutate(status = "After Repair")) %>% 
         mutate(status = factor(status, levels = c("Before Repair", "After Repair")))) + 
  geom_sf() + 
  facet_wrap(vars(status)) + 
  theme_void()

# Save
ggsave(paste0(path_github, "figures/hms_smoke", repaired_date, "_repair.png"),
       plot = p, width = 10, height = 5)

#-------------------------------------------------------------------------------
# AOD Missingness
aod_na = list.files(paste0(path_project, "data/2_from_EE/maiac_AODmissings/"),
                    pattern = sprintf("^aod_pctMissing_10km_subgrid_.*_%s.*\\.csv$", chosen_year),
                    full.names = T) %>% 
  map_dfr(read.csv) %>% 
  rename(id_grid = ID, date = start_date, perc_aod_missing = mean) %>% 
  filter(date == chosen_date)
aod_na = project_grid %>% left_join(aod_na, by = "id_grid")

# Plot
p = ggplot(aod_na %>% mutate(perc_aod_missing = perc_aod_missing*100), aes(fill = perc_aod_missing)) + 
  geom_sf(color = NA) +  
  theme_void() + 
  labs(fill = "% AOD\nMissing")

# Save
ggsave(paste0(path_github, "figures/perc_AOD_missing_", chosen_date, ".png"),
       plot = p, width = 12, height = 8)

#-------------------------------------------------------------------------------
# Final Gridded Smoke Days
# Get filled smoke day grid
filled_smoke_day = readRDS(paste0(path_project, "data/3_intermediate/all_smoke_days_incl_cloudy.rds"))
filled_smoke_day = filled_smoke_day %>% 
  filter(date == ymd(chosen_date)) %>% 
  select(id_grid = grid_id_10km, date, smoke_day)
filled_smoke_day = project_grid %>% 
  left_join(filled_smoke_day, by = "id_grid") %>% 
  replace_na(list(date = ymd(chosen_date), smoke_day = 0)) %>% 
  mutate(smoke_day = factor(smoke_day, levels = 0:1))

# Plot gridded smoke day
p = ggplot(filled_smoke_day, aes(fill = smoke_day)) + 
  geom_sf(color = NA) +  
  theme_void() + 
  scale_fill_manual(values = c("gray50", "orange")) + 
  labs(fill = "Smoke Day")

# Save
ggsave(paste0(path_github, "figures/hms_smoke", chosen_date, "_filled.png"),
       plot = p, width = 12, height = 8)
