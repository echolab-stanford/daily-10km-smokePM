path_out = "~/Documents/send/2022-02-07/"

library(sp)
library(raster)
library(rgeos)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(leaflet)
library(rnaturalearth)

width = 12
height = 7

# ------------------------------------------------------------------------------
# Check Grid Cells with Missing Inputs (Elevation, Land Use Class, ERA5-Land)
# Written by Jessica
# Last edited February 2022
# ------------------------------------------------------------------------------
# load 10 km grid
pg = readRDS("~/Documents/GitHub/purple-air-infiltration/data/grid.RDS")
pg = gBuffer(pg, byid = T, width = 5000, capStyle = "SQUARE")
pg = st_as_sf(pg) %>% st_transform(4326)

# ------------------------------------------------------------------------------
# load land use class
nlcd = read.csv("~/BurkeLab Dropbox/Projects/smoke_PM_prediction/data/2_from_EE/NLCD_areas_10km_grid.csv") %>% 
  select(grid_id = ID, groups) %>% 
  mutate(groups = gsub("\\[|\\]", "", groups), # get rid of outer brackets
         groups = strsplit(groups, "\\}, \\{")) %>% # split on commas between brackets
  unnest(groups, keep_empty = TRUE) %>% # groups is now a list that we want to unnest (i.e. lengthen)
  mutate(groups = gsub("\\{|\\}", "", groups)) %>%  # drop the extra brackets left behind
  separate(groups, into = c("landcover", "area"), sep = ",") %>% # split in commas to get land cover class and area
  mutate(landcover = trimws(gsub("landcover=", "", landcover, fixed = TRUE)), # drop "landcover"
         area = trimws(gsub("sum=", "", area, fixed = TRUE)) %>% as.numeric, # drop "sum"
         landcover = recode(landcover, # recode the landcover variables to their classes
                            "1.0" = "water",
                            "2.0" = "developed",
                            "3.0" = "barren",
                            "4.0" = "forest",
                            "5.0" = "shrubland",
                            "7.0" = "herbaceous",
                            "8.0" = "cultivated",
                            "9.0" = "wetlands")) %>%
  pivot_wider(names_from = landcover, values_from = area, # make it wider, one row for each grid cell, filling missings with 0s because that land class wasn't in the grid cell
              values_fill = 0) %>%
  mutate(total = water + developed + barren + forest + shrubland + herbaceous + cultivated + wetlands) %>% # calculate total area for the grid cell
  mutate(across(!total & !grid_id, ~.x/total))  # calculate percentages in each landcover class

# get land use class as sf object
pg_nlcd = full_join(pg, nlcd %>% select(ID = grid_id, nlcd = barren))

# plot and color 10 km grid cells missing land use class
p_nlcd = ggplot(pg_nlcd) + geom_sf(aes(fill = is.na(nlcd)), color = NA)

# save
ggsave(plot = p_nlcd, filename = paste0(path_out, "pg_nlcd.png"), width = width, height = height)

# ------------------------------------------------------------------------------
# load elevation
elevation = read.csv("~/BurkeLab Dropbox/Projects/smoke_PM_prediction/data/2_from_EE/elevation_avg_sd_10km_grid.csv")

# get elevation as sf object
pg_elevation = full_join(pg, elevation %>% select(ID, elevation = mean))

# plot and color 10 km grid cells missing elevation
p_elevation = ggplot(pg_elevation) + geom_sf(aes(fill = is.na(elevation)), color = NA)

# save
ggsave(plot = p_elevation, filename = paste0(path_out, "pg_elevation.png"), width = width, height = height)

# ------------------------------------------------------------------------------
# load era5-land
era5l = readRDS("~/BurkeLab Dropbox/Data/ERA5/Land/2m_temperature/USA/10km_grid/UTC-0600/daily_mean_of_1-hourly/grid_2m_temperature_daily_mean_2006_01.rds") %>% 
  filter(date == min(date))

# get era5-land as sf object
pg_era5l = full_join(pg, era5l %>% select(ID = id_grid, era5l = `2m_temperature`))

# plot and color by 10 km grid cells missing era5-land
p_era5l = ggplot(pg_era5l) + geom_sf(aes(fill = is.na(era5l)), color = NA)

# save
ggsave(plot = p_era5l, filename = paste0(path_out, "pg_era5l.png"), width = width, height = height)

# ------------------------------------------------------------------------------
# get 10 km grid cells missing any of these inputs
pg_na = Reduce(full_join, list(st_drop_geometry(pg_nlcd), 
                               st_drop_geometry(pg_elevation), 
                               st_drop_geometry(pg_era5l)))
pg_na = full_join(pg, pg_na)
pg_na = pg_na %>% filter(is.na(nlcd) | is.na(elevation) | is.na(era5l))
nrow(pg_na)

# ------------------------------------------------------------------------------
# load EPA stations
epa = read_sf("~/BurkeLab Dropbox/Data/PM25/epa_station_locations/")

# check if any of those NA grid cells have epa stations in them
pg_epa = epa %>% st_filter(pg_na)
nrow(pg_epa) == 0

# plot NA grid cells and EPA stations
p_epa = ggplot() + 
  geom_sf(data = pg_na, color = "blue") + 
  geom_sf(data = epa, color = "green", size = 0.3) + 
  coord_sf(xlim = c(-125, -63), ylim = c(23, 50))

# save
ggsave(plot = p_epa, filename = paste0(path_out, "pg_epa.png"), width = width*2, height = height*2)

# ------------------------------------------------------------------------------
# load US landmass
us_land = ne_countries(scale = 10,
                       country = "United States of America",
                       returnclass = "sf")

# check if any of those NA grid cells overlap US landmass
pg_na_us_land = pg_na %>% st_filter(us_land)
nrow(pg_na_us_land)

# plot NA grid cells and US landmass
p_us_land = ggplot() + 
  geom_sf(data = us_land, fill = "green") + 
  geom_sf(data = pg_na, color = "blue", fill = NA) + 
  coord_sf(xlim = c(-125, -63), ylim = c(23, 50))

# save
ggsave(plot = p_us_land, filename = paste0(path_out, "pg_us_land.png"), width = width*2, height = height*2)

# plot only where there is overlap
p_na_us_land = ggplot() + 
  geom_sf(data = us_land, fill = "green") + 
  geom_sf(data = pg_na_us_land, color = "blue", fill = NA) + 
  coord_sf(xlim = c(-125, -63), ylim = c(23, 50))

# save
ggsave(plot = p_na_us_land, filename = paste0(path_out, "pg_na_us_land.png"), width = width*2, height = height*2)

# ------------------------------------------------------------------------------
# load population grid
population = readRDS("~/BurkeLab Dropbox/Data/10km_grid_data/grid_population.RDS")

# get population as sf object
pg_population = full_join(pg, population %>% select(ID = id, population = pop))

# check if any of those NA grid cells have population in them
# caveat: population data is from Gridded Population of the World, so may 
# include ex-US population too
pg_na_population = pg_population %>% right_join(st_drop_geometry(pg_na))
pg_na_population %>% count(population > 0)

# plot only where there is population
p_na_population = ggplot() + 
  geom_sf(data = pg_na_population %>% filter(population > 0), 
          mapping = aes(fill = population),
          color = NA)

# save
ggsave(plot = p_na_population, filename = paste0(path_out, "pg_na_population.png"), width = width, height = height)

# ------------------------------------------------------------------------------
# interactive look at the most populated places
populated_places = pg_na_population %>% 
  filter(population > quantile(population, 0.99)) %>% 
  arrange(desc(population)) %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame()
leaflet() %>%
  addTiles() %>% 
  addMarkers(lng = populated_places$X, lat = populated_places$Y)

# ------------------------------------------------------------------------------
# confirm current set of 10 km grid cells missing smoke PM predictions is the 
# set of 10 km grid cells missing NLCD or ERA5-Land

# load IDs of the current set of 10 km grid cells missing smoke PM predictions
currently_na = readRDS("~/Downloads/grid_cell_n_pred.rds") %>% 
  filter(is.na(n_date_smokePM)) %>% 
  pull(grid_id_10km)

# get IDs of the set of 10 km grid cells missing NLCD or ERA5-Land
id_na = pg_na %>% 
  filter(is.na(nlcd) | is.na(era5l)) %>% 
  pull(ID)

# compare IDs
identical(sort(currently_na), sort(id_na))
