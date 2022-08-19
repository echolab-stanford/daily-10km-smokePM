path_data = "~/BurkeLab Dropbox/Projects/smoke_PM_prediction/data/"

library(FNN)
library(sf)
library(dplyr)
library(tidyr)

#-------------------------------------------------------------------------------
# Fill NLCD by Nearest Non-Missing Neighbor
# Written by Jessica
# Last edited February 2022
#-------------------------------------------------------------------------------
max_distance_degrees = 1

#-------------------------------------------------------------------------------
#### 1 km grid ####
# Load 1 km grid
grid_1km = read_sf(paste0(path_data, "1_grids/1km_aod_grid/"))
path_filled = paste0(path_data, "2_from_EE/NLCD_1km_subgrid_filled/")
if (!dir.exists(path_filled)) dir.create(path_filled)
files = list.files(paste0(path_data, "2_from_EE/NLCD_1km_subgrid"), full.names = T)
for (i in 1:length(files)) {
  print(i)
  file = files[i]
  nlcd = read.csv(file) %>% 
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
    mutate(across(!total & !grid_id, ~.x/total)) %>%   # calculate percentages in each landcover class
    rename(grid_id_1km = grid_id) %>% 
    select(-any_of(c("NA", "total")))
  if (!anyNA(nlcd)) next
  nlcd_filled = grid_1km %>% 
    select(grid_id_1km = grid_id) %>% 
    right_join(nlcd, by = "grid_id_1km")
  
  cell_1km_na = which(is.na(nlcd_filled$barren))
  nn = get.knnx(st_coordinates(st_centroid(nlcd_filled[-cell_1km_na,])),
                st_coordinates(st_centroid(nlcd_filled[cell_1km_na,])),
                k = 1)
  nlcd_filled[cell_1km_na, "ID_nna"] = nlcd_filled[-cell_1km_na,][nn$nn.index,]$grid_id_1km
  # meters to kilometers to degrees
  nlcd_filled[cell_1km_na, "distance_nna"] = nn$nn.dist/1000/100
  
  # Assign matched non-NA values
  nlcd_filled = nlcd_filled %>% 
    left_join(nlcd %>% select(ID_nna = grid_id_1km, barren, shrubland, herbaceous, wetlands, 
                              cultivated, water, developed, forest), 
              by = "ID_nna") %>% 
    mutate(barren = ifelse(!is.na(ID_nna) & distance_nna <= max_distance_degrees, barren.y, barren.x), 
           shrubland = ifelse(!is.na(ID_nna) & distance_nna <= max_distance_degrees, shrubland.y, shrubland.x), 
           herbaceous = ifelse(!is.na(ID_nna) & distance_nna <= max_distance_degrees, herbaceous.y, herbaceous.x), 
           wetlands = ifelse(!is.na(ID_nna) & distance_nna <= max_distance_degrees, wetlands.y, wetlands.x), 
           cultivated = ifelse(!is.na(ID_nna) & distance_nna <= max_distance_degrees, cultivated.y, cultivated.x), 
           water = ifelse(!is.na(ID_nna) & distance_nna <= max_distance_degrees, water.y, water.x), 
           developed = ifelse(!is.na(ID_nna) & distance_nna <= max_distance_degrees, developed.y, developed.x), 
           forest = ifelse(!is.na(ID_nna) & distance_nna <= max_distance_degrees, forest.y, forest.x))
  
  # Follow original format
  nlcd_filled = nlcd_filled %>% select(names(nlcd))
  
  # Save
  write.csv(nlcd_filled, 
            file = paste0(path_filled, basename(file)),
            row.names = F)
}

#-------------------------------------------------------------------------------
#### 10 km grid ####
# Load 10 km grid
grid_10km = read_sf(paste0(path_data, "1_grids/10km_grid/"))

# Load NLCD
nlcd = read.csv(paste0(path_data, "2_from_EE/NLCD_areas_10km_grid.csv")) %>% 
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
  mutate(across(!total & !grid_id, ~.x/total)) %>%   # calculate percentages in each landcover class
  rename(grid_id_10km = grid_id) %>% 
  select(-`NA`, -total)
nlcd_filled = full_join(grid_10km %>% select(grid_id_10km = ID), nlcd)

# Get ID of NN non-NA 10 km grid cell
cell_10km_na = which(is.na(nlcd_filled$barren))
nn = get.knnx(st_coordinates(st_centroid(nlcd_filled[-cell_10km_na,])),
              st_coordinates(st_centroid(nlcd_filled[cell_10km_na,])),
              k = 1)
nlcd_filled[cell_10km_na, "ID_nna"] = nlcd_filled[-cell_10km_na,][nn$nn.index,]$grid_id_10km
# meters to kilometers to degrees
nlcd_filled[cell_10km_na, "distance_nna"] = nn$nn.dist/1000/100

# Assign matched non-NA values
nlcd_filled = nlcd_filled %>% 
  left_join(nlcd %>% select(ID_nna = grid_id_10km, barren, shrubland, herbaceous, wetlands, 
                            cultivated, water, developed, forest), 
            by = "ID_nna") %>% 
  mutate(barren = ifelse(!is.na(ID_nna) & distance_nna <= max_distance_degrees, barren.y, barren.x), 
         shrubland = ifelse(!is.na(ID_nna) & distance_nna <= max_distance_degrees, shrubland.y, shrubland.x), 
         herbaceous = ifelse(!is.na(ID_nna) & distance_nna <= max_distance_degrees, herbaceous.y, herbaceous.x), 
         wetlands = ifelse(!is.na(ID_nna) & distance_nna <= max_distance_degrees, wetlands.y, wetlands.x), 
         cultivated = ifelse(!is.na(ID_nna) & distance_nna <= max_distance_degrees, cultivated.y, cultivated.x), 
         water = ifelse(!is.na(ID_nna) & distance_nna <= max_distance_degrees, water.y, water.x), 
         developed = ifelse(!is.na(ID_nna) & distance_nna <= max_distance_degrees, developed.y, developed.x), 
         forest = ifelse(!is.na(ID_nna) & distance_nna <= max_distance_degrees, forest.y, forest.x))

# Follow original format
nlcd_filled = nlcd_filled %>% select(names(nlcd))

# Save
write.csv(nlcd_filled, 
          file = paste0(path_data, "2_from_EE/NLCD_areas_10km_grid_filled.csv"),
          row.names = F)
