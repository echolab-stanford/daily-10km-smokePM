# ------------------------------------------------------------------------------
# Written by: Jessica Li and Marissa Childs
# Plots supplemental figure 2.
# ------------------------------------------------------------------------------
chosen_date = "20181111"
chosen_year = year(ymd(chosen_date))
chosen_month = month(ymd(chosen_date))

# Get smoke plume shapes
smoke = readRDS(file.path(path_data, "smoke", "smoke_plumes_sfdf.rds"))
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
ggsave(file.path(path_figures, "figureS02a.png"), 
       plot = p, width = 12, height = 8)

#-------------------------------------------------------------------------------
# Load 10 km grid
project_grid = read_sf(file.path(path_data, "1_grids", "10km_grid"))
project_grid = st_as_sf(project_grid) %>% st_transform(st_crs(smoke)) %>% select(id_grid = ID)

# Get gridded smoke day
smoke_day = readRDS(file.path(path_data, "smoke_days", paste0("grid_smoke_day_", chosen_year, "_", chosen_month, ".rds")))
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
ggsave(file.path(path_figures, "figureS02b.png"), 
       plot = p, width = 12, height = 8)

#-------------------------------------------------------------------------------
# AOD Missingness
aod_na = list.files(file.path(path_data, "2_from_EE", "maiac_AODmissings"),
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
ggsave(file.path(path_figures, "figureS02d.png"), 
       plot = p, width = 12, height = 8)

#-------------------------------------------------------------------------------
hysplit <- list.files(file.path(path_data, "HYSPLIT", "10km_grid_2006-2020"),
                      pattern = paste0(format(ymd(chosen_date), "%Y"), 
                                       "_",
                                       format(ymd(chosen_date), "%m")),
                      full.names = T) %>% 
  readRDS %>% 
  mutate(date = as.Date(date, format = "%Y%m%d")) %>% 
  filter(date == ymd(chosen_date))

grid_10km <- st_read(file.path(path_data, "1_grids", "grid_10km_wgs84"))

hysplit %>% 
  select(ID = id_grid, num_traj_points_height_1) %>% 
  left_join(grid_10km %>% select(ID)) %>% 
  st_as_sf %>% 
  {ggplot(data = .) + 
      geom_sf(aes(fill = num_traj_points_height_1, color = num_traj_points_height_1)) + 
      scale_color_gradientn(colors = c(cmocean::cmocean(name = "oxy", start = 0.2, end = 0.79, direction = -1)(20),
                                       cmocean::cmocean(name = "oxy", start = 0, end = 0.19, direction = -1)(20)),
                            name = "trajectory points\nwithin 50km",
                            aesthetics = c("fill", "color"), 
                            rescaler = mid_rescaler(50),
                            guide = guide_colorbar(title.theme = element_text(size = 7),
                                                   label.theme = element_text(size = 6.5), 
                                                   barheight = 4, barwidth = 1)) +
      theme_void() + 
      theme(legend.position = c(0.93, 0.35))} %>% 
  ggsave(filename = file.path(path_figures, "figureS02e.png"), 
         ., height = 3.5, width = 5)

#-------------------------------------------------------------------------------
# Final Gridded Smoke Days
# Get filled smoke day grid
filled_smoke_day = readRDS(file.path(path_data, "3_intermediate", "all_smoke_days_incl_cloudy.rds"))
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
ggsave(file.path(path_figures, "figureS02f.png"), 
       plot = p, width = 12, height = 8)
