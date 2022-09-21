# ------------------------------------------------------------------------------
# Written by: Marissa Childs
# Plots supplemental figure 9.
# ------------------------------------------------------------------------------
grid <- read_sf(file.path(path_data, "1_grids", "10km_grid"))
pop <- list.files(file.path(path_data, "2_from_EE", "populationDensity_10km_subgrid"), 
                  full.names = T) %>% 
  purrr:::map_dfr(read.csv)

grid %<>% left_join(pop)

# smoke PM data 
epa_stations <- readRDS(file.path(path_data, "3_intermediate", "station_smokePM.rds")) %>% 
  pull(id) %>% 
  unique

epa_ll <- read_sf(file.path(path_data, "epa_station_locations")) %>% 
  st_transform(st_crs(grid)) %>% 
  filter(id %in% epa_stations)

grid_station_dists <- st_distance(st_centroid(grid), epa_ll, by_element = FALSE)

grid$centroid_station_dist <- apply(grid_station_dists, 1, min)

median(grid$centroid_station_dist)/1e3
weighted.mean(grid$centroid_station_dist, grid$mean)/1e3

# Plot
pdf(file.path(path_figures, "figureS09.pdf"), 
    width = 10, height = 5)
par(mar = c(5, 5, 6, 2) + 0.1)
hist(grid$centroid_station_dist/1e3, breaks = seq(0, 300, 10),
     xlab = "minimum distance (km)", ylab = "",
     main = "", las = 1)
title(ylab = "Frequency", line = 3.5)
title(main = "Distance from prediction grid centroid to EPA station", line = 4)
50 %>%
  {abline(v = ., col = "black", lwd = 3)
    text(x = ., y = 15200, label = "merra2\nresolution", col = "black", xpd = T)}

median(grid$centroid_station_dist/1e3) %>% 
  {abline(v = ., col = "red", lty = "dashed")
    text(x = .-4, y = 13800, label = "median",
         col = "red", xpd = T)}

mean(grid$centroid_station_dist/1e3) %>% 
  {abline(v = ., col = "blue")
    text(x = .+4, y = 14000, label = "mean", col = "blue", xpd = T)}

weighted.mean(grid$centroid_station_dist/1e3, grid$mean) %>% 
  {abline(v = ., col = "darkgreen")
    text(x = .-8, y = 14000, label = "pop-weighted\nmean", 
         col = "darkgreen", xpd = T)}

dev.off()
