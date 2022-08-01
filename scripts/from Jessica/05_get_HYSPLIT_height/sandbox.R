path_dropbox <- "~/BurkeLab Dropbox/Data/"
library(splitr)
library(dplyr)
library(purrr)
library(zoo)

# newtraj <- hysplit_trajectory(
#   days = c("2020-01-01","2020-02-02"),
#   met_type = "gdas1",
#   met_dir = paste0(path_dropbox, "meteorology/gdas1/"),
#   exec_dir = "/Users/jessssli/Downloads/mydir3/exec/",
#   clean_up = FALSE
# )
# rawfile <- trajectory_read("/Users/jessssli/Downloads/mydir3")

dat_traj <- paste0(path_dropbox, "hms_hysplit/trajectories/trajectories_CA_2020.rds") %>% 
  map_dfr(readRDS) %>% 
  mutate(receptor = 1)
inits <- dat_traj$hour_along == 0
dat_traj[inits, "run"] <- 1:sum(inits)
dat_traj$run <- na.locf(dat_traj$run)

set.seed(123)
rand <- sample(1:max(dat_traj$run), 100)
trajectory_plot(dat_traj %>% filter(run %in% rand))

# %>% 
#   mapply(mutate, run = 1:length(.), SIMPLIFY = FALSE) %>% 
#   map_dfr(mutate, receptor = 1)
  


# trajectory_plot(dat_traj[1:73,] %>% mutate(run=1, receptor=1))
# 
# trajectory_plot(newtraj %>% select(-lat_i, -lon_i, -height_i))
# trajectory_plot(newtraj)

























cutoffs <- c(10,30,50,80,100)
agg_methods <- c("nn", "idw", "k")
for (agg in agg_methods) for (cut in cutoffs) {
  if (agg == agg_methods[1] & cut == cutoffs[1]) print(paste("agg_method", "cutoff (km)", "N", "% rel. to 100 km", sep = "    "))
  x <- nrow(filter(dat_merged, agg_method == agg, cutoff == cut, !is.na(height)))
  print(paste(agg, cut, x, round(x/92536, 2), sep = "            "))
}

























df <- dat_matched %>% 
  filter(dist <= cutoff) %>% 
  mutate(wavg = wavg,
         # Let dist = 1 m in case dist = 0
         dist = ifelse(!dist, 1, dist), 
         wgt = ifelse(wavg, dist^-idw_pwr, 1)) %>% 
  group_by(id_epa_pm) %>% 
  summarize(n = n(),
            across(c(height, pressure, hour_along), wtd.var, 
                   weights = wgt, normwt = TRUE, .names = "var_{.col}"),
            across(c(height, pressure, hour_along), weighted.mean, w = wgt, na.rm = TRUE)) %>% 
  mutate(across(starts_with("var_"), ~ ifelse(is.na(.), 0, .))) %>% 
  rename_with(~ paste0(., "_idw_", cutoff), .cols = -id_epa_pm)


df <- data.frame(
  id1 = c(1,1,1,2,2,2),
  id2 = c(1,1,2,2,3,3),
  x = c(1,1:5),
  y = c(2,2:6),
  w = 1:6/10
)
df %>% 
  group_by(id1, id2) %>% 
  ungroup(id1, id2) %>% 
  tally()

df1 <- df %>% 
  group_by(id1, id2) %>% 
  summarize(w_x = mean(w[x == min(x)]),
            w_y = mean(w[y == max(y)]),
            x = min(x),
            y = max(y)) %>% 
  ungroup(id2) %>% 
  summarize(var_x = wtd.var(x, weights = w_x, normwt = TRUE),
            var_y = wtd.var(y, weights = w_y, normwt = TRUE),
            x = weighted.mean(x, w = w_x, na.rm = TRUE),
            y = weighted.mean(y, w = w_y, na.rm = TRUE)) %>% 
  rename_with(~ paste0(., c("min", "max")), .cols = -id1)




df <- data.frame(
  y = 1:6,
  x = 1:6,
  z = 1
)
summary(lm(y ~ x + x:z, df))



























over(dat_)
loc_traj <- SpatialPoints(dat_traj$lon, dat_traj$lat)
loc_traj <- st_as_sf(dat_traj, coords = c("lon", "lat"))
# EPSG 4326 = WGS84 geographic coordinate system
st_crs(loc_traj) <- 4326
pt1 <- loc_traj[1,]

x <- st_as_sf(dat_smoke)
pol1 <- x[1,]
st_crs(pol1) <- st_crs(pt1)

y <- st_intersects(pt1, pol1)
z <- pt1[pol1,]

pts <- st_sfc(st_point(c(-0.5, -0.6)), st_point(c(0.3, 0.2)), st_point(c(0.15, 0.1)))
pol <- st_sfc(st_polygon(list(rbind(c(0, 0), c(0, 1), c(1, 1), c(1, 0), c(0, 0)))),
              st_polygon(list(rbind(c(0, 0), c(0, -1), c(-1, -1), c(-1, 0), c(0, 0)))),
              st_polygon(list(rbind(c(0.1, 0), c(0.1, 1), c(1, 1), c(1, 0), c(0.1, 0)))))
pts[pol,]
pol[pts,]



































ggplot()+geom_sf(data=st_cast(st_cast(dat_wf$Shape[[7515]][[1]][[1]], "POLYGON"), "MULTIPOLYGON"))
st_cast(dat_wf$Shape[[7515]], "MULTIPOLYGON")
st_cast(dat_wf$Shape[[7514]], "MULTIPOLYGON")
dat_wf$Shape[[7514]]
st_cast(dat_wf$Shape, "MULTIPOLYGON")[7515,]
ggplot() + 
  # geom_sf(data = dat_wf$Shape[7515]) + 
  geom_sf(data = dat_wf$Shape[[7515]] %>% st_cast("MULTIPOLYGON"))























traj_aug_complex %>% group_by(run) %>% summarize(geometry = st_linestring(as.matrix(lon, lat, , 2)))
ggplot() + geom_sf(data = traj_aug_complex[1:73, c("lon", "lat")] %>% as.matrix() %>% st_linestring())
traj_aug_complex %>% group_by(run) %>% summarize(Shape = st_linestring(matrix(c(lon, lat), , 2)))


y <- traj_aug_complex %>% 
  # filter(run %in% 7792:7795) %>% 
  st_as_sf(coords = c("lon", "lat")) %>% 
  group_by(run) %>% 
  summarize(height_i = factor(first(height_i))) %>% 
  st_cast("LINESTRING")

ggplot(y) +
  geom_sf(aes(color = height_i))




















y <- traj_aug_complex %>% 
  mutate(height_i = factor(height_i)) %>% 
  filter(run %in% 7792:7795) %>%
  st_as_sf(coords = c("lon", "lat")) %>% 
  group_by(run) %>% 
  summarize(height_i = first(height_i)) %>% 
  st_cast("LINESTRING") %>% 
  ungroup()
st_crs(y) <- 4326
y <- y %>% st_transform(crs = st_crs(usa))

z <- traj_aug_complex %>% 
  mutate(height_i = factor(height_i)) %>% 
  filter(run %in% 7792:7795)

ggplot() +
  geom_sf(data = usa) + 
  geom_sf(data = y, mapping = aes(color = height_i))

pal <- colorFactor(
  palette = c("red", "yellow", "blue"),
  domain = z$height_i
)

popup <- 
  paste0(
    "<strong>trajectory</strong> ", z[["traj_dt_i"]],
    "<br><strong>at time</strong> ", z[["traj_dt"]],
    " (", z[["hour_along"]],
    " h)<br><strong>height</strong> ", z[["height"]],
    " <font size=\"1\">m AGL</font> / ",
    "<strong>P</strong> ", z[["pressure"]],
    " <font size=\"1\">hPa</font>"
  )

leaflet(z) %>% 
  addProviderTiles(
    provider = "OpenStreetMap",
    group = "OpenStreetMap"
  ) %>% 
  addProviderTiles(
    provider = "CartoDB.DarkMatter",
    group = "CartoDB Dark Matter"
  ) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addProviderTiles(
    provider = "Esri.WorldTerrain",
    group = "ESRI World Terrain"
  ) %>%
  addProviderTiles(
    provider = "Stamen.Toner",
    group = "Stamen Toner"
  ) %>%
  addLayersControl(
    baseGroups = c(
      "CartoDB Positron", "CartoDB Dark Matter",
      "Stamen Toner", "ESRI World Terrain"
    )) %>% 
  addPolylines(lng = z$lon, lat = z$lat, color = ~pal(height_i)) %>% 
  addCircles(lng = z$lon, lat = z$lat, color = ~pal(height_i), popup = popup)






































# Background PM2.5 is the median PM2.5 on non-smoke days at a given station 
# within the same calendar month across the year before, during, and after
pm25_median <- dat_merged
dat_epa <- dat_merged %>% 
  select(date, id_epa, lon_epa, lat_epa, pm25, smoke_day) %>% 
  distinct() %>% 
  mutate(year = substr(date, 1, 4),
         month = substr(date, 6, 7)) %>% 
  group_by(id_epa, month, year) %>% 
  summarize(pm25 = list(pm25[which(!is.na(pm25) & smoke_day == 0)])) %>% 
  rowwise() %>% 
  mutate(nobs = length(pm25)) %>% 
  ungroup() %>% 
  arrange(id_epa, month, year) %>% 
  group_by(id_epa, month) %>% 
  mutate(pm25_lead = lead(pm25, n = 1, default = list(NA)),
         pm25_lag = lag(pm25, n = 1, default = list(NA)),
         nobs_lead = lead(nobs, n = 1, default = 0),
         nobs_lag = lag(nobs, n = 1, default = 0)) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(pm25_3yr = list(c(pm25, pm25_lag, pm25_lead)),
         nobs_36 = nobs + nobs_lead + nobs_lag) %>% 
  rowwise() %>% 
  mutate(pm25_med_3yr = median(unlist(pm25_3yr), na.rm = TRUE)) %>% 
  select(id_epa, month, year, pm25_med_3yr, nobs_36)








# oh wait I need a full 2020 panel too...


dat_epa_2019 <- readRDS(paste0(path_dropbox, "PM25/EPA/epa_station_level_pm25_data.rds")) %>% 
  select(date, id, year, month, day, pm25) %>% 
  mutate(date = paste(year, month, day, sep = "-")) %>% 
  filter(year == 2019) %>% 
  rename(date_station = date, id_epa = id)

# Read in smoke day and AOT (daily and background) grid
dat_smoke_aot_2019 <- readRDS(paste0(path_dropbox, "MERRA-2/us_grid_final/grid_aot_2019.RDS")) %>% 
  mutate(date = paste(substr(date, 1, 4), substr(date, 5, 6), substr(date, 7, 8), sep = "-")) %>% 
  select(grid_id, date, smoke_day) %>% 
  rename(date_grid = date)

# Join grid ID to trajectory-EPA data on EPA ID
dat_merged_2019 <- dat_epa_2019 %>% left_join(crosswalk, by = c("id_epa" = "epa_id"))

# Join smoke-AOT data to trajectory-EPA data on grid ID
nc <- 80
chunks <- split_chunks(1:nrow(dat_merged_2019), nc)
chunk_list <- vector("list", nc)
start_time <- get_start_time("Started merging with grid:")
for (i in 1:nc) {
  print(paste("Working on chunk:", i))
  chunk_list[[i]] <- dat_merged_2019[chunks[[i]],] %>% 
    left_join(dat_smoke_aot_2019, 
              by = c("grid_id", "date_station" = "date_grid"))
}
print_time(start_time, message = "Finished merging with grid:")


# Clean up merged data frame
dat_merged_2019 <- chunk_list %>% 
  bind_rows() %>% 
  select(-date_grid) %>% 
  rename(date = date_station)

dat_merged <- dat_merged %>% 
  bind_rows(dat_merged_2019) %>% 
  select(date, id_epa, lon_epa, lat_epa, pm25, smoke_day) %>% 
  distinct() %>% 
  mutate(year = substr(date, 1, 4),
         month = substr(date, 6, 7))

dat_epa <- dat_merged %>% 
  group_by(id_epa, month, year) %>% 
  summarize(pm25 = list(pm25[which(!is.na(pm25) & smoke_day == 0)])) %>% 
  rowwise() %>% 
  mutate(nobs = length(pm25)) %>% 
  ungroup() %>% 
  arrange(id_epa, month, year) %>% 
  group_by(id_epa, month) %>% 
  mutate(pm25_lead = lead(pm25, n = 1, default = list(NA)),
         pm25_lag = lag(pm25, n = 1, default = list(NA)),
         nobs_lead = lead(nobs, n = 1, default = 0),
         nobs_lag = lag(nobs, n = 1, default = 0)) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(pm25_3yr = list(c(pm25, pm25_lag, pm25_lead)),
         nobs_36 = nobs + nobs_lead + nobs_lag) %>% 
  rowwise() %>% 
  mutate(pm25_med_3yr = median(unlist(pm25_3yr), na.rm = TRUE)) %>% 
  select(id_epa, month, year, pm25_med_3yr, nobs_36)

dat_epa <- dat_merged %>% 
  left_join(dat_epa) %>% 
  mutate(smokePM = pmax(if_else(smoke_day == 1, pm25 - pm25_med_3yr, 0), 0))














# Anomalize
pm25_background <- dat_epa %>% 
  group_by(id, month, year) %>% 
  summarize(pm25 = list(pm25[which(!is.na(pm25) & smoke_day == 0)])) %>% 
  rowwise() %>% 
  mutate(nobs = length(pm25)) %>% 
  ungroup() %>% 
  arrange(id, month, year) %>% 
  group_by(id, month) %>% 
  mutate(pm25_lead = lead(pm25, n = 1, default = list(NA)),
         pm25_lag = lag(pm25, n = 1, default = list(NA)),
         nobs_lead = lead(nobs, n = 1, default = 0),
         nobs_lag = lag(nobs, n = 1, default = 0)) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(pm25_3yr = list(c(pm25, pm25_lag, pm25_lead)),
         nobs_36 = nobs + nobs_lead + nobs_lag) %>% 
  rowwise() %>% 
  mutate(pm25_med_3yr = median(unlist(pm25_3yr), na.rm = TRUE)) %>% 
  select(id, lon, lat, year, month, pm25_med_3yr)

dat_epa <- dat_epa %>% 
  left_join(pm25_background) %>% 
  mutate(smokePM = pmax(if_else(smoke_day == 1, pm25 - pm25_med_3yr, 0), 0))






































set.seed(3473)
mdl <- xgboost(data = dat_train %>% 
                 mutate(month = month %>% as.character() %>% as.numeric()) %>% 
                 select(-smokePM) %>% 
                 as.matrix(), 
               label = dat_train$smokePM, 
               colsample_bytree = 0.5, 
               max_depth = 4, 
               subsample = 0.5, 
               num_parallel_tree = 10, 
               nrounds = 1,
               missing = NA)

train <- xgb.DMatrix(data = dat_train %>% 
                       mutate(month = month %>% as.character() %>% as.numeric()) %>% 
                       select(-smokePM) %>% 
                       as.matrix(),
                     label = dat_train$smokePM)

