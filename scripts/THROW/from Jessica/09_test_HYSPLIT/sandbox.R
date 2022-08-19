library(lubridate)
library(stringr)
library(sf)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(maps)
library(mapdata)
usa <- map_data('usa')
usa = usa %>% st_as_sf(coords = c("long", "lat"))
st_crs(usa) = 4326

hysplit = readRDS("~/BurkeLab Dropbox/Data/hms_hysplit/hms_hysplit_initialization_distinct.rds")
h0913 = hysplit %>% filter(ymd == "2020-09-13")
h0913 = h0913 %>% st_as_sf(coords = c("lon", "lat"))
st_crs(h0913) = 4326
hist(h0913$hour)
head(h0913)
count(h0913, hour)

t0913 = readRDS("/Volumes/Seagate PD JBL/HYSPLIT/trajectory points/trajectory_points20200913.rds")
t0913

smoke = readRDS("~/BurkeLab Dropbox/Data/smoke/smoke_plumes_sfdf.RDS")
s0913 = smoke %>% filter(date == "20200913")
s0913_2 = s0913 %>% filter(Start != End)

ggplot() + geom_sf(data = s0913)
ggplot() + 
  geom_sf(data = s0913_2) + 
  geom_sf(data = usa) + 
  geom_sf(data = h0913 %>% filter(hour >= 21), color = "red") + 
  geom_sf(data = t0913, color = "purple")

out = readRDS("/Volumes/Seagate PD JBL/HYSPLIT/miscellaneous/grid_trajectories/misc/traj_out.rds")
out2 = unlist(out)
traj_files = readRDS("~/Documents/GitHub/smoke_PM_prediction/work/09_test_HYSPLIT/results/traj_files_2010_2020.rds")
traj_dates = sapply(traj_files, function(x) strsplit(x, "\\/")[[1]][3])
i = which(traj_dates == "2020-09-13")
x = out2[i]
hist(x)

g = readRDS("~/BurkeLab Dropbox/Data/hms_hysplit/10km_grid/grid_trajectory_points_2020_09.rds")
g0913 = g %>% filter(date == "20200913")
count(g0913, num_traj_points_total)

range(s0913_2$Start)
range(s0913_2$End)
with_tz(range(t0913$traj_dt_UTCm6), tz = "UTC")
hist(h0913$hour)

s0913w = s0913 %>% 
  filter(!is.na(Start), !is.na(End)) %>% 
  separate(Start, into = c("Start.Date", "Start.Time"), sep = " ", fill = "left") %>% 
  separate(End, into = c("End.Date", "End.Time"), sep = " ", fill = "left") %>% 
  mutate(across(c(Start.Time, End.Time), str_pad, width = 4, side = "left", pad = 0),
         across(c(Start.Time, End.Time), ~ymd_hm(paste(date, .), tz = "UTC")),
         plume_period = Start.Time %--% End.Time)
h0913w = h0913 %>% 
  mutate(dt = ymd_hm(paste(ymd, hour, minute), tz = "UTC"))
o = h0913w %>% st_join(s0913w, left = F)
ot = o %>% filter(dt %within% plume_period)
View(o %>% select(dt, plume_period) %>% arrange(dt))
h0913 %>% count(hour < 6)
660/(660+2112)
s0913 %>% count(Start == End)

































library(dplyr)
library(lubridate)
library(ggplot2)

# 1 = trajectory up to hitting ground not within date range
# 2 = no plumes on dates of first 48 hours
# 3 = no spatial overlap w/ plume in first 48 hours
# 4 = no temporal overlap w/ spatially overlapping plume in first 48 hours
# 11 = date not within date range
# 12 = date has no qualifying trajectory points
# 0 = date successfully saved

traj_files = readRDS("~/Documents/GitHub/smoke_PM_prediction/work/09_test_HYSPLIT/results/traj_files_2010_2020.rds")
traj_dates = sapply(traj_files, function(x) strsplit(x, "\\/")[[1]][3])

out = readRDS("/Volumes/Seagate PD JBL/HYSPLIT/miscellaneous/grid_trajectories/misc/traj_out.rds")
out0 = sapply(out, function(x) 0 %in% x)
out1 = sapply(out, function(x) 1 %in% x)
out2 = sapply(out, function(x) 2 %in% x)
out3 = sapply(out, function(x) 3 %in% x)
out4 = sapply(out, function(x) 4 %in% x)
sum(out0)
sum(out1)
sum(out2)
sum(out3)
sum(out4)
sum(out2 | out3 | out4)/length(out)

x = traj_dates[which(out2 | out3 | out4)]
ggplot(data.frame(x=ymd(x)), aes(x)) + 
  geom_histogram() + 
  scale_x_date(breaks = scales::pretty_breaks(n = 12)) + 
  labs(x = "initialization date", y = "count of trajectories discarded")
unique(x)
head(x)
tail(x)



























































library(sf)
library(dplyr)

x = read_sf("~/BurkeLab Dropbox/Data/hms_hysplit/hms_hysplit20080412.shp")
st_crs(x)
x = read_sf("~/BurkeLab Dropbox/Data/hms_hysplit/hysplit.20180731.shp")
st_crs(x)

x = read_sf("~/Downloads/data 8/oper/newhms/output/hysplit20190423.shp")
st_crs(x)
x = read_sf("~/Downloads/data 9/oper/newhms/output/hysplit20200805.shp")
st_crs(x)












































library(sp)
library(rgeos)
library(sf)

project_grid = readRDS("~/Documents/GitHub/purple-air-infiltration/data/grid.RDS")

distance_buffer = 5*1000

pg1 = gBuffer(project_grid, byid = T, 
              width = 5000,#distance_buffer, # distance_bin_width * num_distance_bins, 
              capStyle = "SQUARE")
pg2 = gBuffer(project_grid, byid = T, 
              width = 5000,#distance_buffer, # distance_bin_width * num_distance_bins, 
              capStyle = "ROUND")
pg3 = gBuffer(project_grid, byid = T, 
              width = distance_buffer, # distance_bin_width * num_distance_bins, 
              capStyle = "ROUND")

plot(pg1[1:10,])
plot(pg2[1:10,], add = T)
plot(pg3[1:10,])

pg4 = project_grid %>% 
  st_as_sf() %>% 
  # st_transform(st_crs(traj_points)) %>% 
  rename(id_grid = ID)
plot(pg4[1:10, "id_grid"], add = T)

pg5 = pg4 %>% st_buffer(dist = 5000)
plot(pg5[1:10, "id_grid"], add = T)

pg6 = gBuffer(project_grid, byid = T, 
              width = 4000,#distance_buffer, # distance_bin_width * num_distance_bins, 
              capStyle = "ROUND")
plot(pg6[1:10,], add = T)
pg7 = pg4 %>% st_buffer(dist = 3000)
plot(pg7[1:10, "id_grid"], add = T)
pg8 = gBuffer(project_grid, byid = T, 
              width = 2000,#distance_buffer, # distance_bin_width * num_distance_bins, 
              capStyle = "ROUND",
              quadsegs = 30)
plot(pg8[1:10,], add = T)














































traj_points_grid1_d

full_grid = t(matrix(1:(515*433), 515, 433))
g = data.frame(id_grid = 1:(515*433))
x = g %>% left_join(traj_points_grid1_d)
y = x %>% pull(num_traj_points_total)
y2 = t(matrix(y, 515, 433))

r = raster(ncols = 515, nrows = 433)
values(r) = y2
plot(r)
f = focal(r, w = matrix(1, 3, 3), fun = "sum", na.rm = T)
plot(f)






















































# # Set window
# win = 5
# 
# # Set output path
# path_out = paste0(path_dropbox, "hms_hysplit/10km_grid_buffered/")
# 
# # Read in project grid
# project_grid = readRDS(paste0("~/Documents/GitHub/purple-air-infiltration/", "data/grid.RDS"))
# project_grid_ids = project_grid$ID
# 
# # Build a raster for computation (does not correspond exactly to shapes in project grid)
# res = 10000
# proj = "+proj=utm +zone=19 +datum=NAD83 +units=m +no_defs"
# counties = readRDS(paste0(path_dropbox, "boundaries/all_national_counties.rds"))
# counties = counties[!counties$STATEFP %in% c("02", "15", "60", "66", "69", "72", "78"), ]
# counties = spTransform(counties, proj)
# full_grid = CreateGrid(counties, resolution = res, returnclass = "sp")
# dim_x = length(unique(full_grid$COORDX))
# dim_y = length(unique(full_grid$COORDY))
# # full_grid_ids = 1:(dim_x*dim_y)
# # full_grid_df = data.frame(id_grid = full_grid_ids)
# full_grid_df = full_grid@data %>% 
#   arrange(desc(COORDY), COORDX) %>% 
#   select(id_grid = ID)
# comp_rast = raster(ncols = dim_x, nrows = dim_y, 
#                    xmn = 0, xmx = dim_x, ymn = 0, ymx = dim_y)
# 
# # Get grid file names
# grid_files = list.files(paste0(path_dropbox, "hms_hysplit/10km_grid/"), full.names = T)
# traj_points_grid = readRDS(grid_files[1])
# cols = grep("^num_traj_points", names(traj_points_grid), value = T)
# 
# start_time = get_start_time()
# for (grid_file in grid_files) {
#   traj_points_grid = readRDS(grid_file)
#   dates_m = unique(traj_points_grid$date)
#   out = vector("list", length(dates_m))
#   for (j in seq_along(dates_m)) {
#     d = dates_m[j]
#     traj_points_grid_d = traj_points_grid %>% filter(date == d)
#     traj_points_grid_d = full_grid_df %>% left_join(traj_points_grid_d)
#     out_d = vector("list", length(cols))
#     for (i in seq_along(cols)) {
#       col = cols[i]
#       # col = "id_grid"
#       comp_rast_c = comp_rast
#       # vals = matrix(traj_points_grid_d[col], ncol = dim_x, nrow = dim_y, byrow = T)
#       # vals = vals[nrow(vals):1, ]
#       # values(comp_rast_c) = as.vector(t(vals))
#       values(comp_rast_c) = traj_points_grid_d %>% pull(col)
#       # comp_rast_c = focal(comp_rast_c, w = matrix(1, win, win), fun = sum, na.rm = T)
#       df = full_grid_df %>% 
#         mutate(col = values(comp_rast_c)) %>% 
#         filter(id_grid %in% project_grid_ids)
#       names(df)[2] = col
#       out_d[[i]] = df
#     }
#     out_d = reduce(out_d, full_join) %>% mutate(date = d, .after = id_grid)
#     out[[j]] = out_d
#   }
#   out = bind_rows(out) %>% arrange(id_grid, date)
#   out_file = paste0(path_out, basename(grid_file))
#   saveRDS(out, out_file)
# }
# print_time(start_time)









































# # Set window
# win = 5
# 
# # Set output path
# path_out = paste0(path_dropbox, "hms_hysplit/10km_grid_buffered/")
# 
# # Read in project grid
# project_grid = readRDS(paste0("~/Documents/GitHub/purple-air-infiltration/", "data/grid.RDS"))
# # project_grid = gBuffer(project_grid, byid = T, width = 5000, capStyle = "SQUARE")
# # project_grid = project_grid %>% 
# #   st_as_sf() %>% 
# #   select(id_grid = ID)
# project_grid_ids = project_grid$ID
# 
# # Build a raster for computation (does not correspond exactly to shapes in project grid)
# res = 10000
# proj = "+proj=utm +zone=19 +datum=NAD83 +units=m +no_defs"
# # county_proj = "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
# counties = readRDS(paste0(path_dropbox, "boundaries/all_national_counties.rds"))
# counties = counties[!counties$STATEFP %in% c("02", "15", "60", "66", "69", "72", "78"), ]
# counties = spTransform(counties, proj)
# # c = gBuffer(counties, width = res)
# full_grid = CreateGrid(counties, resolution = res, returnclass = "sp")
# # full_grid = spTransform(full_grid, proj)
# dim_x = length(unique(full_grid$COORDX))
# dim_y = length(unique(full_grid$COORDY))
# full_grid_ids = 1:(dim_x*dim_y)
# full_grid_df = data.frame(id_grid = full_grid_ids)
# comp_rast = raster(ncols = dim_x, nrows = dim_y, 
#                    xmn = 0, xmx = dim_x, ymn = 0, ymx = dim_y)
# 
# # Get grid file names
# grid_files = list.files(paste0(path_dropbox, "hms_hysplit/10km_grid/"), full.names = T)
# traj_points_grid = readRDS(grid_files[1])
# cols = grep("^num_traj_points", names(traj_points_grid), value = T)
# 
# for (grid_file in grid_files) {
#   traj_points_grid = readRDS(grid_file)
#   dates_m = unique(traj_points_grid$date)
#   out = vector("list", length(dates_m))
#   for (j in seq_along(dates_m)) {
#     d = dates_m[j]
#     traj_points_grid_d = traj_points_grid %>% filter(date == d)# %>% 
#       # select(id_grid, starts_with("num_traj_points"))
#     traj_points_grid_d = full_grid_df %>% left_join(traj_points_grid_d)
#     out_d = vector("list", length(cols))
#     for (i in seq_along(cols)) {
#       col = cols[i]
#       comp_rast_c = comp_rast
#       values(comp_rast_c) = traj_points_grid_d %>% pull(col)
#       comp_rast_c = focal(comp_rast_c, w = matrix(1, win, win), fun = sum, na.rm = T)
#       df = full_grid_df %>% 
#         mutate(col = values(comp_rast_c)) %>% 
#         filter(id_grid %in% project_grid_ids)
#       names(df)[2] = col
#       # df1 = df %>% filter(id_grid %in% project_grid_ids)
#       out_d[[i]] = df
#     }
#     out_d = reduce(out_d, full_join) %>% mutate(date = d, .after = id_grid)
#     out[[j]] = out_d
#   }
#   out = bind_rows(out) %>% arrange(id_grid, date)
#   out_file = paste0(path_out, basename(grid_file))
#   saveRDS(out, out_file)
# }

























# # Read in project grid
# project_grid = readRDS(paste0("~/Documents/GitHub/purple-air-infiltration/", "data/grid.RDS"))
# project_grid = gBuffer(project_grid, byid = T, width = 5000, capStyle = "SQUARE")
# project_grid = project_grid %>% 
#   st_as_sf() %>% 
#   select(id_grid = ID)
# 
# # Get grid file names
# grid_files = list.files(paste0(path_dropbox, "hms_hysplit/10km_grid/"), full.names = T)
# path_out = paste0(path_dropbox, "hms_hysplit/10km_grid_buffered/")
# 
# # Code by edzer at https://github.com/r-spatial/stars/issues/176
# # foc = function(x, w) {
# #   raster::as.matrix(raster::focal(raster::raster(x), w))
# # }
# 
# # Focalize
# start_time = get_start_time()
# for (grid_file in grid_files) {
#   traj_points_grid = readRDS(grid_file) %>% select(id_grid, date, starts_with("num_traj_points"))
#   traj_points_grid1 = project_grid %>% full_join(traj_points_grid)
#   cols = grep("^num_traj_points", names(traj_points_grid1), value = T)
#   
#   traj_points_grid1_d = traj_points_grid1 %>% filter(date == "20100607")
#   
#   for (col in cols) {
#     # traj_points_grid2 = traj_points_grid1 %>% st_as_stars()
#     traj_points_grid2 = traj_points_grid1_d %>% select(col) %>% st_rasterize()
#     ggplot() + geom_sf(data = traj_points_grid2)
#     plot(traj_points_grid2[,1:10,])
#     
#     
#     traj_points_grid3 = traj_points_grid2 %>% focal2(w = matrix(1, 5, 5), fun = "sum")
#     # traj_points_grid3 = traj_points_grid2 %>% st_apply(3, foc, w = matrix(1, 5, 5))#focal(traj_points_grid2, w = 5, fun = sum)
#     traj_points_grid4 = traj_points_grid3 %>% st_as_sf()
#   }
#   grid_file_name = basename(grid_file)
#   saveRDS(traj_points_grid4, paste0(path_out, grid_file_name))
# }
# print_time(start_time)



























































library(sf)
library(dplyr)

x = readRDS("~/BurkeLab Dropbox/Data/hms_hysplit/hms_hysplit_points.rds")
glimpse(x)

x1 = names(x)
"20191231" %in% x1

x2 = lapply(x, class)
tail(x2)
x[["20201231"]]

x3 = lapply(x, st_crs)

x4 = lapply(x, function(x) nchar(x$Date))
unique(unlist(x4))

x5 = lapply(x, function(x) 6 %in% nchar(x$Date))
View(x[["20100601"]])

x6 = lapply(x, function(x) x %>% filter(Duration > 0))
x7 = lapply(x6, function(x) nchar(x$Date))
unique(unlist(x7))

x8 = lapply(x, function(x) x %>% filter(nchar(Date) == 8))
identical(x6, x8)






















library(lubridate)
library(dplyr)
library(tools)

mydates = format(seq.Date(ymd("20051226"), ymd("20060430"), by = "day"), "%Y%m%d")
myfiles = list.files("~/BurkeLab Dropbox/Data/hms_hysplit/", pattern = "^hms_hysplit")
myfiles = grep(paste(mydates, collapse = "|"), myfiles, value = T)
myfiles = paste0("~/BurkeLab Dropbox/Data/hms_hysplit/", myfiles)
unique(file_ext(myfiles))
mydf = list()

for (i in seq_along(myfiles)) mydf[[gsub("^hms_hysplit|\\.txt$", "", basename(myfiles[i]))]] = read.table(myfiles[i])
























hysplit_missing_dates = readRDS("~/BurkeLab Dropbox/Data/hms_hysplit/hysplit_dates_not_online.rds")
smoke_missing_dates = readRDS("~/BurkeLab Dropbox/Data/smoke/smoke_dates_not_online.rds")
fire_missing_dates = readRDS("~/BurkeLab Dropbox/Data/fire/fire_dates_not_online.rds")
missing_dates = unique(c(hysplit_missing_dates, smoke_missing_dates, fire_missing_dates))
missing_dates_df = data.frame(missing_date = missing_dates) %>% mutate(fire_missing_date = ifelse(missing_date %in% fire_missing_dates, missing_date, NA), hysplit_missing_date = ifelse(missing_date %in% hysplit_missing_dates, missing_date, NA), smoke_missing_date = ifelse(missing_date %in% smoke_missing_dates, missing_date, NA))
missing_dates_df = missing_dates_df %>% arrange(missing_date)
length(hysplit_missing_dates)
saveRDS(missing_dates_df, "~/Desktop/missing_dates_df.rds")


































# Timers
print_time <- function(start, unit = "auto", message = "Time elapsed:") {
  end <- Sys.time()
  d <- difftime(time1 = end, time2 = start, units = unit)
  t <- round(d, digits = 1)
  u <- units(d)
  
  print(paste("Start time:", start))
  print(paste("End time:", end))
  message <- paste(message, t, u)
  print(message)
  return(d)
}

get_start_time <- function(message = "Time started:") {
  t <- Sys.time()
  print(paste(message, t))
  return(t)
}

library(dplyr)
library(splitr)
library(lubridate)

start_time = get_start_time()
hysplit_trajectory(
  duration = 24*6,
  days = ymd("20200609"),
  daily_hours = 10,
  met_type = "gdas1",
  met_dir = "/Users/jessssli/BurkeLab Dropbox/Data/meteorology/gdas1/",
  exec_dir = "/Users/jessssli/Desktop/tmp/run_hysplit/",
  extended_met = T
  # clean_up = F
)
print_time(start_time)




























































library(sf)
library(dplyr)
library(stringr)

# ARCHIVE GIS ZIP
files = list.files("~/Downloads/", pattern = "^data")
files = files[which(nchar(files) == 7)]
files = files[which(substr(files, 6, 7) >= "13" & substr(files, 6, 7) <= "43")]
files = sapply(paste0("~/Downloads/", files), list.files, pattern = "\\.shp$", recursive = T, full.names = T)

df = lapply(files, read_sf)
sapply(df, nrow) %>% unique()

# ARCHIVE GIS DBF, SHP, SHX
files = paste0("~/Downloads/hysplit201810", str_pad(1:31, 2, "left", 0), ".shp")
df = lapply(files, read_sf)
sapply(df, nrow) %>% unique()

# ARCHIVE TEXT: no files available for download

# BACKUP ARCHIVE GIS ZIP
files = list.files("~/Downloads/", pattern = "^data")
files = files[which(nchar(files) == 7)]
files = files[which(substr(files, 6, 7) >= "44" & substr(files, 6, 7) <= "74")]
files = sapply(paste0("~/Downloads/", files), list.files, pattern = "\\.shp$", recursive = T, full.names = T)

df = lapply(files, read_sf)
sapply(df, nrow) %>% unique()

# BACKUP ARCHIVE GIS DBF.GZ, SHP, SHX
files = paste0("~/Downloads/tmp/hysplit201810", str_pad(1:31, 2, "left", 0), " (1).shp")

df = lapply(files, read_sf)
sapply(df, nrow) %>% unique()

# BACKUP ARCHIVE TEXT: no files available for download




































































library(dplyr)
library(sf)
library(lubridate)

# ODDLY EMPTY?
hysplit = readRDS("~/BurkeLab Dropbox/Data/hms_hysplit/hms_hysplit_points_20060419-20201231.rds")
dates_empty = sapply(hysplit, nrow)
dates_empty = names(hysplit)[which(dates_empty == 0)]
dates_not_online = readRDS("~/BurkeLab Dropbox/Data/hms_hysplit/hysplit_dates_not_online.rds")
intersect(dates_not_online, dates_empty)
dates_gis_corrupt = readRDS("~/BurkeLab Dropbox/Data/hms_hysplit/hysplit_dates_gis_corrupt.rds")
intersect(dates_gis_corrupt, dates_empty)
# dates_empty[which(substr(dates_empty, 1, 6) != "201810")]

# seems we have smoke data files on all the empty hysplit dates
smoke = readRDS("~/BurkeLab Dropbox/Data/smoke/smoke_plumes_list_sf.rds")
smoke = smoke[which(names(smoke) %in% dates_empty)]
# sapply(smoke, nrow)

# seems we have fire data files on all the empty hysplit dates
fire = readRDS("~/BurkeLab Dropbox/Data/fire/hms_fires.RDS")
fire = fire[which(names(fire) %in% dates_empty)]
# sapply(fire, nrow)

df = data.frame(empty_date = dates_empty,
                num_hysplit_points = 0,
                num_fire_points = sapply(fire, nrow),
                num_smoke_plumes = sapply(smoke, nrow)) %>% 
  mutate(day_before = format(ymd(empty_date) - days(1), "%Y%m%d"),
         day_after = format(ymd(empty_date) + days(1), "%Y%m%d"),
         empty_day_before = lag(empty_date, 1),
         empty_day_after = lead(empty_date, 1),
         consecutive = ifelse(((day_before != empty_day_before) & (day_after != empty_day_after)) | 
                                (is.na(empty_day_before) | is.na(empty_day_after)), 0, 1)) %>% 
  select(-day_before, -day_after, -empty_day_before, -empty_day_after) %>% 
  mutate(month = month(ymd(empty_date)), .before = num_hysplit_points)

saveRDS(df, "~/Desktop/empty_hysplit_dates.rds")

df %>% 
  filter(substr(empty_date, 1, 6) != "201810",
         !(empty_date %in% paste0("201903", 13:18))) %>% 
  View()

# 20190313-20190318
# ARCHIVE TEXT: all empty files
# ARCHIVE GIS: no files available for download
# BACKUP ARCHIVE TEXT: all empty files
# BACKUP ARCHIVE GIS ZIP
files = list.files("~/Downloads/", pattern = "^data")
files = files[which(nchar(files) == 7)]
files = files[which(substr(files, 6, 7) >= "75" & substr(files, 6, 7) <= "80")]
files = sapply(paste0("~/Downloads/", files), list.files, pattern = "\\.shp$", recursive = T, full.names = T)

x = lapply(files, read_sf)
sapply(x, nrow) %>% unique()

# BACKUP ARCHIVE GIS DBF.GZ, SHP, SHX
files = paste0("~/Downloads/tmp20190313-20190318/hysplit201903", 13:18, ".shp")

x = lapply(files, read_sf)
sapply(x, nrow) %>% unique()























































library(dplyr)
library(sf)
library(lubridate)
library(stringr)
library(splitr)
library(ggplot2)

df = readRDS("~/Desktop/tmp/trajectory_points20060819.rds")
i = which.max(df$traveled_distance_km)

View(df[(i-200):(i+1),])
x = df[i,]
dups = readRDS("~/BurkeLab Dropbox/Data/hms_hysplit/hms_hysplit_initialization_duplicates_20060419-20201231.rds")
x2 = inner_join(x %>% mutate(dt_i = with_tz(traj_dt_UTCm6 - hours(hour_along), "UTC"),
                             year_i = year(dt_i),
                             month_i = month(dt_i),
                             day_i = day(dt_i),
                             hour_i = hour(dt_i)), 
                dups %>% rename(lon_i = lon, lat_i = lat, height_i = height,
                                year_i = year, month_i = month, day_i = day, hour_i = hour) %>% 
                  select(-date))
y = unique(x2$year_i)
m = str_pad(unique(x2$month_i), 2, "left", 0)
d = str_pad(unique(x2$day_i), 2, "left", 0)
h = str_pad(unique(x2$hour_i), 2, "left", 0)
lon = unique(x2$lon_i)
lat = unique(x2$lat_i)
height = 2500

file = sprintf("/Volumes/Seagate PD JBL/HYSPLIT/6-day trajectories/rds/%s/%s-%s/%s-%s-%s/%s-%s-%s-%s/traj-traj-fwd-%s-%s-%s-%s-1lat_%s_lon_%s-hgt_%s-144h.rds",
               y, y, m, y, m, d, y, m, d, h, substr(y, 3, 4), m, d, h,
               gsub("\\.", "p", lat), gsub("\\.", "p", lon), height)
traj = readRDS(file)
any(traj$height == x$height)

trajectory_plot(traj)
traj = st_as_sf(traj, coords = c("lon", "lat"), crs = 4326) # not right CRS per se but works enough
ggplot(traj) + geom_sf()
# wow this smoke legit started in Idaho and made it to Hungary in 6 days

















































library(dplyr)
library(sf)
library(lubridate)
library(stringr)
library(splitr)

x = readRDS("/Volumes/Seagate PD JBL/HYSPLIT/6-day trajectories/trajectory points/trajectory_points20060420.rds")
files = list.files("/Volumes/Seagate PD JBL/HYSPLIT/6-day trajectories/trajectory points/",
                   pattern = "^trajectory_points2020.*\\.rds$",
                   full.names = T)
x = files %>% 
  sapply(function(x) {
    df = readRDS(x)
    any(df$consecutive_distance_km == max_consecutive_distance, na.rm = T)
  })
file = files[which(x)]
df = readRDS(file)
xx = df %>% filter(consecutive_distance_km == max_consecutive_distance)
dups = readRDS("~/BurkeLab Dropbox/Data/hms_hysplit/hms_hysplit_initialization_duplicates_20060419-20201231.rds")
x2 = inner_join(xx %>% mutate(dt_i = with_tz(traj_dt_UTCm6 - hours(hour_along), "UTC"),
                              year_i = year(dt_i),
                              month_i = month(dt_i),
                              day_i = day(dt_i),
                              hour_i = hour(dt_i)), 
                dups %>% rename(lon_i = lon, lat_i = lat, height_i = height,
                                year_i = year, month_i = month, day_i = day, hour_i = hour) %>% 
                  select(-date))
y = unique(x2$year_i)
m = str_pad(unique(x2$month_i), 2, "left", 0)
d = str_pad(unique(x2$day_i), 2, "left", 0)
h = str_pad(unique(x2$hour_i), 2, "left", 0)
lon = unique(x2$lon_i)
lat = unique(x2$lat_i)
height = unique(x2$height_i)

file = sprintf("/Volumes/Seagate PD JBL/HYSPLIT/6-day trajectories/rds/%s/%s-%s/%s-%s-%s/%s-%s-%s-%s/traj-traj-fwd-%s-%s-%s-%s-1lat_%s_lon_%s-hgt_%s-144h.rds",
               y, y, m, y, m, d, y, m, d, h, substr(y, 3, 4), m, d, h,
               gsub("\\.", "p", lat), gsub("\\.", "p", lon), height)
traj = readRDS(file)
trajectory_plot(traj)
traj %>% filter(height == xx$height)






























































path_dropbox = "~/BurkeLab Dropbox/Data/"

library(dplyr)
library(sf)
library(lubridate)

library(ggplot2)

#-------------------------------------------------------------------------------
# Get Oddly Empty HYSPLIT Dates
# Written by Jessica
# Last edited December 2021
# 
# Dates where HYSPLIT file is available for download but empty and should be 
# treated as missing data. For each date, criteria are:
#     1. HYSPLIT data file is available online
#     2. HYSPLIT data file is empty
#     3. At least one of the following:
#            a. Number of fire points >= x (3000)
#            b. Number of smoke plumes >= y (50)
#            c. Both fire and smoke data are not available online
#     4. Number of consecutive days satisfying criteria 1-3 >= z (3)
#-------------------------------------------------------------------------------
hysplit = readRDS(paste0(path_dropbox, "hms_hysplit/hms_hysplit_points_20060419-20201231.rds"))
smoke = readRDS(paste0(path_dropbox, "smoke/smoke_plumes_list_sf.rds"))
fire = readRDS(paste0(path_dropbox, "fire/hms_fires.RDS"))
dates = intersect(intersect(names(hysplit), names(smoke)), names(fire))
df = data.frame(date = dates,
                num_hysplit_points = sapply(hysplit[dates], nrow),
                num_fire_points = sapply(fire[dates], nrow),
                num_smoke_plumes = sapply(smoke[dates], nrow))
ggplot(df %>% filter(num_hysplit_points > 0), aes(num_fire_points, num_hysplit_points)) + 
  geom_point()
ggsave("~/Documents/send/2021-12-07/hysplit_on_fire.png")
ggplot(df %>% filter(num_hysplit_points > 0), aes(num_smoke_plumes, num_hysplit_points)) + 
  geom_point()
ggsave("~/Documents/send/2021-12-07/hysplit_on_smoke.png")
quantile(df %>% filter(num_hysplit_points > 0) %>% pull(num_fire_points), seq(0, 1, 0.01))
quantile(df %>% filter(num_hysplit_points > 0) %>% pull(num_smoke_plumes), seq(0, 1, 0.01))


# df = data.frame(date = dates,
#                 num_hysplit_points = sapply(hysplit[dates], nrow),
#                 num_fire_points = sapply(fire[dates], nrow),
#                 num_smoke_plumes = sapply(smoke[dates], nrow)) %>% 
#   filter(num_hysplit_points == 0) %>% 
#   mutate(month = month(date))
# plot(df$num_fire_points, df$num_hysplit_points, pch = 19, col = rgb(0,0,0,0.2))
# plot(df$num_smoke_plumes, df$num_hysplit_points, pch = 19, col = rgb(0,0,0,0.2))
# plot(log(df$num_fire_points), log(df$num_smoke_plumes))
# ggplot(df, aes(num_fire_points, num_hysplit_points)) + 
#   geom_point(alpha = 0.5) + 
#   geom_smooth(method = "lm")
# ggplot(df, aes(num_smoke_plumes, num_hysplit_points)) + 
#   geom_point(alpha = 0.5) + 
#   geom_smooth(method = "lm")
# x = kmeans(df %>% select(num_fire_points, num_smoke_plumes), centers = 2)
# x2 = fitted(x, "classes")
# df2 = df %>% mutate(class = as.factor(x2))
# ggplot(df2, aes(num_fire_points, num_smoke_plumes, color = class)) + 
#   geom_point()
# df3 = df %>% mutate(class = as.factor(fitted(kmeans(data.frame(sqrt(num_fire_points), sqrt(num_smoke_plumes)), 2), "classes")))
# 
# 
# ggplot(df3, aes(num_fire_points, num_smoke_plumes, color = class)) + 
#   geom_point()
# y1 = fitted(kmeans(data.frame(sqrt(df$num_fire_points), sqrt(df$num_smoke_plumes)), 2), "classes")
# ggplot(df %>% mutate(class = as.factor(y1)), aes(num_fire_points, num_smoke_plumes, color = class)) + 
#   geom_point()
# plot(boxcox(df$num_fire_points))
# 
# df4 = df %>% mutate(class = as.factor(fitted(kmeans(data.frame(sqrt(num_fire_points), sqrt(num_smoke_plumes), month, consecutive), 2), "classes")))
# ggplot(df4, aes(num_fire_points, consecutive, color = class)) + 
#   geom_point()

# get empty HYSPLIT dates
hysplit = readRDS(paste0(path_dropbox, "hms_hysplit/hms_hysplit_points_20060419-20201231.rds"))
dates_empty = sapply(hysplit, nrow)
dates_empty = names(hysplit)[which(dates_empty == 0)]
dates_not_online = readRDS(paste0(path_dropbox, "hms_hysplit/hysplit_dates_not_online.rds"))
stopifnot(length(intersect(dates_not_online, dates_empty)) == 0)
dates_gis_corrupt = readRDS(paste0(path_dropbox, "hms_hysplit/hysplit_dates_gis_corrupt.rds"))
stopifnot(length(intersect(dates_gis_corrupt, dates_empty)) == 0)

# looks like we have smoke data files on all the empty HYSPLIT dates
smoke = readRDS(paste0(path_dropbox, "smoke/smoke_plumes_list_sf.rds"))
dates_empty_missing_smoke = setdiff(dates_empty, names(smoke))
for (d in dates_empty_missing_smoke) smoke[[d]] = data.frame()
smoke = smoke[dates_empty]

# looks like we have fire data files on all the empty HYSPLIT dates
fire = readRDS(paste0(path_dropbox, "fire/hms_fires.RDS"))
dates_empty_missing_fire = setdiff(dates_empty, names(fire))
for (d in dates_empty_missing_fire) fire[[d]] = data.frame()
fire = fire[dates_empty]
dates_empty_missing_fire = intersect(dates_empty_missing_fire, dates_empty_missing_smoke)

# how are fire and smoke data on empty HYSPLIT dates?
df = data.frame(empty_date = dates_empty,
                num_hysplit_points = 0) %>% 
  mutate(num_fire_points = ifelse(empty_date %in% dates_empty_missing_fire, NA, sapply(fire, nrow)),
         num_smoke_plumes = ifelse(empty_date %in% dates_empty_missing_smoke, NA, sapply(smoke, nrow)),
         day_before = format(ymd(empty_date) - days(1), "%Y%m%d"),
         day_after = format(ymd(empty_date) + days(1), "%Y%m%d"),
         empty_day_before = lag(empty_date, 1),
         empty_day_after = lead(empty_date, 1),
         consecutive = ifelse(((day_before != empty_day_before) & (day_after != empty_day_after)) | 
                                (is.na(empty_day_before) & (day_after != empty_day_after)) | 
                                (is.na(empty_day_after) & (day_before != empty_day_before)), 0, 1),
         id_consecutive_string = cumsum(c(T, diff(ymd(empty_date)) != 1))) %>% 
  select(-day_before, -day_after, -empty_day_before, -empty_day_after) %>% 
  mutate(month = month(ymd(empty_date)), .before = num_hysplit_points)
write.csv(df, "~/Documents/send/2021-12-07/empty_hysplit_dates.csv", row.names = F)

df5 = df %>% mutate(class = as.factor(ifelse(fitted(kmeans(data.frame(sqrt(num_fire_points), sqrt(num_smoke_plumes)), 2), "classes") == 1, "empty", "mark NA")))
ggplot(df5, aes(num_fire_points, num_smoke_plumes, color = class)) + 
  geom_point()
ggsave("~/Documents/send/2021-12-07/kmeans_clustering.png")

get_affected_dates = function(x) {
  return(x %>% 
           pull(empty_date) %>% 
           lapply(function(x) format(seq.Date(ymd(x), ymd(x) + days(6), by = "day"), "%Y%m%d")) %>% 
           unlist() %>% 
           unique())
}
df11 = df %>% filter(substr(empty_date, 1, 6) %in% c("201810", "201903"))
dates1 = get_affected_dates(df11)
df12 = df %>% filter(num_smoke_plumes > 0)
dates2 = get_affected_dates(df12)
df13 = df %>% filter(num_fire_points > 0, num_smoke_plumes > 0)
dates3 = get_affected_dates(df13)


# set criteria for oddly empty dates
min_num_fire_points = 3000
min_num_smoke_plumes = 50
min_num_consecutive_empty_dates = 3

# get empty HYSPLIT dates
hysplit = readRDS(paste0(path_dropbox, "hms_hysplit/hms_hysplit_points_20060419-20201231.rds"))
dates_empty = sapply(hysplit, nrow)
dates_empty = names(hysplit)[which(dates_empty == 0)]
dates_not_online = readRDS(paste0(path_dropbox, "hms_hysplit/hysplit_dates_not_online.rds"))
stopifnot(length(intersect(dates_not_online, dates_empty)) == 0)
dates_gis_corrupt = readRDS(paste0(path_dropbox, "hms_hysplit/hysplit_dates_gis_corrupt.rds"))
stopifnot(length(intersect(dates_gis_corrupt, dates_empty)) == 0)

# looks like we have smoke data files on all the empty HYSPLIT dates
smoke = readRDS(paste0(path_dropbox, "smoke/smoke_plumes_list_sf.rds"))
dates_empty_missing_smoke = setdiff(dates_empty, names(smoke))
for (d in dates_empty_missing_smoke) smoke[[d]] = data.frame()
smoke = smoke[dates_empty]

# looks like we have fire data files on all the empty HYSPLIT dates
fire = readRDS(paste0(path_dropbox, "fire/hms_fires.RDS"))
dates_empty_missing_fire = setdiff(dates_empty, names(fire))
for (d in dates_empty_missing_fire) fire[[d]] = data.frame()
fire = fire[dates_empty]
dates_empty_missing_fire = intersect(dates_empty_missing_fire, dates_empty_missing_smoke)

# how are fire and smoke data on empty HYSPLIT dates?
df = data.frame(empty_date = dates_empty,
                num_hysplit_points = 0) %>% 
  mutate(num_fire_points = ifelse(empty_date %in% dates_empty_missing_fire, NA, sapply(fire, nrow)),
         num_smoke_plumes = ifelse(empty_date %in% dates_empty_missing_smoke, NA, sapply(smoke, nrow)),
         day_before = format(ymd(empty_date) - days(1), "%Y%m%d"),
         day_after = format(ymd(empty_date) + days(1), "%Y%m%d"),
         empty_day_before = lag(empty_date, 1),
         empty_day_after = lead(empty_date, 1),
         consecutive = ifelse(((day_before != empty_day_before) & (day_after != empty_day_after)) | 
                                (is.na(empty_day_before) & (day_after != empty_day_after)) | 
                                (is.na(empty_day_after) & (day_before != empty_day_before)), 0, 1),
         id_consecutive_string = cumsum(c(T, diff(ymd(empty_date)) != 1))) %>% 
  select(-day_before, -day_after, -empty_day_before, -empty_day_after) %>% 
  mutate(month = month(ymd(empty_date)), .before = num_hysplit_points)

saveRDS(df, "~/Desktop/empty_hysplit_dates.rds")

# subset to oddly empty dates
dates_oddly_empty = df %>% 
  filter((num_fire_points >= min_num_fire_points) | is.na(num_fire_points),
         (num_smoke_plumes >= min_num_smoke_plumes) | is.na(num_smoke_plumes)) %>% 
  mutate(id_consecutive_string_empty = cumsum(c(T, diff(ymd(empty_date)) != 1))) %>% 
  group_by(id_consecutive_string_empty) %>% 
  filter(n() >= min_num_consecutive_empty_dates) %>% 
  ungroup() %>% 
  pull(id_consecutive_string) %>% 
  unique()
dates_oddly_empty = df %>% 
  filter(id_consecutive_string %in% dates_oddly_empty) %>% 
  pull(empty_date)

saveRDS(dates_oddly_empty, paste0(path_dropbox, "hms_hysplit/hysplit_dates_oddly_empty.rds"))





















































files = list.files("~/BurkeLab Dropbox/Projects/smoke_PM_prediction/data/3_intermediate/",
                   pattern = "^aot_anom_all_days.*\\.rds$",
                   full.names = T)
myaot = vector("list", length(files))
for (i in 1:length(files)) myaot[[i]] = readRDS(files[i])$aot_anom
myaot = unlist(myaot)
quantile(myaot, seq(0, 1, 0.01))
hist(myaot)
summary(myaot)
quantile(myaot[which(myaot >= 0)], seq(0, 1, 0.01))

files = list.files("~/BurkeLab Dropbox/Projects/smoke_PM_prediction/data/3_intermediate/",
                   pattern = "^aot_anom_smoke_days.*\\.rds$",
                   full.names = T)
myaot = vector("list", length(files))
for (i in 1:length(files)) myaot[[i]] = readRDS(files[i])$aot_anom
myaot = unlist(myaot)
quantile(myaot, seq(0, 1, 0.01))
hist(myaot)
quantile(myaot[which(myaot >= 0)], seq(0, 1, 0.01))

pm_aot = vector("list", length(files))
for (i in 1:length(files)) {
  aot_y = readRDS(files[i]) %>% mutate(date = format(date, "%Y%m%d"))
  pm_aot[[i]] = inner_join(anom_pm %>% select(grid_id_10km, date, pm25_anom, smoke_day), aot_y)
}
pm_aot = bind_rows(pm_aot)
count(pm_aot, pm25_anom > 50)
pm_aot %>% filter(pm25_anom > 50) %>% pull(aot_anom) %>% quantile(seq(0,1,0.01))
pm_aot %>% filter(pm25_anom > 50) %>% pull(aot_anom) %>% hist()
pm_aot %>% filter(pm25_anom > 25) %>% pull(aot_anom) %>% quantile(seq(0,1,0.01))



















































counts_stations_day = counts_stations
counts_grid_day = counts_grid
counts_stations_agg_day = counts_stations
counts_grid_agg_day = counts_grid
p1day = perc_station_smoke_day_traj_of_plume
p2day = perc_station_smoke_day_plume_of_traj
p3day = perc_grid_smoke_day_traj_of_plume
p4day = perc_grid_smoke_day_plume_of_traj
counts_stations_month = counts_stations
counts_grid_month = counts_grid
counts_stations_agg_month = counts_stations
counts_grid_agg_month = counts_grid
p1month = perc_station_smoke_day_traj_of_plume
p2month = perc_station_smoke_day_plume_of_traj
p3month = perc_grid_smoke_day_traj_of_plume
p4month = perc_grid_smoke_day_plume_of_traj















































# Timers
print_time <- function(start, unit = "auto", message = "Time elapsed:") {
  end <- Sys.time()
  d <- difftime(time1 = end, time2 = start, units = unit)
  t <- round(d, digits = 1)
  u <- units(d)
  
  print(paste("Start time:", start))
  print(paste("End time:", end))
  message <- paste(message, t, u)
  print(message)
  return(d)
}

get_start_time <- function(message = "Time started:") {
  t <- Sys.time()
  print(paste(message, t))
  return(t)
}

path_scratch = paste0(Sys.getenv("SCRATCH"), "/")

library(rgeos)
library(sf)
library(dplyr)
library(purrr)
library(lubridate)
library(ggplot2)
library(foreach)
library(doParallel)

num_cores = as.integer(Sys.getenv("SLURM_CPUS_PER_TASK")) - 1
registerDoParallel(num_cores)

#-------------------------------------------------------------------------------
# Compare Smoke Day According to Plumes and Trajectories
# Written by Jessica
# Last edited December 2021
# 
# What % of high anomalous PM station-days/high anomalous AOT grid cell-days that
# are smoke day according to plumes are also smoke day according to trajectories?
# Vice-versa?
# Subset to dates where HYSPLIT trajectory point counts are not missing.
# Subset to dates not missing smoke data.
# 
# For specific dates, plot high anom AOT or high anom PM cells and color by 
# smoke day according to plume and/or trajectories.
#-------------------------------------------------------------------------------
# Set thresholds
min_num_traj_points = 3 # minimum traj point count if HYSPLIT points
min_pm25_anom = 50 # from previous evaluations
min_aot_anom = 0.1 # 5th percentile value of anom AOT among station-days w/ anom PM > 50 and plume overhead

# Get time period and trajectory duration
duration_days = 6
start_date = "20060101"
end_date = "20201231"
all_dates = seq.Date(ymd(start_date), ymd(end_date), by = "day")
all_dates_str = format(all_dates, "%Y%m%d")
year_months = unique(format(all_dates, "%Y_%m"))

# Get dates where plume data and trajectory point counts not missing
na_dates_plume = readRDS("data/smoke_dates_not_online.rds")
na_dates_traj = c(format(seq.Date(ymd(start_date), ymd("20060418"), by = "day"), "%Y%m%d"),
                  readRDS(paste0(path_scratch, "get_HYSPLIT_over_grid/hysplit_dates_not_online.rds")),
                  readRDS(paste0(path_scratch, "get_HYSPLIT_over_grid/hysplit_dates_gis_corrupt.rds")),
                  readRDS(paste0(path_scratch, "get_HYSPLIT_over_grid/hysplit_dates_oddly_empty.rds"))) %>% 
  lapply(function(x) format(seq.Date(ymd(x), ymd(x) + days(duration_days), by = "day"), "%Y%m%d")) %>% 
  unlist() %>% 
  unique() %>% 
  sort()
na_dates = sort(unique(c(na_dates_plume, na_dates_traj)))
nna_dates = setdiff(all_dates_str, na_dates)

# Read in anomalous PM
anom_pm = readRDS("data/epa_pm25_anom_all_days.rds") %>% 
  mutate(date = format(date, "%Y%m%d")) %>% 
  filter(date %in% nna_dates)

start_time = get_start_time()
counts_stations = foreach(m = 1:length(year_months)) %dopar% {
  year_month = year_months[m]
  y_str = substr(year_month, 1, 4)
  m_str = substr(year_month, 6, 7)
  nna_dates_m = grep(paste0("^", gsub("_", "", year_month)), nna_dates, value = T)
  if (length(nna_dates_m) == 0) return(NULL)
  
  # Subset anomalous PM for the month
  anom_pm_m = anom_pm %>% 
    filter(date %in% nna_dates_m) %>% 
    select(epa_id = id, grid_id_10km, date, pm25_anom)
  
  # Read in smoke plumes
  plume_m = readRDS(paste0("data/grid_smoke_day_", year_month, ".rds")) %>% 
    mutate(date = format(date, "%Y%m%d")) %>% 
    filter(date %in% nna_dates_m) %>% 
    select(grid_id_10km = id_grid, date, smoke_day_plume = smoke_day)
  
  # Read in trajectory point counts
  traj_m = readRDS(paste0(path_scratch, "get_HYSPLIT_over_grid/10km_grid_2006-2020/grid_trajectory_points_", year_month, ".rds")) %>% 
    filter(date %in% nna_dates_m) %>% 
    mutate(smoke_day_traj = ifelse(num_traj_points_height_1 >= min_num_traj_points, 1, 0)) %>% 
    select(grid_id_10km = id_grid, date, smoke_day_traj)
  
  # Combine grids
  stations_m = reduce(list(anom_pm_m, plume_m, traj_m), left_join, by = c("grid_id_10km", "date"))
  
  # Get counts
  return(stations_m %>% 
           filter(pm25_anom > min_pm25_anom) %>% 
           group_by(date) %>% 
           count(smoke_day_plume, smoke_day_traj) %>% 
           ungroup())
}
print_time(start_time)
saveRDS(counts_stations, "counts_stations_2.rds")


start_time = get_start_time()
counts_grid = foreach(m = 1:length(year_months)) %dopar% {
  year_month = year_months[m]
  y_str = substr(year_month, 1, 4)
  m_str = substr(year_month, 6, 7)
  nna_dates_m = grep(paste0("^", gsub("_", "", year_month)), nna_dates, value = T)
  if (length(nna_dates_m) == 0) return(NULL)
  
  # Read in anomalous AOT
  anom_aot_m = readRDS(paste0("data/aot_anom_all_days_", m_str, ".rds")) %>% 
    mutate(date = format(date, "%Y%m%d")) %>% 
    filter(date %in% nna_dates_m)
  
  # Read in smoke plumes
  plume_m = readRDS(paste0("data/grid_smoke_day_", year_month, ".rds")) %>% 
    mutate(date = format(date, "%Y%m%d")) %>% 
    filter(date %in% nna_dates_m) %>% 
    select(grid_id_10km = id_grid, date, smoke_day_plume = smoke_day)
  
  # Read in trajectory point counts
  traj_m = readRDS(paste0(path_scratch, "get_HYSPLIT_over_grid/10km_grid_2006-2020/grid_trajectory_points_", year_month, ".rds")) %>% 
    filter(date %in% nna_dates_m) %>% 
    mutate(smoke_day_traj = ifelse(num_traj_points_height_1 >= min_num_traj_points, 1, 0)) %>% 
    select(grid_id_10km = id_grid, date, smoke_day_traj)
  
  # Combine grids
  grid_m = reduce(list(anom_aot_m, plume_m, traj_m), left_join, by = c("grid_id_10km", "date"))
  
  # Get counts
  return(grid_m %>% 
           filter(aot_anom > min_aot_anom) %>% 
           group_by(date) %>% 
           count(smoke_day_plume, smoke_day_traj) %>% 
           ungroup())
}
print_time(start_time)
saveRDS(counts_grid, "counts_grid_2.rds")











































































# Timers
print_time <- function(start, unit = "auto", message = "Time elapsed:") {
  end <- Sys.time()
  d <- difftime(time1 = end, time2 = start, units = unit)
  t <- round(d, digits = 1)
  u <- units(d)
  
  print(paste("Start time:", start))
  print(paste("End time:", end))
  message <- paste(message, t, u)
  print(message)
  return(d)
}

get_start_time <- function(message = "Time started:") {
  t <- Sys.time()
  print(paste(message, t))
  return(t)
}

path_scratch = paste0(Sys.getenv("SCRATCH"), "/")
path_group_home = paste0(Sys.getenv("GROUP_HOME"), "/")

library(rgeos)
library(sf)
library(dplyr)
library(purrr)
library(lubridate)
library(ggplot2)
library(scales)
library(foreach)
library(doParallel)

num_cores = as.integer(Sys.getenv("SLURM_CPUS_PER_TASK")) - 1
registerDoParallel(num_cores)

#-------------------------------------------------------------------------------
# Compare Smoke Day According to Plumes and Trajectories
# Written by Jessica
# Last edited December 2021
# 
# What % of high anomalous PM station-days/high anomalous AOT grid cell-days that
# are smoke day according to plumes are also smoke day according to trajectories?
# Vice-versa?
# Subset to dates where HYSPLIT trajectory point counts are not missing.
# Subset to dates not missing smoke data.
# 
# For specific dates, plot high anom AOT or high anom PM cells and color by 
# smoke day according to plume and/or trajectories.
#-------------------------------------------------------------------------------
# Set thresholds
min_num_traj_points = 3 # minimum traj point count if HYSPLIT points
min_pm25_anom = 50 # from previous evaluations
min_aot_anom = 0.1 # 5th percentile value of anom AOT among station-days w/ anom PM > 50 and plume overhead

# Get time period and trajectory duration
duration_days = 6
start_date = "20060101"
end_date = "20201231"
all_dates = seq.Date(ymd(start_date), ymd(end_date), by = "day")
all_dates_str = format(all_dates, "%Y%m%d")
year_months = unique(format(all_dates, "%Y_%m"))

# Get dates where plume data and trajectory point counts not missing
na_dates_plume = readRDS("data/smoke_dates_not_online.rds")
na_dates_traj = c(format(seq.Date(ymd(start_date), ymd("20060418"), by = "day"), "%Y%m%d"),
                  readRDS(paste0(path_scratch, "get_HYSPLIT_over_grid/hysplit_dates_not_online.rds")),
                  readRDS(paste0(path_scratch, "get_HYSPLIT_over_grid/hysplit_dates_gis_corrupt.rds")),
                  readRDS(paste0(path_scratch, "get_HYSPLIT_over_grid/hysplit_dates_oddly_empty.rds"))) %>% 
  lapply(function(x) format(seq.Date(ymd(x), ymd(x) + days(duration_days), by = "day"), "%Y%m%d")) %>% 
  unlist() %>% 
  unique() %>% 
  sort()
na_dates = sort(unique(c(na_dates_plume, na_dates_traj)))
nna_dates = setdiff(all_dates_str, na_dates)

# Read in anomalous PM
anom_pm = readRDS("data/epa_pm25_anom_all_days.rds") %>% 
  mutate(date = format(date, "%Y%m%d")) %>% 
  filter(date %in% nna_dates)

#-------------------------------------------------------------------------------
# Do trajectories tell us the presence of smoke when plumes are undetected due 
# to interfering cloud cover?
min_perc_aod_missing = 0.75
aod_missing_files = list.files(paste0(path_group_home, "smoke_PM_prediction/predict_AOD/data/2_from_EE/maiac_AODmissings/"))
aod_missing_years = sapply(aod_missing_files, function(x) substr(strsplit(x, "_")[[1]][6], 1, 4))

start_time = get_start_time()
anom_pm_SD = foreach(year_month = year_months) %dopar% {
  y_str = substr(year_month, 1, 4)
  m_str = substr(year_month, 6, 7)
  nna_dates_m = grep(paste0("^", gsub("_", "", year_month)), nna_dates, value = T)
  if (length(nna_dates_m) == 0) return(NULL)
  
  # Subset anomalous PM for the month
  anom_pm_m = anom_pm %>% 
    filter(date %in% nna_dates_m) %>% 
    select(epa_id = id, grid_id_10km, date, pm25_anom)
  if (nrow(anom_pm_m) == 0) return(NULL)
  
  # Read in smoke plumes
  plume_m = readRDS(paste0("data/grid_smoke_day_", year_month, ".rds")) %>% 
    mutate(date = format(date, "%Y%m%d")) %>% 
    filter(date %in% nna_dates_m) %>% 
    select(grid_id_10km = id_grid, date, smoke_day_plume = smoke_day)
  if (nrow(plume_m) == 0) return(NULL)
  
  # Read in trajectory point counts
  traj_m = readRDS(paste0(path_scratch, "get_HYSPLIT_over_grid/10km_grid_2006-2020/grid_trajectory_points_", year_month, ".rds")) %>% 
    filter(date %in% nna_dates_m) %>% 
    mutate(smoke_day_traj = ifelse(num_traj_points_height_1 >= min_num_traj_points, 1, 0)) %>% 
    select(grid_id_10km = id_grid, date, smoke_day_traj)
  if (nrow(traj_m) == 0) return(NULL)
  
  # Read in percent missing AOD
  aod_missing_m = aod_missing_files[which(aod_missing_years == y_str)]
  aod_missing_m = paste0(path_group_home, "smoke_PM_prediction/predict_AOD/data/2_from_EE/maiac_AODmissings/", aod_missing_m)
  aod_missing_m = aod_missing_m %>% 
    map_dfr(read.csv, colClasses = c(start_date = "character")) %>% 
    select(grid_id_10km = ID, date = start_date, perc_aod_missing = mean) %>% 
    filter(date %in% nna_dates_m)
  if (nrow(aod_missing_m) == 0) return(NULL)
  
  # Combine grids
  stations_m = reduce(list(anom_pm_m, plume_m, traj_m, aod_missing_m), left_join, by = c("grid_id_10km", "date"))
  
  # Get counts
  return(
    list(anom_pm_SDplume = stations_m %>% 
           filter(smoke_day_plume == 1) %>% 
           pull(pm25_anom),
         anom_pm_notSDplume_SDtraj_cloudy = stations_m %>% 
           filter(smoke_day_plume == 0,
                  smoke_day_traj == 1,
                  perc_aod_missing > min_perc_aod_missing) %>% 
           pull(pm25_anom))
  )
}
print_time(start_time)
stopImplicitCluster()
anom_pm_SDplume = lapply(anom_pm_SD, "[[", "anom_pm_SDplume")
anom_pm_notSDplume_SDtraj_cloudy = lapply(anom_pm_SD, "[[", "anom_pm_notSDplume_SDtraj_cloudy")
saveRDS(anom_pm_SDplume, "anom_pm_SDplume.rds")
saveRDS(anom_pm_notSDplume_SDtraj_cloudy, "anom_pm_notSDplume_SDtraj_cloudy.rds")

anom_pm_SDplume = unlist(anom_pm_SDplume)
anom_pm_notSDplume_SDtraj_cloudy = unlist(anom_pm_notSDplume_SDtraj_cloudy)

# Plot distribution of anomalous PM on station-days that are smoke day according
# to trajectory point count, do not have plume overhead, and are cloudy
# ax = pretty(range(c(anom_pm_SDplume, anom_pm_notSDplume_SDtraj_cloudy)), n = 100)
pdf("anom_pm_SD.pdf", width = 13, height = 6)
hist(anom_pm_SDplume, col = alpha("red", 0.5))#, breaks = ax)
hist(anom_pm_notSDplume_SDtraj_cloudy, add = T, col = alpha("blue", 0.5))#, breaks = ax)
dev.off()

summary(anom_pm_SDplume)
summary(anom_pm_notSDplume_SDtraj_cloudy)

quantile(anom_pm_SDplume, seq(0, 1, 0.01))
quantile(anom_pm_notSDplume_SDtraj_cloudy, seq(0, 1, 0.01))






































x2 = full_join(x, project_grid) %>% 
  group_by(grid_id_10km, date) %>% 
  summarize(pm25_anom = mean(pm25_anom, na.rm = T)) %>% 
  ungroup()
x3 = left_join(project_grid, x2)
ggplot(x3, aes(color = pm25_anom)) + geom_sf()























































wrong_dates_all = c()
for (m in 1:length(year_months)) {
  year_month = year_months[m]
  print(paste(year_month, "--------------------------------------------------"))
  y_str = substr(year_month, 1, 4)
  m_str = substr(year_month, 6, 7)
  dates_m = ymd(grep(paste0(y_str, m_str), all_dates_str, value = T))
  wrong_dates = dates_m[which((year(dates_m) != as.integer(y_str)) | (month(dates_m) != as.integer(m_str)))]
  wrong_dates_all = c(wrong_dates_all, wrong_dates)
  print(wrong_dates)
}
class(wrong_dates_all) = "Date"
print(wrong_dates_all)
unique(year(wrong_dates_all))



























































path_dropbox = "~/BurkeLab Dropbox/Data/"
path_project = "~/BurkeLab Dropbox/Projects/smoke_PM_prediction/"

library(dplyr)
library(tidyr)
library(lubridate)

#-------------------------------------------------------------------------------
d0p = rep(c(T,F), times = 16)
d0t = rep(c(T,F), each = 2, times = 8)
d1t = rep(c(T,F), each = 4, times = 4)
d2p = rep(c(T,F), each = 8, times = 2)
d2t = rep(c(T,F), each = 16)

# option a
# 1. fill missing smoke dates using temporal NN
d1a = (d0p & d2p)
# 2. assign additional trajectory-based smoke day given 0 and 1 from above
d0a = d0p | d0t
d1a = d1a | d1t
d2a = d2p | d2t

# option b
# 1. assign trajectory-based smoke day given 0 and NA on dates available and missing dates
d0b = d0p | d0t
d1b = d1t
d2b = d2p | d2t
# 2. fill the rest of missing smoke dates using temporal NN
d1b = d1b | (d0b & d2b)

# compare options
identical(d0a, d0b)
identical(d1a, d1b)
identical(d2a, d2b)

# what is different for day 1?
df = data.frame(d0p,d0t,d1t,d2p,d2t,d0a,d0b,d1a,d1b,d2a,d2b)
df[which(d1a != d1b), c("d0p", "d0t", "d1t", "d2p", "d2t", "d1a", "d1b")]

#-------------------------------------------------------------------------------
# are there missing smoke dates that trajectory is not missing on?
duration_days = 6
start_date = "20060101"
na_dates_plume = readRDS(paste0(path_dropbox, "smoke/smoke_dates_not_online.rds"))
na_dates_traj = c(format(seq.Date(ymd(start_date), ymd("20060418"), by = "day"), "%Y%m%d"),
                  readRDS(paste0(path_dropbox, "hms_hysplit/hysplit_dates_not_online.rds")),
                  readRDS(paste0(path_dropbox, "hms_hysplit/hysplit_dates_gis_corrupt.rds")),
                  readRDS(paste0(path_dropbox, "hms_hysplit/hysplit_dates_oddly_empty.rds"))) %>% 
  lapply(function(x) format(seq.Date(ymd(x), ymd(x) + days(duration_days), by = "day"), "%Y%m%d")) %>% 
  unlist() %>% 
  unique() %>% 
  sort()
dx = setdiff(na_dates_plume, na_dates_traj)

# what is the difference in smoke day for that missing smoke date?
dx0 = format(ymd(dx) - days(1), "%Y%m%d")
dx2 = format(ymd(dx) + days(1), "%Y%m%d")
# no need to go out to 2-day window
stopifnot(!(dx0 %in% na_dates_plume))
stopifnot(!(dx2 %in% na_dates_plume))
dx_range = c(dx0, dx, dx2)

min_num_traj_points = 3
traj = readRDS(paste0(path_dropbox, "hms_hysplit/10km_grid_2006-2020/grid_trajectory_points_", substr(dx, 1, 4), "_", substr(dx, 5, 6), ".rds")) %>% 
  filter(date %in% dx_range) %>% 
  mutate(smoke_day_traj = ifelse(num_traj_points_height_1 >= min_num_traj_points, 1, 0)) %>% 
  select(grid_id_10km = id_grid, date, smoke_day_traj) # NA for missing traj date
plume = readRDS(paste0(path_project, "data/3_intermediate/all_smoke_days.rds")) %>% 
  mutate(date = format(date, "%Y%m%d")) %>% 
  filter(date %in% dx_range) %>% 
  full_join(expand.grid(grid_id_10km = unique(traj$grid_id_10km), date = dx_range), by = c("grid_id_10km", "date")) %>% 
  replace_na(list(smoke_day = 0)) %>% 
  select(grid_id_10km, date, smoke_day_plume = smoke_day)
option_a = function(d, traj, plume) {
  d1 = d
  d0 = format(ymd(d1) - days(1), "%Y%m%d")
  d2 = format(ymd(d1) + days(1), "%Y%m%d")
  
  df = full_join(traj, plume, by = c("grid_id_10km", "date")) %>% arrange(grid_id_10km, date)
  
  d0p = df %>% filter(date == d0) %>% pull(smoke_day_plume)
  d0t = df %>% filter(date == d0) %>% pull(smoke_day_traj)
  d1t = df %>% filter(date == d1) %>% pull(smoke_day_traj)
  d2p = df %>% filter(date == d2) %>% pull(smoke_day_plume)
  d2t = df %>% filter(date == d2) %>% pull(smoke_day_traj)
  
  # 1. fill missing smoke dates using temporal NN
  d1a = (d0p & d2p)
  # 2. assign additional trajectory-based smoke day given 0 and 1 from above
  d0a = d0p | d0t
  d1a = d1a | d1t
  d2a = d2p | d2t
  
  return(as.integer(d1a))
}

option_b = function(d, traj, plume) {
  d1 = d
  d0 = format(ymd(d1) - days(1), "%Y%m%d")
  d2 = format(ymd(d1) + days(1), "%Y%m%d")
  
  df = full_join(traj, plume, by = c("grid_id_10km", "date")) %>% arrange(grid_id_10km, date)
  
  d0p = df %>% filter(date == d0) %>% pull(smoke_day_plume)
  d0t = df %>% filter(date == d0) %>% pull(smoke_day_traj)
  d1t = df %>% filter(date == d1) %>% pull(smoke_day_traj)
  d2p = df %>% filter(date == d2) %>% pull(smoke_day_plume)
  d2t = df %>% filter(date == d2) %>% pull(smoke_day_traj)
  
  # 1. assign trajectory-based smoke day given 0 and NA on dates available and missing dates
  d0b = d0p | d0t
  d1b = d1t
  d2b = d2p | d2t
  # 2. fill the rest of missing smoke dates using temporal NN
  d1b = d1b | (d0b & d2b)
  
  return(as.integer(d1b))
}

res_a = option_a(dx, traj, plume)
res_b = option_b(dx, traj, plume)
sum(res_a != res_b)
