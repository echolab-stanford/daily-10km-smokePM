# Output directory where trajectories get stored
out_dir <- "/Users/jessssli/Downloads/my_dir/my_out_dir/"
# Meteorology directory
met_dir <- "/Users/jessssli/Downloads/my_dir/my_met_dir/"
# Directory in which working directories will be made
exec_dir <- "/Users/jessssli/Downloads/my_dir/my_exec_dir/"



x <- paste0(exec_dir, "/1") %>% 
  lapply(list.files, full.names = TRUE, recursive = TRUE) %>% 
  unlist()
y <- paste0(exec_dir, "/1") %>% 
  lapply(list.files, full.names = TRUE) %>% 
  lapply(list.files, full.names = TRUE) %>% 
  unlist()


dat_hms_hysplit <- dat_hms_hysplit %>% 
  map_dfr(st_drop_geometry) %>% 
  select(-Lon, -Lat, -Date, -Time, -Duration) %>% 
  setNames(c("id", "lon", "lat", "YYYYmmDD", "HHMM", "duration")) %>%
  mutate(id_unique = paste(lon, lat, YYYYmmDD, HHMM, id)) %>% 
  separate(YYYYmmDD, c("year", "month", "day"), c(4, 6)) %>% 
  separate(HHMM, c("hour", "minute"), 2) %>% 
  mutate(hour = as.numeric(hour), 
         minute = as.numeric(minute),
         height = 500) ### NEED INJECTION HEIGHTS



# Get rid of duplicate starting points
discard_duplicates <- function(df) df %>% select(-X..ID) %>% distinct()
dat_hms_hysplit <- dat_hms_hysplit %>% lapply(discard_duplicates)






seq(length(dat_hms_hysplit))[(lapply(dat_hms_hysplit, names) %>% lapply(length) %>% unlist()) == 6]

dat_hms_hysplit[(lapply(dat_hms_hysplit, names) %>% lapply(length) %>% unlist()) == 6][[1]]
"20161020"

dat_hms_hysplit[[2724]]




# Column names suddenly change format from 20161020 onwards (row 450705)
# dat_hms_hysplit[(lapply(dat_hms_hysplit, names) %>% lapply(length) %>% unlist()) == 6][[1]]

# Do we skip a year at this format change? Not necessarily

dat_hms_hysplit <- dat_hms_hysplit %>% 
  map_dfr(st_drop_geometry) %>% 
  mutate(date = ifelse(is.na(Date), X.YearMmDd, Date)) %>% 
  mutate(date = as.Date(as.character(date), format = "%Y%m%d"))

df <- count(dat_hms_hysplit, date)
ggplot() +
  geom_point(data = df, aes(date, n), size=0.001) +
  coord_cartesian(ylim = c(0, 500))



dat_hms_hysplit %>% 
  map_dfr(st_drop_geometry) %>% 
  filter(X.YearMmDd == 20110101 | Date == 20161020) %>% 
  slice_sample(n=10) %>% 
  arrange(X.YearMmDd)%>% 
  mutate(lon = ifelse(is.na(Lon), X......LON, Lon),
         lat = ifelse(is.na(Lat), X....LAT, Lat),
         YYYYmmdd = ifelse(is.na(Date), X.YearMmDd, Date),
         HHMM = ifelse(is.na(Time), X.HhMm, Time),
         duration = ifelse(is.na(Duration), X.Dura, Duration))



# 01
library(plyr)
library(dplyr)
library(foreach)
library(doParallel)
library(splitr)
library(ff)

# 02
library(sf)
library(dplyr)
library(tidyr)
library(purrr)
library(foreach)
library(doParallel)




# calculate runtime in hours per trajectory-hour
t_ran <- (8*60+57)/60/60
traj_ran <- 80
dur_ran <- sum(dat_hms_hysplit$duration[1:80])
traj_ttl <- nrow(dat_hms_hysplit)
dur_ttl <- sum(dat_hms_hysplit$duration)
# t_ran/(traj_ran*dur_ran)*(traj_ttl*dur_ttl) # integer overflow
t_ran/(traj_ran*dur_ran)*traj_ttl*dur_ttl # this seems wrong
t_ran/dur_ran*dur_ttl # 3516 hours --> 146.5 days!





which(is.na(dat_hms_hysplit), arr.ind=TRUE) %>% 
  mutate() %>% 
  pivot_wider(id_cols = row, )


which(is.na(dat_hms_hysplit), arr.ind=TRUE)[,1] %>% unique


df1 <- readRDS(paste0(path_dropbox, "hms_hysplit/hms_hysplit_points.rds"))
df1[[480]]
df1[[1]]
df1[[480]]
df1[[479]]
dat_hms_hysplit[480,]
df1[[481]]
df1[[480]]
which(is.na(dat_hms_hysplit), arr.ind=TRUE)[,1] %>% unique
which(is.na(dat_hms_hysplit), arr.ind=TRUE)[,1] %>% unique %>% tail
df1[[501448]]
dat_hms_hysplit[501448,]
library(sf)
df1 %>% map_dfr(st_drop_geoemetry)
library(purrr)
df1 %>% map_dfr(st_drop_geoemetry)
library(sp)
df1 %>% map_dfr(st_drop_geoemetry)
library(tidyr)
df1 %>% map_dfr(st_drop_geometry)
df1 %>% map_dfr(st_drop_geometry) %>% filter(!is.na(Lon))
df1 %>% map_dfr(st_drop_geometry) %>% filter(!is.na(Lon), !is.na(X.Dura))
df1 %>% map_dfr(st_drop_geometry) %>% filter(is.na(Lon), is.na(X.Dura))
?is.na
df1 %>% map_dfr(st_drop_geometry) %>% filter(!is.na(X..ID), is.na(X......LON))
df1 %>% map_dfr(st_drop_geometry) %>% filter(!is.na(X..ID), is.na(X.Dura))
View(dat_hms_hysplit)
df2 <- df1 %>% map_dfr(st_drop_geometry)
View(df2)
df3 <- df1[[480]]
df3
df4 <- df1[[479]]
df5 <- df1[[481]]
df6 <- list(df4, df3, df5)
df7 <- df6 %>% map_dfr(st_drop_geometry)
df7
View(df7)
anyNA(df7)
df8 <- df7 %>% mutate(lon = ifelse(is.na(Lon), X......LON, Lon),
                      lat = ifelse(is.na(Lat), X....LAT, Lat),
                      YYYYmmdd = ifelse(is.na(Date), X.YearMmDd, Date),
                      HHMM = ifelse(is.na(Time), X.HhMm, Time),
                      duration = ifelse(is.na(Duration), X.Dura, Duration),
                      YYYYmmddHHMM = paste(YYYYmmdd, HHMM),
                      YYYYmmddHHMM = YYYYmmddHHMM %>%
                        as.POSIXct(tz = "UTC", format = "%Y%m%d %H%M") %>%
                        # splitr does not take minutes as input, so round to nearest hour
                        round(units = "hours") %>%
                        strftime(format = "%Y-%m-%d-%H-%M"))

df1 %>%
  map_dfr(st_drop_geometry) %>%
  filter(X.YearMmDd == 20110101 | Date == 20161020) %>%
  slice_sample(n=10) %>%
  arrange(X.YearMmDd)
df1[[2724]]

df6 <- list(df4, df3, df5, df1[[2724]])
df6
df7 <- df6 %>% map_dfr(st_drop_geometry)
df7
View(df7)
df8 <- df7 %>% mutate(lon = ifelse(is.na(Lon), X......LON, Lon),
                      lat = ifelse(is.na(Lat), X....LAT, Lat),
                      YYYYmmdd = ifelse(is.na(Date), X.YearMmDd, Date),
                      HHMM = ifelse(is.na(Time), X.HhMm, Time),
                      duration = ifelse(is.na(Duration), X.Dura, Duration),
                      YYYYmmddHHMM = paste(YYYYmmdd, HHMM),
                      YYYYmmddHHMM = YYYYmmddHHMM %>%
                        as.POSIXct(tz = "UTC", format = "%Y%m%d %H%M") %>%
                        # splitr does not take minutes as input, so round to nearest hour
                        round(units = "hours") %>%
                        strftime(format = "%Y-%m-%d-%H-%M"))

View(df8)
1845/24
2400/24
2400/60
df9 <- df8%>%
  separate(YYYYmmddHHMM, c("year", "month", "day", "hour", "minute"))
View(df9)
df10 <- df9 %>%
  mutate(ymd = paste(year, month, day, sep = "-"),
         across(c(year, month, day, hour, minute), as.numeric),
         height = 500)
View(df10)
df1[[481]]
df1[[479]]

View(df9)
View(df8)
df1[[480]]
df1[[481]]
df1[[481]]$X.HhMm
df1[[481]]$X.HhMm %>% class
df1[[1]]
df1[[481]]$X.HhMm
# df1[[481]]$X.HhMm has 829 as a value. Seems weird since most times are by 15 
# minute intervals

# df1[[481]] and df1[[2724]], and briefly df[[479]], all get NA for year,
# month, day, hour, minute, and NA-NA-NA for ymd because X.HhMm or Time
# i.e. HHMM is less than 4 digits (e.g. 829, 30, and 45 respectively)

df11 <- df1 %>% 
  # Discard geometry
  map_dfr(st_drop_geometry) %>% 
  mutate(
    # From 2016/10/20 onwards, data gets recorded under different names
    # So unify columns representing same variable but named differently
    lon = ifelse(is.na(Lon), X......LON, Lon),
    lat = ifelse(is.na(Lat), X....LAT, Lat),
    YYYYmmdd = ifelse(is.na(Date), X.YearMmDd, Date),
    HHMM = ifelse(is.na(Time), X.HhMm, Time),
    duration = ifelse(is.na(Duration), X.Dura, Duration),
    # splitr does not take minutes as input, so round to nearest hour
    YYYYmmddHHMM = paste(YYYYmmdd, HHMM),
    YYYYmmddHHMM = YYYYmmddHHMM %>% 
      as.POSIXct(tz = "UTC", format = "%Y%m%d %H%M") %>% 
      round(units = "hours") %>% 
      strftime(format = "%Y-%m-%d-%H-%M")
  ) %>% 
  # Reformat date and time columns
  separate(YYYYmmddHHMM, c("year", "month", "day", "hour", "minute")) %>% 
  mutate(ymd = paste(year, month, day, sep = "-"),
         across(c(year, month, day, hour, minute), as.numeric),
         # Specify injection height
         height = 500)

df11 %>% filter(HHMM < 1000) %>% pull(HHMM) %>% unique() %>% sort()
df11 %>% filter(HHMM > 1000) %>% pull(HHMM) %>% unique() %>% sort()

# looking at all the unique values of HHMM, it seems 15 minute intervals
# is the exception and not the rule

df11 %>% filter(HHMM == 0) %>% View()

df12 <- df1 %>% 
  # Discard geometry
  map_dfr(st_drop_geometry) %>% 
  mutate(
    # From 2016/10/20 onwards, data gets recorded under different names
    # So unify columns representing same variable but named differently
    lon = ifelse(is.na(Lon), X......LON, Lon),
    lat = ifelse(is.na(Lat), X....LAT, Lat),
    YYYYmmdd = ifelse(is.na(Date), X.YearMmDd, Date),
    HHMM = ifelse(is.na(Time), X.HhMm, Time),
    duration = ifelse(is.na(Duration), X.Dura, Duration),
    # splitr does not take minutes as input, so round to nearest hour
    HHMM = case_when(0 <= HHMM & HHMM < 10 ~ paste0("000", HHMM),
                     10 <= HHMM & HHMM < 100 ~ paste0("00", HHMM),
                     100 <= HHMM & HHMM < 1000 ~ paste0("0", HHMM),
                     TRUE ~ as.character(HHMM)),
    YYYYmmddHHMM = paste(YYYYmmdd, HHMM),
    YYYYmmddHHMM = YYYYmmddHHMM %>% 
      as.POSIXct(tz = "UTC", format = "%Y%m%d %H%M") %>% 
      round(units = "hours") %>% 
      strftime(format = "%Y-%m-%d-%H-%M")
  ) %>% 
  # Reformat date and time columns
  separate(YYYYmmddHHMM, c("year", "month", "day", "hour", "minute")) %>% 
  mutate(ymd = paste(year, month, day, sep = "-"),
         across(c(year, month, day, hour, minute), as.numeric),
         # Specify injection height
         height = 500)









# Get points in California
library(dplyr)
library(tigris)
library(ggplot2)

usa <- states()
california <- usa %>% filter(NAME == "California")
california$geometry


ggplot() +
  geom_sf(data = usa)

ggplot() +
  geom_sf(data = california) + 
  geom_point(data = dat_hms_hysplit %>% head(200), mapping = aes(lon, lat))

library(sp)

hysplit_points <- SpatialPoints(dat_hms_hysplit[c("lon", "lat")])
california_sp <- SpatialPolygons(as(california, "Spatial")@polygons)
o <- over(california_sp@polygons, hysplit_points)
hysplit_points_ca <- hysplit_points[california_sp]

ggplot() +
  geom_sf(data = california) + 
  geom_point(data = hysplit_points_ca@coords %>% as.data.frame(), 
             mapping = aes(lon, lat),
             size = 0.2,
             alpha = 0.5)

# Make column for states
cawa <- usa %>% filter(NAME %in% c("California", "Washington"))
ggplot() +
  geom_sf(data = cawa)

cawa_sp <- as(cawa, "Spatial")
cawa_sp_df <- SpatialPolygonsDataFrame(SpatialPolygons(cawa_sp@polygons), 
                                       cawa_sp@data %>% select(NAME))
o <- over(hysplit_points, cawa_sp_df)
ggplot() +
  geom_sf(data = cawa) + 
  geom_point(data = hysplit_points[SpatialPolygons(cawa_sp@polygons)] %>% 
               as.data.frame(),
             mapping = aes(lon, lat),
             size = 0.2,
             alpha = 0.5)





usa <- map_data("usa")
ggplot() + 
  # geom_polygon(data = usa, mapping = aes(long, lat)) + 
  # coord_fixed(1.3) + 
  geom_point(data = dat_hms_hysplit %>% filter(is.na(state)),
             mapping = aes(lon, lat),
             size = 0.2,
             alpha = 0.5)


hist(dat_hms_hysplit %>% 
filter(state == "California") %>% 
pull(ymd) %>% 
  as.POSIXct(tz = "UTC", format = "%Y-%m-%d"))

df <- data.frame(dates = dat_hms_hysplit %>% 
                   filter(state == "California") %>% 
                   pull(ymd) %>% 
                   as.POSIXct(tz = "UTC", format = "%Y-%m-%d"))
library(ggpubr)
gghistogram(data = df, 
            x = "dates", 
            bins = 100) + 
  labs(title = "California HYSPLIT Points",
       caption = paste("Earliest date:", min(df$dates), "\n",
                       "Latest date:", max(df$dates)))

df <- data.frame(dates = dat_hms_hysplit %>% 
                   # filter(state == "California") %>% 
                   pull(ymd) %>% 
                   as.POSIXct(tz = "UTC", format = "%Y-%m-%d"))
gghistogram(data = df, 
            x = "dates", 
            bins = 100) + 
  labs(title = "HYSPLIT Points",
       caption = paste("Earliest date:", min(df$dates), "\n",
                       "Latest date:", max(df$dates)))


# dat_hms_hysplit as original list
df1 <- df %>% 
  group_by(ymd) %>% 
  mutate(id = cur_group_id())
df1 %>% 
  filter(year == 2019) %>% 
  pull(id)
df1 %>% filter(year == 2019) %>% pull(ymd)
dat_hms_hysplit[2716:3000]
dat_hms_hysplit[3184]



# dat_hms_hysplit as data frame
# non-US
nrow(dat_hms_hysplit %>% filter(is.na(state)))
# US
nrow(dat_hms_hysplit %>% filter(!is.na(state)))
# hours
sum(dat_hms_hysplit %>% pull(duration))
sum(dat_hms_hysplit %>% filter(is.na(state)) %>% pull(duration))
sum(dat_hms_hysplit %>% filter(!is.na(state)) %>% pull(duration))
sum(dat_hms_hysplit %>% head(100) %>% pull(duration))
length(unique(dat_hms_hysplit %>% head(100) %>% pull(ymd)))

df1 <- dat_hms_hysplit %>% 
  group_by(ymd) %>% 
  mutate(id = cur_group_id())
nrow(df1 %>% filter(id %in% 1:4))



# dat_hms_hysplit as list
names(dat_hms_hysplit) %>% 
  # substr(1, 6) %>% 
  unique() %>% 
  sort() %>% 
  tail()
dat_hms_hysplit %>% 
  lapply(nrow) %>% 
  unlist() %>% 
  stack() %>% 
  rename(date = ind, rows = values) %>% 
  # arrange(desc(date)) %>% 
  # head(20)
  filter(date == "20191101")


dat_hms_hysplit %>% 
  lapply(names) %>% 
  unlist() %>% 
  unique()





# searching for semi-duplicates

# Read in HYSPLIT points in NOAA HMS data
dat_hms_hysplit <- readRDS(paste0(path_dropbox, "hms_hysplit/hms_hysplit_points.rds"))
df1 <- dat_hms_hysplit %>%
# Discard geometry
map_dfr(st_drop_geometry)
df2 <- df1%>%
rename(id = ID, lon = Lon, lat = Lat, date = Date, time = Time, duration = Duration)
df3 <- df2%>%
# There are 6 rows w/ Date = 200000 and Duration = 0
# Throw away for now, but would be good to look into
filter(nchar(date) == 8 & duration > 0)
View(df2)
View(df3)
df4 <- df3 %>%
# Discard duplicates that have different ID but same data
select(-id) %>%
distinct()
View(df4)
df0 <- dat_hms_hysplit %>%
# Discard geometry
map_dfr(st_drop_geometry) %>%
mutate(
# splitr does not take minutes as input, so round to nearest hour
Time = case_when(0 <= Time & Time < 10 ~ paste0("000", Time),
10 <= Time & Time < 100 ~ paste0("00", Time),
100 <= Time & Time < 1000 ~ paste0("0", Time),
TRUE ~ as.character(Time)),
DateTime = paste(Date, Time) %>%
as.POSIXct(tz = "UTC", format = "%Y%m%d %H%M") %>%
round(units = "hours") %>%
strftime(format = "%Y-%m-%d-%H-%M")
) %>%
# Reformat date and time columns
separate(DateTime, c("year", "month", "day", "hour", "minute")) %>%
mutate(ymd = paste(year, month, day, sep = "-"),
across(c(year, month, day, hour, minute), as.numeric),
# Specify injection height
height = 500) %>% ### MULTIPLE INJECTION HEIGHTS?
select(Lon, Lat, height, year, month, day, hour, minute, ymd, Duration) %>%
rename(lon = Lon, lat = Lat, duration = Duration) %>%
# duration is recorded as HHMM
mutate(duration = duration/100) %>%
# Discard duplicates that have different ID but same data
# Also discard points that rounded to the same date-hour
distinct()
rm(df0)
df5 <- df4%>%
# Redefine HYSPLIT points based on duration following Brey et al. (2018)
mutate(
# Get date-time
time = case_when(0 <= time & time < 10 ~ paste0("000", time),
10 <= time & time < 100 ~ paste0("00", time),
100 <= time & time < 1000 ~ paste0("0", time),
TRUE ~ as.character(time)),
datetime_0 = paste(date, time) %>% as.POSIXct(tz = "UTC", format = "%Y%m%d %H%M"),
# Convert duration from HHMM to hours
duration = duration/100,
# Unlike Brey et al., we have durations beyond 24 hours, so slightly modify
# initialization percentiles to be evenly distanced
seconds = duration*60*60,
denominator = (function(x) ifelse(x < 0, 0, x))(duration - 1) %/% 6 + 2,
numerator_1 = 1,
numerator_2 = ifelse(0 <= duration & duration <= 6,  NA, 2),
numerator_3 = ifelse(0 <= duration & duration <= 12, NA, 3),
numerator_4 = ifelse(0 <= duration & duration <= 18, NA, 4),
numerator_5 = ifelse(0 <= duration & duration <= 24, NA, 5),
numerator_6 = ifelse(0 <= duration & duration <= 30, NA, 6),
datetime_1 = datetime_0 + seconds * numerator_1 / denominator,
datetime_2 = datetime_0 + seconds * numerator_2 / denominator,
datetime_3 = datetime_0 + seconds * numerator_3 / denominator,
datetime_4 = datetime_0 + seconds * numerator_4 / denominator,
datetime_5 = datetime_0 + seconds * numerator_5 / denominator,
datetime_6 = datetime_0 + seconds * numerator_6 / denominator
)
View(df5)
anyNA(df5$datetime_1)
df6 <- df5%>%
pivot_longer(cols = starts_with("datetime"),
names_to = "initialization_along",
names_prefix = "datetime_",
values_to = "datetime")
View(df6)
750292*7
df7 <- df6 %>%
filter(initialization_along != 0)
5252044*6/7
df8 <- df7 %>%
drop_na(datetime)
hist(df7$duration)
df9 <- df8 %>%
# splitr does not take minutes as input, so round to nearest hour
mutate(datetime = datetime %>%
round(units = "hours") %>%
strftime(format = "%Y-%m-%d-%H-%M"))
anyNA(df9)
anyNA(df8)
View(df9)
anyNA(df9 %>% select(lon, lat, datetime))
View(df9)
unique(length(df9$datetime))
unique(nchar(df9$datetime))
df10 <- df9  %>%
# Reformat date and time columns
separate(datetime, c("year", "month", "day", "hour", "minute"))
View(df10)
?separate
(df10 <- df9  %>%
# Reformat date and time columns
separate(datetime, c("year", "month", "day", "hour", "minute"))) %>% system.time()
(df10 <- df9  %>%
# Reformat date and time columns
separate(datetime, c("year", "month", "day", "hour", "minute"), sep = "-")) %>% system.time()
View(df10)
df11 <- df10 %>%
mutate(ymd = paste(year, month, day, sep = "-"),
across(c(year, month, day, hour, minute), as.numeric),
# Specify injection height
height = 500)
View(df11)
View(df10)
df12 <- df11 %>% ### MULTIPLE INJECTION HEIGHTS?
select(lon, lat, height, year, month, day, hour, minute, ymd)
View(df12)
View(df11)
6*7*24
df13 <- df12 %>%
# Discard points that rounded to the same date-time
distinct()
View(df13)
View(df12)
View(df12 %>% arrange(lon, lat, ymd))
View(setdiff(df12, df13))
?duplicated
duplicated(data.frame(x=c(1,1,2), y = c(2,2,3)))
data.frame(x=c(1,1,2), y = c(2,2,3))
View(duplicated(df12))
View(df12[duplicated(df12),])
View(df11)
print_unique(df11)
?file.move
library(ff)
?file.move
find_dups <- function(df) {
return(df[duplicated(df),])
}
find_dups(df12)
View(df11[duplicated(df12),])
View(df10 %>% filter(lon == -141.084, lat == 66.017))
View(df10 %>% filter(lon == -141.084, lat == 66.017, date == 20100604, time = "1119", duration == 24))
View(df10 %>% filter(lon == -141.084, lat == 66.017, date == 20100604, time == "1119", duration == 24))
View(df10 %>% filter(lon == -141.084, lat == 66.017, year == 2010, month == 6, day == 4, hour == 16))
View(df10 %>% filter(lon == -141.084, lat == 66.017, year == "2010", month == "06", day == "04", hour == "16"))



# checking file move
file.move("~/Downloads/my_dir/my_exec_dir/1/traj-my_traj_2-fwd-11-01-02-20-1lat_30p885_lon_-112p795-hgt_500-200h", "~/Downloads/my_dir/my_out_dir/")
?file.move
file.move("~/Downloads/my_dir/my_exec_dir/1/traj-my_traj_2-fwd-11-01-02-20-1lat_30p776_lon_-112p939-hgt_500-400h", "~/Downloads/my_dir/my_out_dir/")
c("~/Downloads/my_dir/my_exec_dir/1/traj-my_traj_2-fwd-11-01-02-20-1lat_30p885_lon_-112p795-hgt_500-200h", "~/Downloads/my_dir/my_exec_dir/2/traj-my_traj_2-fwd-11-01-02-20-1lat_30p885_lon_-112p795-hgt_500-200h") %>% lapply(file.move, "~/Downloads/my_dir/my_out_dir/")
source("work/get_HYSPLIT_height/00_utils.R")










# searching for exact duplicates (even id same)

# Read in HYSPLIT points in NOAA HMS data
dat_hms_hysplit <- readRDS(paste0(path_dropbox, "hms_hysplit/hms_hysplit_points.rds"))
#-------------------------------------------------------------------------------
#### Restructure HMS data ####
dat_hms_hysplit <- dat_hms_hysplit %>%
# Discard geometry
map_dfr(st_drop_geometry) %>%
rename(id = ID, lon = Lon, lat = Lat, date = Date, time = Time, duration = Duration)
nrow(find_dups(dat_hms_hysplit))
View(find_dups(dat_hms_hysplit))
View(dat_hms_hysplit)
View(dat_hms_hysplit %>% filter(date == 20151027, ))
View(dat_hms_hysplit %>% filter(date == 20151027, time == 1945, duration == 400, lon == -99.678, lat == 30.959))
View(dat_hms_hysplit %>% filter(date == 20151027)) %>% arrange(id)
View(dat_hms_hysplit %>% filter(date == 20151027) %>% arrange(id))

# 2015-10-28 contains duplicate values from 2015-10-27
dat_hms_hysplit <- readRDS(paste0(path_dropbox, "hms_hysplit/hms_hysplit_points.rds"))
dat_hms_hysplit$`20151026`
dat_hms_hysplit$`20151027`
dat_hms_hysplit$`20151028`
dat_hms_hysplit$`20151029`
# 2015-10-27 contains values from 2015-10-28
View(dat_hms_hysplit$`20151027`)
# 2015-10-27 contains values from 2015-10-27, 28, and 29
View(dat_hms_hysplit$`20151028`)





# seems like some US points get excluded by US states shape?
# doesn't look like it in the map
usa <- map_data("usa")
ggplot() +
  geom_polygon(data = usa, mapping = aes(long, lat), fill = "lightblue") + 
  coord_fixed(1.3) + 
  geom_point(data = dat_hms_hysplit %>% filter(is.na(state)), 
             mapping = aes(lon, lat))
# nvm, it was due to tibble rounding
dat_hms_hysplit %>% filter(is.na(state)) %>% head(100) %>% View()










# GADM more precise than rnaturalearth and broader coverage than tigris?
ggplot()+
  geom_polygon(data = map_data("usa"), mapping = aes(long, lat), fill = "lightblue")+
  coord_fixed(1.3)+
  geom_point(data = df %>% filter(is.na(country)), mapping = aes(lon, lat))


countries <- readOGR("~/BurkeLab Dropbox/Data/boundaries/gadm/gadm36_0/gadm36_0.shp")
countries <- SpatialPolygonsDataFrame(SpatialPolygons(countries@polygons), 
                                      countries@data)
o <- over(SpatialPoints(hysplit_points), countries)
df <- dat_hms_hysplit %>% 
  left_join(bind_cols(hysplit_points, o)) %>% 
  rename(country = NAME_0)



library(raster)
states <- getData(country="USA", level=1)
provinces <- getData(country="Canada", level=1)
provinces <- SpatialPolygonsDataFrame(SpatialPolygons(provinces@polygons),
                                      provinces@data)
x <- list.files(paste0(path_dropbox, "boundaries/gadm_rds/"), full.names = TRUE) %>% 
  lapply(readRDS) %>% 
  bind()
x <- SpatialPolygonsDataFrame(SpatialPolygons(x@polygons),
                              x@data %>% select(NAME_0, NAME_1))
o <- over(SpatialPoints(hysplit_points), x)


o <- over(hp[1:10,], provinces)

#### Make columns for country and state ####
hysplit_points <- dat_hms_hysplit[c("lon", "lat")] %>% distinct()

# Country
countries <- SpatialPolygonsDataFrame(SpatialPolygons(countries110@polygons), 
                                      countries110@data)
o <- over(SpatialPoints(hysplit_points), countries)
unique(o$name_long)
unique(o$admin)
unique(o$adm0_a3)

















library(dplyr)
library(ggplot2)

dat_hms_hysplit <- readRDS(paste0(path_dropbox, "hms_hysplit/hms_hysplit_initialization_distinct_gadm.rds"))

glimpse(dat_hms_hysplit)
anyNA(dat_hms_hysplit)
find_na_cols(dat_hms_hysplit,T)
nrow(find_na_rows(dat_hms_hysplit))
df <- dat_hms_hysplit %>% filter(is.na(country) | is.na(state))

usa <- map_data("usa")
ggplot() +
  geom_polygon(data = usa, mapping = aes(long, lat), fill = "lightblue") +
  coord_fixed(1.3) +
  geom_point(data = df, mapping = aes(lon, lat))














library(dplyr)
library(tidyr)
library(splitr)
library(ff)

traj_files <- list.files("/Users/jessssli/BurkeLab Dropbox/Jessica Li/core13", 
                         full.names = TRUE, recursive = TRUE)
fail <- c()
for (t in traj_files) {
  traj <- trajectory_read(t)
  if (nrow(traj) != 73) {
    fail <- c(fail, t)
  }
}

x <- trajectory_read("/Users/jessssli/BurkeLab Dropbox/Jessica Li/core13/_13/")
x[x$hour_along == 0, "mycol"] <- 1:nrow(x[x$hour_along == 0,])
x$mycol <- na.locf(x$mycol)



traj_files <- list.files("/Users/jessssli/BurkeLab Dropbox/Jessica Li/core13", 
                         recursive = TRUE)
traj_files %>% 
  lapply()




x[x$hour_along == 0, "mycol"] <- 1:nrow(x[x$hour_along == 0,])
for (i in x$mycol) {
  
}
x %>% 
  filter()






x <- c(1,NA,NA,NA,2,NA,NA,NA,NA,3)
fill_nums <- function(v) {
  l <- length(v)
  print(l)
  print(v)
  if (l == 1) {
    print("voila")
    print(l)
    return(v)
  }
  fill_nums(v[1:(l-1)])
}

fill_nums <- function(v) {
  l <- length(v)
  if (is.na(v[l])) {
    v[l]
  }
  if (l == 1) {
    return(0)
  }
  fill_nums(v[1:(l-1)])
}

fill_nums <- function(v) {
  l <- length(v)
  if (!is.na(v[l])) {
    return(v[l])
  } else {
    fill_nums(v[1:(l-1)])
  }
}
which(x)


recursive_fn <- function(x) {
  print(x)
  if (x == 0) {
    return(x)
  }
  recursive_fn(x - 1)
}

