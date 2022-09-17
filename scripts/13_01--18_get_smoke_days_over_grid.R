try(log_file <- file(paste0("~/Desktop/grid_smoke_day_", format(Sys.time(), "%Y-%m-%d-%H-%M-%OS"), ".log"), open = "at"))
sink(log_file, type="output", append = TRUE, split = TRUE)
sink(log_file, type="message", append = TRUE)

source("~/Documents/GitHub/smoke_PM_prediction/work/05_get_HYSPLIT_height/00_utils.R")
# source("../00_utils.R")
db_path = "~/BurkeLab Dropbox/"
git_path = "~/Documents/GitHub/purple-air-infiltration"
library(sp)
library(rgeos)
library(sf)
library(dplyr)
library(lubridate)

########################################################################################
# Modified by Jessica from Anne's code
# Gets a count of plumes overhead at the grid level for 2006-2020
########################################################################################

start_month_str = "2006-01"
start_month = ym(start_month_str)
end_month_str = "2020-12"
end_month = ym(end_month_str)
year_months = seq.Date(start_month, end_month, by = "month") %>% format("%Y-%m")

###############################################################
# Read in data
###############################################################

# Original smoke data at: https://www.ospo.noaa.gov/Products/land/hms.html
# here all the individual day files have been processed and combined
smoke = readRDS(paste0(db_path, "/Data/smoke/smoke_plumes_sfdf.RDS"))
smoke = smoke %>% 
  select(-Start, -End) %>% 
  mutate(date = ymd(date),
         year = year(date),
         month = month(date),
         Density = as.numeric(gsub(".000", "", Density)),
         id_plume = row_number())

# get grid over which to query for smoke
project_grid = readRDS(paste0(git_path, "/data/grid.RDS"))
project_grid = gBuffer(project_grid, byid=T, width=5000, capStyle="SQUARE") #slow
project_grid = st_as_sf(project_grid) %>% rename(id_grid = ID)
project_grid = project_grid %>% st_transform(st_crs(smoke))

# details about dates
dates_not_online = ymd(readRDS(paste0(db_path, "/Data/smoke/smoke_dates_not_online.rds")))
dates_empty_data = ymd(readRDS(paste0(db_path, "/Data/smoke/smoke_dates_empty_data.rds")))
dates_repaired_geometry = ymd(readRDS(paste0(db_path, "/Data/smoke/smoke_dates_repaired_geometry.rds")))
dates_density = ymd(readRDS(paste0(db_path, "/Data/smoke/smoke_dates_density.rds")))
# first_density_date = ymd("2007-08-13")

###############################################################
# Process smoke
###############################################################

#get the plumes over each grid id for all time
for (year_month in year_months) {
  # Takes ~1-2 minutes per month
  print(paste(year_month, "---------------------------------------------------------"))
  start_time <- get_start_time()
  
  # Get year, month, and days
  y <- substr(year_month, 1, 4)
  m <- substr(year_month, 6, 7)
  dm <- days_in_month(as.numeric(m))
  if (leap_year(as.numeric(y)) & (m == "02")) dm <- dm + 1
  days <- seq.Date(ymd(paste0(year_month, "-01")), 
                   ymd(paste0(year_month, "-", dm)), 
                   "day")
  
  # Get smoke day, plume count, and density if available
  out_month <- vector("list", length(days))
  # progress = txtProgressBar(max = length(days), style = 3)
  for (d in 1:length(days)) {
    # post = days[d] >= first_density_date
    
    # Initialize output
    plumes <- data.frame(id_grid = project_grid$id_grid,
                         date = days[d], 
                         smoke_day = NA,
                         total = NA,
                         light = NA,
                         medium = NA,
                         dense = NA,
                         note_smoke_date_not_online = days[d] %in% dates_not_online,
                         note_smoke_date_empty_data = days[d] %in% dates_empty_data,
                         note_smoke_date_repaired_geometry = days[d] %in% dates_repaired_geometry)
    
    # Limit to plumes that day
    cur_smoke <- smoke[smoke$date == days[d], ]
    if (nrow(cur_smoke) == 0) {
      plumes = plumes %>% 
        mutate(across(c(smoke_day, total), ~ifelse(note_smoke_date_empty_data, 0, .x)),
               across(c(light, medium, dense), ~ifelse(note_smoke_date_empty_data & (date %in% dates_density), 0, .x)))
    } else if (nrow(cur_smoke) > 0) {
      # Find intersecting plumes and aggregate
      overlap = project_grid %>% 
        st_join(cur_smoke) %>% 
        st_drop_geometry() %>% 
        mutate(date = days[d]) %>% 
        group_by(id_grid, date) %>% 
        summarize(total = sum(!is.na(id_plume)),
                  smoke_day = ifelse(total > 0, 1, 0),
                  light = sum(Density == 5, na.rm = TRUE),
                  medium = sum(Density == 16, na.rm = TRUE),
                  dense = sum(Density == 27, na.rm = TRUE)) %>% 
        ungroup() %>% 
        mutate(across(c(light, medium, dense), 
                      ~ifelse(!(date %in% dates_density) | 
                                ((date %in% dates_density) & 
                                   (total > 0) &
                                   (light == 0) &
                                   (medium == 0) &
                                   (dense == 0)), NA, .x)))
      
      plumes = plumes %>% 
        select(-smoke_day, -total, -light, -medium, -dense) %>% 
        left_join(overlap) %>% 
        select(id_grid, date, smoke_day, total, light, medium, dense, 
               note_smoke_date_not_online, note_smoke_date_empty_data,
               note_smoke_date_repaired_geometry)
    }
    # Append to list for the month
    out_month[[d]] <- plumes
    # setTxtProgressBar(progress, d)
  }
  # Save
  out_month <- out_month %>% bind_rows() %>% arrange(id_grid, date)
  saveRDS(out_month, paste0(path_dropbox, "smoke/10km_grid/", 
                            sprintf("grid_smoke_day_%s_%s.rds", y, m)))
  
  print_time(start_time)
}

sink(type = "output")
sink(type = "message")
close(log_file)
