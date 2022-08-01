source("../00_init.R")
library(stringr)
library(tidyr)
library(data.table)

setwd(db_path)

########################################################################################
# Written by: Anne Driscoll
# Last edited by: Anne Driscoll 
# Pull MERRA daily and monthly grid cell means
########################################################################################


########################################################################################
# load in data and prep 
########################################################################################

years = 2006:2020

# read in data needed
merra_path = "Data/MERRA-2/total_aot/raw_merra2_aot/merra2/"
files = list.files(merra_path)
file_dates = unname(sapply(files, function(x) {substr(x, 28, 35)}))

grid_merra = raster(paste0(merra_path, files[1]))
grid = readRDS(paste0(git_path, "/data/grid.RDS"))
grid = spTransform(grid, crs(grid_merra))

smoke = readRDS(paste0(db_path, "Data/smoke/sparse_smoke_grid.RDS"))
smoke$month = as.numeric(substr(smoke$date, 5, 6))
smoke$year = as.numeric(substr(smoke$date, 1, 4))

grid$cell_m = cellFromXY(grid_merra, as.matrix(coordinates(grid)))


########################################################################################
# get the daily aot
########################################################################################

for (i in 1:length(years)) {
  
  year = years[i]
  print(year)
  
  for (j in 1:12) {
    
    month_str = str_pad(j, 2, "left", "0")
    month_files = grep(paste0("Nx.", year, month_str), files, value=T)
    month_out = as.list(rep(NA, length(month_files)))
    
    for (k in 1:length(month_files)) {
    
      cur_file = month_files[k]
      cur = raster(paste0(merra_path, cur_file))
      month_out[[k]] = data.frame(grid_id = grid$ID,
                                   date = substr(cur_file, 28, 35), 
                                   aot = cur[grid$cell_m])
    }
    
    month_out = rbindlist(month_out)
    saveRDS(month_out, paste0("Data/MERRA-2/us_grid_daily_aot/daily_grid_aot_", 
                              year, "_", j, ".RDS"))
    print(j)
  }
}


########################################################################################
# calculate the 3 year averages for each month
########################################################################################

files = list.files("Data/MERRA-2/us_grid_daily_aot")

# loop over years
for (i in 1:length(years)) {
  
  year = years[i]
  print(year)
  
  # loop over months 
  for (m in 1:12) {
    
    month = str_pad(m, 2, "left", "0")
    print(month)
    
    # get the file names that are for the appropriate month
    cur_years = paste0("daily_grid_aot_", 
                       (year-1):(year+1), "_", m, ".RDS")
    
    # if the file doesn't exist throw warning and keep going
    if (any(!cur_years %in% files)) {
      print(paste(cur_years[!cur_years %in% files], 
                  "not in files, dropping. Only using", 
                  cur_years[cur_years %in% files]))
    }
    
    # read in the files
    cur_years = paste0("Data/MERRA-2/us_grid_daily_aot/", 
                       cur_years[cur_years %in% files])
    aot_days = as.list(rep(NA, length(cur_years))) 
    for (j in 1:length(aot_days)) {
      aot_days[[j]] = readRDS(cur_years[j])
    }
    aot_days = rbindlist(aot_days)
    
    # throw out aot values on grid-days that have smoke
    cur_smoke = smoke$month==m & smoke$year >= (year-1) & smoke$year <= (year+1)
    cur_smoke = setDT(smoke[cur_smoke, ])
    names(cur_smoke)[3] = "grid_id"
    dt_aot = setDT(aot_days)
    setkey(setDT(dt_aot), date)
    aot_days = dt_aot[!cur_smoke, on = c("date", "grid_id")]
    
    # summarise to mean grid cell aot over entire month
    aot_days = aot_days %>% 
      group_by(grid_id) %>% 
      summarise(median_aot = median(aot, na.rm=T))
    
    # save the month and year in df
    aot_days$year = year
    aot_days$month = month
    
    # save the file out
    new_file = paste0("Data/MERRA-2/us_grid_3yr_arnd_means/3yr_mean_grid_aot_", 
                      year, "_", m, ".RDS")
    saveRDS(aot_days, new_file)
    print(new_file)
  }
}
