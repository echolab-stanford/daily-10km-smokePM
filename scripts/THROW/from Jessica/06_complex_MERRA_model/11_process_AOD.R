source("work/06_complex_MERRA_model/00_utils.R")

library(dplyr)
library(tidyr)

#-------------------------------------------------------------------------------
# Process AOD
# Written by Jessica
# Last edited August 2021
#-------------------------------------------------------------------------------
# Set null value from extraction script
null_value <- -999999

# Set directory paths
path_raw <- paste0(path_dropbox, "AOD/raw/")
path_processed <- paste0(path_dropbox, "AOD/processed/")

# Get raw file paths
raw_files <- list.files(path_raw, full.names = TRUE)
for (raw_file in raw_files) {
  print(paste("Working on:", basename(raw_file)))
  
  # Process raw file
  df <- read.csv(raw_file) %>% 
    select(-system.index, -.geo) %>% 
    pivot_longer(cols = starts_with("AOD_"),
                 names_to = "date",
                 names_prefix = "AOD_",
                 values_to = "aod") %>% 
    mutate(date = as.Date(date, format = "%Y.%m.%d"),
           aod = na_if(aod, null_value)) %>% 
    select(epa_id = id, lon, lat, date, aod)
  
  # Set up processed file name
  processed_file <- basename(raw_file) %>% 
    strsplit("_") %>% 
    unlist()
  processed_file <- paste(processed_file[1:5], collapse = "_")
  processed_file <- paste0(path_processed, processed_file, ".rds")
  
  # Save processed file
  saveRDS(df, processed_file)
}

#-------------------------------------------------------------------------------
#### Merge AOD into EPA panel ####
# Load AOD
processed_files <- list.files(path_processed, full.names = TRUE)
dat_aod <- processed_files %>% 
  lapply(readRDS) %>% 
  bind_rows()

# Load EPA panel
panel_file <- paste0(path_dropbox, "PM25/epa_station_smokePM_full_panel_extendedCovariates.rds")
dat_panel <- readRDS(panel_file)

# Merge
dat_merged <- left_join(dat_panel, dat_aod)

# Save
saveRDS(dat_merged, gsub(".rds", "_AOD.rds", panel_file))
