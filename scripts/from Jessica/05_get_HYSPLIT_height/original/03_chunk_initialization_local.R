source("work/get_HYSPLIT_height/00_utils.R")

library(dplyr)

#-------------------------------------------------------------------------------
# Chunk Initialization Points
# Written by Jessica
# Last edited May 2021
#-------------------------------------------------------------------------------
# Set number of chunks (nodes)
chunks <- 2

# Set path and file base name for chunks
save_as <- paste0(path_dropbox, "hms_hysplit/hms_hysplit_initialization_distinct")

# Read in initialization points
dat_hms_hysplit <- readRDS(paste0(path_dropbox, "hms_hysplit/hms_hysplit_initialization_distinct.rds"))

# Split data into chunks
rows <- split_chunks(1:nrow(dat_hms_hysplit), chunks)
for (i in 1:chunks) saveRDS(dat_hms_hysplit[rows[[i]],], sprintf("%s_%s.rds", save_as, i))
