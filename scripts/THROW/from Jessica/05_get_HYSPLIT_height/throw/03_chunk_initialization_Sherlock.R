split_chunks <- function(v, chunks) {
  return(split(v, cut(seq_along(v), chunks, labels = FALSE)))
}

library(dplyr)

#-------------------------------------------------------------------------------
# Chunk Initialization Points
# Written by Jessica
# Last edited June 2021
#-------------------------------------------------------------------------------
scratch <- Sys.getenv("SCRATCH")

# Set number of chunks (nodes)
chunks <- as.numeric(commandArgs(trailingOnly = TRUE))

# Set path and file base name for chunks
save_as <- sprintf("%s/hms_hysplit/hms_hysplit_initialization_distinct", scratch)

# Read in initialization points
dat_hms_hysplit <- readRDS(sprintf("%s/hms_hysplit/hms_hysplit_initialization_distinct.rds", scratch))

# Split data into chunks
rows <- split_chunks(1:nrow(dat_hms_hysplit), chunks)
for (i in 1:chunks) saveRDS(dat_hms_hysplit[rows[[i]],], sprintf("%s_%s.rds", save_as, i))
