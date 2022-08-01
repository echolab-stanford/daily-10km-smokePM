source("work/05_get_HYSPLIT_height/00_utils.R")

library(dplyr)
library(colorspace)
library(ggcorrplot)

#-------------------------------------------------------------------------------
# Compare Trajectory-EPA Station-Day-PM2.5 Aggregation Methods
# Written by Jessica
# Last edited July 2021
# 
# Variables of interest: 
#     height of trajectory point in meters above ground level
#     pressure of trajectory point in hPa
#     hour_along the trajectory (methods #1-3)
#     n of matched trajectory points (method #2)
# 
# Methods for aggregating:
# nn        Nearest neighbor within radius (average if multiple minima)
# idw       IDW average within radius
# k         IDW average up to k nearest neighbors within radius
# min/max   IDW average within-trajectory min height/max pressure within radius
#-------------------------------------------------------------------------------
cut <- 30
inj <- "500+1500+2500"

# Read in matched data
dat_agg <- readRDS(paste0(path_dropbox, "hms_hysplit/trajectories/trajectories_72hr_CA_2020_overlap_48hr_EPA_aggregated.rds")) %>%
  filter(cutoff == cut, injection_heights == inj)

# Select variables of interest
heights <- dat_agg %>% select(starts_with("height_"))
pressures <- dat_agg %>% select(starts_with("pressure_"))
hour_alongs <- dat_agg %>% select(starts_with("hour_along_"))
ns <- dat_agg %>% select(starts_with("n_"))

# Calculate correlations
corr_height <- cor(heights, use = "complete.obs")
corr_pressure <- cor(pressures, use = "complete.obs")
corr_hour_along <- cor(hour_alongs, use = "complete.obs")
corr_n <- cor(ns, use = "complete.obs")

# Plot correlations
colors_corr <- diverging_hcl(3)
width <- 6
height <- 6
ggcorrplot_ <- function(corr) {
  return(ggcorrplot(corr, 
                    colors = colors_corr, 
                    lab = TRUE, 
                    lab_col = "gray", 
                    lab_size = 2))
}
cp_height <- ggcorrplot_(corr_height)
cp_pressure <- ggcorrplot_(corr_pressure)
cp_hour_along <- ggcorrplot_(corr_hour_along)
cp_n <- ggcorrplot_(corr_n)

# Save correlation plots
ggsave(paste0(path_results, "corr_height.png"), cp_height, width = width, height = height)
ggsave(paste0(path_results, "corr_pressure.png"), cp_pressure, width = width, height = height)
ggsave(paste0(path_results, "corr_hour_along.png"), cp_hour_along, width = width, height = height)
ggsave(paste0(path_results, "corr_n.png"), cp_n, width = width*2/3, height = height*2/3)
