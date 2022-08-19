source("work/06_complex_MERRA_model/00_utils.R")

library(dplyr)
library(fixest)

#-------------------------------------------------------------------------------
# Fit Linear Covariate Model
# Written by Jessica
# Last edited July 2021
#-------------------------------------------------------------------------------
# Load training data
dat_train <- readRDS(paste0(path_project, "dat_train.rds"))

# Fit linear model
mdl <- feols(
  smokePM ~ aot_anom:smoke_day + smoke_day + 
    aot_anom:smoke_day:km_dist + smoke_day:km_dist + 
    aot_anom:smoke_day:elevation + smoke_day:elevation + 
    aot_anom:smoke_day:pbl_min + smoke_day:pbl_min + 
    aot_anom:smoke_day:temperature + smoke_day:temperature + 
    aot_anom:smoke_day:precipitation + smoke_day:precipitation | 
    epa_id + year + month, 
  data = dat_train
)

# Save model
saveRDS(mdl, paste0(path_results, "model_covariates_linear.rds"))
