source("work/06_complex_MERRA_model/00_utils.R")

library(dplyr)
library(mlr)
library(ranger)

#-------------------------------------------------------------------------------
# Fit Random Forest Covariate Model
# Written by Jessica
# Last edited July 2021
#-------------------------------------------------------------------------------
# Load training data
dat_train_full <- readRDS(paste0(path_project, "dat_train.rds"))

# Select features
dat_train <- dat_train_full %>% 
  select(smokePM, smoke_day, aot_anom, precipitation, temperature, pbl_min, 
         km_dist, elevation, wind_u, wind_v, month, year, lon, lat)
nf <- length(setdiff(names(dat_train), "smokePM"))

# Choose resampling strategy and define grid
task <- makeRegrTask(data = dat_train, target = "smokePM")
learner <- makeLearner("regr.ranger")
rsmpl_desc <- makeResampleDesc("CV", iters = 2)
params <- makeParamSet(makeIntegerParam("mtry", 2, floor(nf/3) + 1),
                       makeIntegerParam("min.node.size", 5, 50),
                       makeIntegerParam("sample.fraction", 7, 10, trafo = function(x) x/10),
                       makeDiscreteParam("num.trees", nf*10))

# Tune hyperparameters
start_time <- get_start_time()
set.seed(7569)
res <- tuneParams(learner, task, rsmpl_desc, par.set = params,
                  control = makeTuneControlRandom())
print_time(start_time)
beep_alert("Done tuning")

# Fit random forest model
set.seed(3473)
mdl <- ranger(smokePM ~ .,
              data = dat_train,
              min.node.size = res$x$min.node.size,
              mtry = res$x$mtry,
              sample.fraction = res$x$sample.fraction,
              num.trees = 500,
              importance = "impurity")

# Save model
saveRDS(mdl, paste0(path_results, "model_covariates_random_forest.rds"))
