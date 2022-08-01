source("work/06_complex_MERRA_model/00_utils.R")

library(dplyr)
library(mlr)
library(gbm)

#-------------------------------------------------------------------------------
# Fit Gradient Boosted Trees Covariate Model
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

# Choose resampling strategy
task <- makeRegrTask(data = dat_train, target = "smokePM")
learner <- makeLearner("regr.gbm")
rsmpl_desc <- makeResampleDesc("CV", iters = 2)

# Define grid
params <- makeParamSet(
  makeIntegerParam("interaction.depth", 3, 8),
  makeIntegerParam("n.minobsinnode", 10, 50),
  makeIntegerParam("bag.fraction", 5, 10, trafo = function(x) x/10),
  makeDiscreteParam("n.trees", 100),
  makeDiscreteParam("shrinkage", 0.1)
)

# Tune hyperparameters
start_time <- get_start_time()
set.seed(7569)
res <- tuneParams(learner, task, rsmpl_desc, par.set = params,
                  control = makeTuneControlRandom(maxit = 50))
print_time(start_time)
beep_alert("Done tuning")

# Fit gradient boosted model
set.seed(3473)
mdl <- gbm(formula = smokePM ~ .,
           data = dat_train,
           n.trees = 5000,
           interaction.depth = res$x$interaction.depth,
           n.minobsinnode = res$x$n.minobsinnode,
           shrinkage = 0.05,
           bag.fraction = res$x$bag.fraction)

# Save model
saveRDS(mdl, paste0(path_results, "model_covariates_gradient_boosted_trees.rds"))