source("work/06_complex_MERRA_model/00_utils.R")

library(fixest)
library(ranger)
library(gbm)
library(dplyr)
library(ggplot2)
library(vip)
library(pdp)

#-------------------------------------------------------------------------------
# Plot Variable Importance and Partial Dependence
# Written by Jessica
# Last edited July 2021
#-------------------------------------------------------------------------------
# Load models
mdl_ln <- readRDS(paste0(path_results, "model_covariates_linear.rds"))
mdl_rf <- readRDS(paste0(path_results, "model_covariates_random_forest.rds"))
mdl_gb <- readRDS(paste0(path_results, "model_covariates_gradient_boosted_trees.rds"))

# Load training data
dat_train_full <- readRDS(paste0(path_project, "dat_train.rds"))

#-------------------------------------------------------------------------------
# Select features in linear model
dat_train <- dat_train_full %>% 
  select(smokePM, smoke_day, aot_anom, km_dist, elevation, pbl_min, temperature,
         precipitation, epa_id, month, year)
features <- names(dat_train) %>% setdiff("smokePM")

# Plot variable importance for linear model
set.seed(7568)
vi_ln <- vi(mdl_ln, method = "permute", target = "smokePM", metric = "rmse", 
            pred_wrapper = predict, nsim = 10)
ggplot(data = vi_ln, 
       mapping = aes(x = Importance, y = reorder(Variable, Importance))) + 
  geom_pointrange(mapping = aes(xmin = Importance - 1.96*StDev,
                                xmax = Importance + 1.96*StDev), size = 0.2) + 
  theme_bw() + 
  labs(y = "", title = "Linear MERRA Covariate Model")

# Save plot
ggsave(filename = paste0(path_results, "model_covariates_vip_ln.png"),
       width = 6, height = 3)

#-------------------------------------------------------------------------------
# Plot variable importance for random forest model
vip(mdl_rf, geom = "point") + 
  theme_bw() + 
  labs(title = "Random Forest MERRA Covariate Model")

# Save plot
ggsave(filename = paste0(path_results, "model_covariates_vip_rf.png"),
       width = 6, height = 3)

#-------------------------------------------------------------------------------
# Plot variable importance for gradient boosted trees model
vip(mdl_gb, geom = "point") + 
  theme_bw() + 
  labs(title = "Gradient Boosted Trees MERRA Covariate Model")

# Save plot
ggsave(filename = paste0(path_results, "model_covariates_vip_gb.png"),
       width = 6, height = 3)

#-------------------------------------------------------------------------------
# Set up function for plotting PDPs
partial_ <- function(object, pred.var, train, n.trees = NULL) {
  if (is.null(n.trees)) {
    return(pdp::partial(object = object, 
                        pred.var = pred.var, 
                        train = train))
  } else {
    return(pdp::partial(object = object, 
                        pred.var = pred.var, 
                        train = train, 
                        n.trees = n.trees))
  }
}
plot_pdp <- function(object, pred.var, train, data, ylim, n.trees = NULL) {
  part <- partial_(object = object,
                   pred.var = pred.var,
                   train = train, 
                   n.trees = n.trees)
  autoplot(part) + 
    geom_rug(data = data, 
             mapping = aes(x = get(pred.var), y = mean(part$yhat)),
             alpha = 0.5) + 
    ylim(ylim) + 
    theme_classic()
}

#-------------------------------------------------------------------------------
# Plot partial dependence for linear model
ylim <- c(-20, 20)
start_time <- get_start_time()
pdp_ln <- lapply(features, plot_pdp, object = mdl_ln, train = dat_train, 
                 data = dat_train, ylim = ylim)
print_time(start_time)

# Save linear model PDPs
png(paste0(path_results, "model_covariates_pdp_ln.png"),
    width = 1200,
    height = 1200)
do.call("grid.arrange", pdp_ln)
dev.off()

#-------------------------------------------------------------------------------
# Select features in ML models
dat_train <- dat_train_full %>% 
  select(smokePM, smoke_day, aot_anom, precipitation, temperature, pbl_min, 
         km_dist, elevation, wind_u, wind_v, month, year, lon, lat)
features <- names(dat_train) %>% setdiff("smokePM")

# Plot partial dependence for random forest model
ylim <- c(0, 25)
start_time <- get_start_time()
pdp_rf <- lapply(features, plot_pdp, object = mdl_rf, train = dat_train, 
                 data = dat_train, ylim = ylim)
print_time(start_time)

# Save random forest PDPs
png(paste0(path_results, "model_covariates_pdp_rf.png"),
    width = 1200,
    height = 1200)
do.call("grid.arrange", pdp_rf)
dev.off()

#-------------------------------------------------------------------------------
# Plot partial dependence for gradient boosted trees model
ylim <- c(0, 25)
pdp_gb <- lapply(features, plot_pdp, object = mdl_gb, train = dat_train, 
                 data = dat_train, ylim = ylim, n.trees = 5000)

# Save gradient boosted trees PDPs
png(paste0(path_results, "model_covariates_pdp_gb.png"),
    width = 1200,
    height = 1200)
do.call("grid.arrange", pdp_gb)
dev.off()
