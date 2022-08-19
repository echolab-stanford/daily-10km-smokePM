source("work/06_complex_MERRA_model/00_utils.R")

library(fixest)
library(ranger)
library(gbm)
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(ggplot2)

#-------------------------------------------------------------------------------
# Compare Linear, Random Forest, and Gradient Boosted Covariate Models
# Written by Jessica
# Last edited July 2021
#-------------------------------------------------------------------------------
# Load models
mdl_ln <- readRDS(paste0(path_results, "model_covariates_linear.rds"))
mdl_rf <- readRDS(paste0(path_results, "model_covariates_random_forest.rds"))
mdl_gb <- readRDS(paste0(path_results, "model_covariates_gradient_boosted_trees.rds"))

# Load test data set
dat_test <- readRDS(paste0(path_project, "dat_test.rds"))

# Get model predictions
coef_ln <- coef(mdl_ln)
smoke_day <- dat_test$smoke_day
aot_anom <- dat_test$aot_anom
km_dist <- dat_test$km_dist
elevation <- dat_test$elevation
pbl_min <- dat_test$pbl_min
temperature <- dat_test$temperature
precipitation <- dat_test$precipitation
pred_ln <- coef_ln["smoke_day"]*smoke_day + 
  coef_ln["aot_anom:smoke_day"]*aot_anom*smoke_day + 
  coef_ln["smoke_day:km_dist"]*smoke_day*km_dist + 
  coef_ln["smoke_day:elevation"]*smoke_day*elevation + 
  coef_ln["smoke_day:pbl_min"]*smoke_day*pbl_min + 
  coef_ln["smoke_day:temperature"]*smoke_day*temperature + 
  coef_ln["smoke_day:precipitation"]*smoke_day*precipitation + 
  coef_ln["aot_anom:smoke_day:km_dist"]*aot_anom*smoke_day*km_dist + 
  coef_ln["aot_anom:smoke_day:elevation"]*aot_anom*smoke_day*elevation + 
  coef_ln["aot_anom:smoke_day:pbl_min"]*aot_anom*smoke_day*pbl_min + 
  coef_ln["aot_anom:smoke_day:temperature"]*aot_anom*smoke_day*temperature + 
  coef_ln["aot_anom:smoke_day:precipitation"]*aot_anom*smoke_day*precipitation
dat_test$pred_ln <- pred_ln
dat_test$pred_rf <- predict(mdl_rf, dat_test)$predictions
dat_test$pred_gb <- predict(mdl_gb, dat_test)

#-------------------------------------------------------------------------------
# Prepare data frame for plotting scatters
df_scatter <- dat_test %>% 
  pivot_longer(cols = starts_with("pred_"),
               names_to = "model",
               names_prefix = "pred_",
               values_to = "pred_smokePM")

# Plot scatters
ggplot(data = df_scatter, mapping = aes(x = pred_smokePM, y = smokePM, color = model)) + 
  geom_point(size = 1, alpha = 0.5) + 
  geom_abline() + 
  theme_light() + 
  labs(title = "MERRA Covariate Model Predictions")

# Save plot
ggsave(filename = paste0(path_results, "model_covariates_LNvsRFvsGB_scatters.png"), width = 8, height = 3)

#-------------------------------------------------------------------------------
# Prepare data frame for plotting metric by model
dat_test_sd <- dat_test %>% 
  filter(smoke_day == 1) %>% 
  add_column(subset = "Smoke")
dat_test_sd_100 <- dat_test %>% 
  filter(smoke_day == 1, smokePM > 100) %>% 
  add_column(subset = "Smoke100")

df_eval <- dat_test %>% 
  add_column(subset = "Full") %>% 
  bind_rows(dat_test_sd, dat_test_sd_100) %>% 
  select(smokePM, starts_with("pred_"), subset) %>% 
  pivot_longer(cols = starts_with("pred_"),
               names_to = "model",
               names_prefix = "pred_",
               values_to = "pred_smokePM") %>% 
  group_by(subset, model) %>% 
  summarize(r2 = cor(smokePM, pred_smokePM)^2,
            rmse = sqrt(mean((smokePM - pred_smokePM)^2)),
            me = mean(smokePM - pred_smokePM)) %>% 
  ungroup() %>% 
  pivot_longer(cols = c(r2, rmse, me),
               names_to = "metric") %>% 
  unite("metric_subset", c(metric, subset), sep = "") %>% 
  group_by(metric_subset) %>% 
  arrange(abs(value)) %>% 
  mutate(rank = case_when(str_detect(metric_subset, "^r2") ~ rev(row_number()),
                          str_detect(metric_subset, "^rmse") ~ row_number(),
                          str_detect(metric_subset, "^me") ~ row_number()) %>% 
           as.integer(),
         value = round(value, 3)) %>% 
  ungroup()

# Plot metric by model
ggplot(data = df_eval, mapping = aes(x = metric_subset, y = model, fill = rank)) + 
  geom_tile() + 
  geom_text(aes(label = value)) + 
  scale_fill_gradient(low = "skyblue2", high = "palevioletred2") + 
  labs(title = "MERRA Covariate Model Performance",
       caption = "Rank 1 means best performance.") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, hjust=1))
  
# Save plot
ggsave(filename = paste0(path_results, "model_covariates_LNvsRFvsGB_metrics.png"), width = 8, height = 3)

#-------------------------------------------------------------------------------
# Look at R2
reg_ln <- lm(smokePM ~ pred_ln:smoke_day, dat_test)
reg_rf <- lm(smokePM ~ pred_rf:smoke_day, dat_test)
reg_gb <- lm(smokePM ~ pred_gb:smoke_day, dat_test)

summary(reg_ln)
summary(reg_rf)
summary(reg_gb)

#-------------------------------------------------------------------------------
#### Compare against Reid et al. ####
#-------------------------------------------------------------------------------
# Read in Reid et al. predictions
dat_reid <- readRDS(paste0(path_dropbox, "PM25/Reid et al 2021/clean/reid_anom_df.rds")) %>% 
  select(epa_id = id, date, pm25_anom_reid = pred_pm25_anom)

# Join to test data and filter to comparable obs
dat_test <- dat_test %>% 
  left_join(dat_reid, by = c("epa_id", "date")) %>%
  mutate(pred_reid = pmax(if_else(smoke_day == 1, pm25_anom_reid, 0), 0),
         pred_reid = if_else(is.na(pm25_anom_reid), pm25_anom_reid, pred_reid)) %>% 
  drop_na(pred_reid)

#-------------------------------------------------------------------------------
# Prepare data frame for plotting scatters
df_scatter <- dat_test %>% 
  pivot_longer(cols = starts_with("pred_"),
               names_to = "model",
               names_prefix = "pred_",
               values_to = "pred_smokePM")

# Plot scatters
ggplot(data = df_scatter, mapping = aes(x = pred_smokePM, y = smokePM, color = model)) + 
  geom_point(size = 1, alpha = 0.5) + 
  geom_abline() + 
  theme_light() + 
  labs(title = "MERRA Covariate Model and Reid et al. Predictions")

# Save plot
ggsave(filename = paste0(path_results, "model_covariates_LNvsRFvsGBvsReid_scatters.png"), width = 8, height = 3)

#-------------------------------------------------------------------------------
# Prepare data frame for plotting metric by model
dat_test_sd <- dat_test %>% 
  filter(smoke_day == 1) %>% 
  add_column(subset = "Smoke")
dat_test_sd_50 <- dat_test %>% 
  filter(smoke_day == 1, smokePM > 50) %>% 
  add_column(subset = "Smoke50")

df_eval <- dat_test %>% 
  add_column(subset = "Full") %>% 
  bind_rows(dat_test_sd, dat_test_sd_50) %>% 
  select(smokePM, starts_with("pred_"), subset) %>% 
  pivot_longer(cols = starts_with("pred_"),
               names_to = "model",
               names_prefix = "pred_",
               values_to = "pred_smokePM") %>% 
  group_by(subset, model) %>% 
  summarize(r2 = cor(smokePM, pred_smokePM)^2,
            rmse = sqrt(mean((smokePM - pred_smokePM)^2)),
            me = mean(smokePM - pred_smokePM)) %>% 
  ungroup() %>% 
  pivot_longer(cols = c(r2, rmse, me),
               names_to = "metric") %>% 
  unite("metric_subset", c(metric, subset), sep = "") %>% 
  group_by(metric_subset) %>% 
  arrange(abs(value)) %>% 
  mutate(rank = case_when(str_detect(metric_subset, "^r2") ~ rev(row_number()),
                          str_detect(metric_subset, "^rmse") ~ row_number(),
                          str_detect(metric_subset, "^me") ~ row_number()) %>% 
           as.integer(),
         value = round(value, 3)) %>% 
  ungroup()

# Plot metric by model
ggplot(data = df_eval, mapping = aes(x = metric_subset, y = model, fill = rank)) + 
  geom_tile() + 
  geom_text(aes(label = value)) + 
  scale_fill_gradient(low = "skyblue2", high = "palevioletred2") + 
  labs(title = "MERRA Covariate Model and Reid et al. Performance",
       caption = "Rank 1 means best performance.") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, hjust=1))

# Save plot
ggsave(filename = paste0(path_results, "model_covariates_LNvsRFvsGBvsReid_metrics.png"), width = 8, height = 3)

#-------------------------------------------------------------------------------
# Look at R2
reg_ln <- lm(smokePM ~ pred_ln:smoke_day, dat_test)
reg_rf <- lm(smokePM ~ pred_rf:smoke_day, dat_test)
reg_gb <- lm(smokePM ~ pred_gb:smoke_day, dat_test)
reg_reid <- lm(smokePM ~ pred_reid:smoke_day, dat_test)

summary(reg_ln)
summary(reg_rf)
summary(reg_gb)
summary(reg_reid)
