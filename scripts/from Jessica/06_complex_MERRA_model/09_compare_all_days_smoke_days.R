source("work/06_complex_MERRA_model/00_utils.R")

library(fixest)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

#-------------------------------------------------------------------------------
# Compare Training on All Days and Training on Smoke Days
# Written by Jessica
# Last edited August 2021
# 
# Fit random forest using nested CV with all days and smoke days. Evaluate on 
# the held out test set for each fold using model predictions (lower bounded at 
# 0) on smoke days and 0 for non-smoke days with R2, within R2, and RMSE for 
# full set of days, smoke days, and days with smokePM > 50.
#-------------------------------------------------------------------------------
# Load predictions
dat_preds <- readRDS(paste0(path_project, "sample_test_random_forest_predictions.rds"))

# Get state-month and state-year FEs
dat_panel <- readRDS(paste0(path_dropbox, "PM25/epa_station_smokePM_full_panel_extendedCovariates.rds")) %>% 
  select(epa_id, date, state_month, state_year)

# Limit to test data set
dat_test <- dat_preds %>% 
  pivot_longer(cols = starts_with("pred"),
               names_prefix = "pred_",
               names_to = c("train_sample", "test_fold"),
               names_sep = "_",
               values_to = "pred") %>% 
  mutate(train_sample = ifelse(train_sample == "sample1", "all days", "smoke days"),
         train_sample = factor(train_sample, levels = c("all days", "smoke days")),
         test_fold = gsub("fold", "", test_fold) %>% as.integer()) %>% 
  filter(fold == test_fold) %>% 
  left_join(dat_panel)

#-------------------------------------------------------------------------------
# Prepare data frame for plotting metric by training sample, evaluation sample, and fold
dat_test_sd <- dat_test %>% 
  filter(smoke_day == 1) %>% 
  add_column(eval_sample = "Smoke")
dat_test_sd_50 <- dat_test %>% 
  filter(smoke_day == 1, smokePM > 50) %>% 
  add_column(eval_sample = "Smoke50")
df_eval <- dat_test %>% 
  add_column(eval_sample = "Full") %>% 
  bind_rows(dat_test_sd, dat_test_sd_50) %>% 
  group_by(train_sample, eval_sample, fold) %>% 
  summarize(r2 = r2(feols(smokePM ~ pred,
                          data = cur_data()), "r2"),
            wr2 = r2(feols(smokePM ~ pred | epa_id + state_month + state_year,
                           data = cur_data()), "wr2"),
            rmse = sqrt(mean((smokePM - pred)^2)),
            me = mean(smokePM - pred)) %>% 
  ungroup() %>% 
  pivot_longer(cols = c(r2, wr2, rmse, me),
               names_to = "metric") %>% 
  group_by(metric, eval_sample, fold) %>% 
  arrange(abs(value)) %>% 
  mutate(rank = case_when(metric == "r2" ~ rev(row_number()),
                          metric == "wr2" ~ rev(row_number()),
                          metric == "rmse" ~ row_number(),
                          metric == "me" ~ row_number()),
         value = round(value, 3)) %>% 
  ungroup() %>% 
  mutate(metric = factor(metric, levels = c("r2", "wr2", "rmse", "me")))

# Plot metric by training sample, evaluation sample, and fold
ggplot(data = df_eval, 
       mapping = aes(x = eval_sample, y = train_sample, fill = rank)) + 
  geom_tile() + 
  geom_text(aes(label = value)) + 
  facet_wrap(vars(fold, metric), ncol = 4) +
  scale_fill_gradient(low = "skyblue2", high = "palevioletred2") + 
  labs(caption = "Rank 1 means best performance.") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, hjust=1))

# Save
ggsave(paste0(path_results, "sample_test_performance_byfold.png"),
       width = 15, height = 9)

#-------------------------------------------------------------------------------
# Prepare data frame for plotting metric by training sample and evaluation sample
# averaged across folds
df_eval <- dat_test %>% 
  add_column(eval_sample = "Full") %>% 
  bind_rows(dat_test_sd, dat_test_sd_50) %>% 
  group_by(train_sample, eval_sample, fold) %>% 
  summarize(r2 = r2(feols(smokePM ~ pred,
                          data = cur_data()), "r2"),
            wr2 = r2(feols(smokePM ~ pred | epa_id + state_month + state_year,
                           data = cur_data()), "wr2"),
            rmse = sqrt(mean((smokePM - pred)^2))) %>% 
  ungroup(fold) %>% 
  summarize(across(c(r2, wr2, rmse), mean)) %>% 
  ungroup() %>% 
  pivot_longer(cols = c(r2, wr2, rmse),
               names_to = "metric") %>% 
  group_by(metric, eval_sample) %>% 
  arrange(abs(value)) %>% 
  mutate(rank = case_when(metric == "r2" ~ rev(row_number()),
                          metric == "wr2" ~ rev(row_number()),
                          metric == "rmse" ~ row_number()),
         value = round(value, 3)) %>% 
  ungroup() %>% 
  mutate(metric = factor(metric, levels = c("r2", "wr2", "rmse")))

# Plot metric by training sample and evaluation sample
ggplot(data = df_eval, 
       mapping = aes(x = eval_sample, y = train_sample, fill = rank)) + 
  geom_tile() + 
  geom_text(aes(label = value)) + 
  facet_wrap(vars(metric), ncol = 4) +
  scale_fill_gradient(low = "skyblue2", high = "palevioletred2") + 
  labs(caption = "Rank 1 means best performance.") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, hjust=1))

# Save
ggsave(paste0(path_results, "sample_test_performance_avg.png"),
       width = 12, height = 3)
