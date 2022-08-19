source("work/05_get_HYSPLIT_height/00_utils.R")

library(fixest)
library(ranger)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(ggplot2)
library(ggpubr)
library(lubridate)
library(tigris)

width <- 10
height <- 6

#-------------------------------------------------------------------------------
# Assess Importance of HYSPLIT
# Written by Jessica
# Last edited August 2021
# 
# In terms of model performance, variable importance, and time series
#-------------------------------------------------------------------------------
#### Model performance ####
# How does model performance change upon including the number of trajectory 
# points that fall within 100 km of an EPA station-day improve performance as a
# predictor?
#-------------------------------------------------------------------------------
# Load predictions
dat_preds <- readRDS(paste0(path_project, "using_ranger/HYSPLIT_random_forest_predictions.rds"))

# Get state-month and state-year FEs
dat_panel <- readRDS(paste0(path_dropbox, "PM25/epa_station_smokePM_full_panel_extendedCovariates.rds")) %>% 
  select(epa_id, date, state_month, state_year, county)

# Limit to test data set
dat_test <- dat_preds %>% 
  pivot_longer(cols = starts_with("pred"),
               names_to = c("spec", "test.fold", "train.cutoff", "train.injection_heights"),
               names_prefix = "pred.",
               names_sep = "\\.",
               names_transform = list(test.fold = as.integer,
                                      train.cutoff = as.numeric),
               values_to = "pred") %>% 
  mutate(pred = pmax(if_else(smoke_day == 1, pred, 0), 0),
         train.injection_heights = gsub("_", "+", train.injection_heights),
         train.cutoff = ifelse(is.na(train.cutoff), 10, train.cutoff),
         train.injection_heights = ifelse(is.na(train.injection_heights), "500+1500+2500", train.injection_heights)) %>% 
  filter(fold == test.fold,
         cutoff == train.cutoff,
         injection_heights == train.injection_heights) %>% 
  left_join(dat_panel) %>% 
  mutate(county = as.character(county),
         state_code = substr(county, 1, 2),
         county_code = substr(county, 3, 5)) %>% 
  left_join(fips_codes %>% select(state_code, county_code, county_name = county))

#-------------------------------------------------------------------------------
# Prepare data frame for plotting metric by model specification and evaluation sample
dat_test_sd_50 <- dat_test %>% 
  filter(smoke_day == 1, smokePM > 50) %>% 
  add_column(subset = "Smoke50")
df_eval <- dat_test %>% 
  add_column(subset = "Smoke") %>% 
  bind_rows(dat_test_sd_50) %>% 
  group_by(subset, spec, fold, injection_heights, cutoff) %>% 
  summarize(r2 = r2(feols(smokePM ~ pred,
                          data = cur_data()), "r2"),
            wr2 = r2(feols(smokePM ~ pred | epa_id + state_month + state_year,
                           data = cur_data()), "wr2"),
            rmse = sqrt(mean((smokePM - pred)^2))) %>% 
  ungroup(cutoff, injection_heights, fold) %>% 
  summarize(across(c(r2, wr2, rmse), mean)) %>% 
  ungroup() %>% 
  pivot_longer(cols = c(r2, wr2, rmse),
               names_to = "metric") %>% 
  group_by(metric, subset) %>% 
  arrange(abs(value)) %>% 
  mutate(rank = case_when(metric == "r2" ~ rev(row_number()),
                          metric == "wr2" ~ rev(row_number()),
                          metric == "rmse" ~ row_number()) %>% 
           as.integer(),
         value = round(value, 3)) %>% 
  ungroup() %>% 
  mutate(metric = factor(metric, levels = c("r2", "wr2", "rmse")))

# Plot metric by model specification and evaluation sample
ggplot(data = df_eval, mapping = aes(x = subset, y = spec, fill = rank)) + 
  geom_tile() + 
  geom_text(aes(label = value)) + 
  facet_wrap(vars(metric), ncol = 3) +
  scale_fill_gradient(low = "skyblue2", high = "palevioletred2") + 
  labs(caption = "Rank 1 means best performance.") + 
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  labs(x = "")

# Save
ggsave(paste0(path_results, "using_ranger/noHYwithHY_performance_avg.png"),
       width = width, height = height)

#-------------------------------------------------------------------------------
# Prepare data frame for plotting distribution of metric by specification and sample
df_eval <- dat_test %>% 
  add_column(subset = "Smoke") %>% 
  bind_rows(dat_test_sd_50) %>% 
  group_by(subset, spec, injection_heights, cutoff, fold) %>% 
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
  mutate(metric = factor(metric, levels = c("r2", "wr2", "rmse")))

# Plot distribution of metric by specification and sample
ggplot() + 
  geom_violin(data = df_eval %>% filter(spec == "n_traj_points"),
              mapping = aes(x = subset, y = value, color = spec)) + 
  geom_point(data = df_eval %>% filter(spec == "baseline"),
             mapping = aes(x = subset, y = value, color = spec)) + 
  facet_wrap(vars(subset,metric), scales = "free") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  labs(y = "")

# Save
ggsave(paste0(path_results, "using_ranger/noHYwithHY_performance_distribution.png"),
       width = width, height = height)

#-------------------------------------------------------------------------------
# Prepare data frame to plot range of metric by specification and sample
df_eval <- df_eval %>% 
  group_by(subset, spec, metric) %>% 
  summarize(value_min = min(value), 
            value_max = max(value)) %>% 
  ungroup()

# Plot range of metric by specification and sample
ggplot() +  
  geom_linerange(data = df_eval %>% filter(spec == "n_traj_points"), 
                 mapping = aes(x = subset, ymin = value_min, ymax = value_max, color = spec)) + 
  geom_point(data = df_eval %>% filter(spec == "baseline"),
             mapping = aes(x = subset, y = value_min, color = spec)) + 
  facet_wrap(vars(subset, metric), scales = "free") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  labs(y = "")

# Save
ggsave(paste0(path_results, "using_ranger/noHYwithHY_performance_range.png"),
       width = width, height = height)

#-------------------------------------------------------------------------------
#### Variable importance ####
# How does n_traj_points compare in importance against other predictors?
#-------------------------------------------------------------------------------
# Load random forests
mdls <- readRDS(paste0(path_project, "using_ranger/HYSPLIT_random_forest_models.rds"))

# Get variable importance
get_importance <- function(x) {
  x[[1]]$variable.importance %>% 
    as.data.frame() %>% 
    rownames_to_column(var = "variable") %>% 
    rename(importance = ".") %>% 
    mutate(spec = x$spec, 
           fold = x$fold, 
           injection_heights = x$injection_heights, 
           cutoff = x$cutoff) %>% 
    return()
}
importance <- mdls %>% 
  map_dfr(get_importance) %>% 
  filter(spec != "baseline")

#-------------------------------------------------------------------------------
# Prepare data frame for plotting importance
df_imp <- importance %>% 
  group_by(variable, injection_heights, cutoff) %>% 
  summarize(importance = mean(importance)) %>% 
  ungroup(cutoff, injection_heights) %>% 
  summarize(importance_min = min(importance),
            importance_max = max(importance),
            importance = median(importance)) %>% 
  ungroup()

# Plot median and range across injection heights and cutoffs of 
# across-folds-average importance for each variable
ggplot(data = df_imp, mapping = aes(x = importance, y = reorder(variable, importance))) + 
  geom_pointrange(mapping = aes(xmin = importance_min, xmax = importance_max, color = (variable == "n_traj_points")), fatten = 1.5) + 
  scale_color_manual(values = c("black", "red3")) + 
  theme_bw() + 
  labs(y = "") + 
  theme(legend.position = "")

# Save
ggsave(paste0(path_results, "using_ranger/noHYwithHY_importance_avgfolds.png"),
       width = width, height = height)

#-------------------------------------------------------------------------------
# Prepare data frame for plotting importance by fold
df_imp <- importance %>% 
  group_by(variable, fold) %>% 
  summarize(importance_min = min(importance),
            importance_max = max(importance),
            importance = median(importance)) %>% 
  ungroup()

# Plot median and range across injection heights and cutoffs of importance for
# each variable by fold
ggplot(data = df_imp, mapping = aes(x = importance, y = reorder_within(variable, importance, fold))) + 
  geom_pointrange(mapping = aes(xmin = importance_min, xmax = importance_max, color = (variable == "n_traj_points")), fatten = 1.5) + 
  facet_wrap(vars(fold), scales = "free") + 
  scale_y_reordered() + 
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) + 
  scale_color_manual(values = c("black", "red3")) + 
  theme_bw() + 
  labs(y = "") + 
  theme(legend.position = "")

# Save
ggsave(paste0(path_results, "using_ranger/noHYwithHY_importance_byfold.png"),
       width = width, height = height)

#-------------------------------------------------------------------------------
#### Time series ####
# How do predictions differ over the course of a wildfire?
# Plot smoke PM, baseline predictions, and HYSPLIT model predictions. Faded 
# lines are for HYSPLIT model predictions not averaged across injection height 
# combinations and cutoffs. Each specification predicts using different forests 
# for each fold. Fire information is from CAL FIRE 2020 Incident Archive 
# (https://www.fire.ca.gov/incidents/2020/).
#-------------------------------------------------------------------------------
plot_fire <- function(df, start_date, end_date, counties, fire_name, output = "avg") {
  stopifnot(output %in% c("avg", "dist"))
  
  # Adjust 10% buffer for time period
  adj <- (end_date - start_date)/10
  start_date <- start_date - adj
  end_date <- end_date + adj
  
  # Reshape data frame
  event <- df %>% 
    filter(start_date <= date, 
           date <= end_date,
           county_name %in% paste(counties, "County")) %>% 
    group_by(date, spec, injection_heights, cutoff) %>% 
    summarize(across(c(smokePM, pred), mean)) %>% 
    ungroup() %>% 
    pivot_wider(names_from = spec,
                names_prefix = "pred_",
                values_from = pred) %>% 
    mutate(smokePM = ifelse(is.na(pred_baseline), NA, smokePM)) %>% 
    pivot_longer(cols = c(smokePM, starts_with("pred")),
                 names_to = "spec") %>% 
    drop_na() %>% 
    mutate(injection_heights = ifelse(spec == "pred_n_traj_points", injection_heights, NA),
           cutoff = ifelse(spec == "pred_n_traj_points", cutoff, NA),
           spec = factor(spec, levels = c("smokePM", "pred_baseline", "pred_n_traj_points")))
  
  # Average predictions across injection height combinations and cutoffs
  event_avg <- event %>% 
    group_by(date, spec) %>% 
    summarize(value = mean(value)) %>% 
    ungroup()
  
  if (output == "avg") {
    return(
      # Plot time series
      ggplot(data = event_avg, 
             mapping = aes(x = date, 
                           y = value, 
                           color = spec)) + 
        geom_line() + 
        theme_classic() + 
        labs(title = fire_name,
             x = "",
             y = expression(PM[2.5])) + 
        scale_color_manual(values = c("black", "blue", "red"))
    )
  } else {
    # Add rows for predictions of each injection height combination and cutoff
    event_dist <- event %>% 
      filter(spec == "pred_n_traj_points") %>% 
      mutate(alpha = 0.01) %>% 
      bind_rows(event_avg %>% mutate(alpha = 1))
    
    return(
      # Plot time series; faded lines show distribution
      ggplot(data = event_dist, 
             mapping = aes(x = date, 
                           y = value, 
                           color = spec, 
                           group = interaction(injection_heights, cutoff),
                           alpha = alpha)) + 
        geom_line() + 
        theme_classic() + 
        labs(title = fire_name,
             x = "",
             y = expression(PM[2.5])) + 
        scale_alpha(guide = "none")
    )
  }
}

#-------------------------------------------------------------------------------
# August Complex Fire
plot_fire(dat_test, 
          ymd("2020-08-16"), 
          ymd("2020-11-11"), 
          c("Mendocino", "Humboldt", "Trinity", "Tehama", "Glenn", "Lake", "Colusa"), 
          "August Complex Fire")

# Save
ggsave(paste0(path_results, "using_ranger/noHYwithHY_fire_AugustComplex.png"),
       width = width, height = height)

#-------------------------------------------------------------------------------
# SCU Lightning Complex Fire
plot_fire(dat_test,
          ymd("2020-08-18"),
          ymd("2020-10-01"),
          c("Santa Clara", "Alameda", "Contra Costa", "San Joaquin", "Merced", "Stanislaus"),
          "SCU Lightning Complex Fire")

# Save
ggsave(paste0(path_results, "using_ranger/noHYwithHY_fire_SCULightningComplex.png"),
       width = width, height = height)

#-------------------------------------------------------------------------------
# CZU Lightning Complex Fire
plot_fire(dat_test,
          ymd("2020-08-16"),
          ymd("2020-09-22"),
          c("Santa Cruz", "San Mateo"),
          "CZU Lightning Complex Fire")

# Save
ggsave(paste0(path_results, "using_ranger/noHYwithHY_fire_CZULightningComplex.png"),
       width = width, height = height)

#-------------------------------------------------------------------------------
# Creek Fire
plot_fire(dat_test,
          ymd("2020-09-04"),
          ymd("2020-12-18"),
          c("Fresno", "Madera"),
          "Creek Fire")

# Save
ggsave(paste0(path_results, "using_ranger/noHYwithHY_fire_Creek.png"),
       width = width, height = height)

#-------------------------------------------------------------------------------
# What is the upper bound on improved performance?
dat_test %>% 
  group_by(date, county_name, spec, injection_heights, cutoff) %>% 
  summarize(across(c(smokePM, pred), mean)) %>% 
  ungroup(cutoff, injection_heights) %>% 
  summarize(across(c(smokePM, pred), mean)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = spec,
              names_prefix = "pred_",
              values_from = pred) %>% 
  filter((smokePM > pred_n_traj_points & pred_n_traj_points > pred_baseline) |
           (smokePM < pred_n_traj_points & pred_n_traj_points < pred_baseline)) %>% 
  mutate(diff = abs(pred_n_traj_points - pred_baseline)) %>% 
  arrange(desc(diff)) %>% 
  filter(diff > 10) %>% 
  # View()
  group_by(county_name) %>%
  summarize(start_date = min(date),
            end_date = max(date)) %>%
  ungroup() %>%
  filter(end_date - start_date > days(7))

# Plot time series for counties with greatest improved prediction performance
inyo <- plot_fire(dat_test,
                  ymd("2020-08-24"), 
                  ymd("2020-09-25"),
                  "Inyo",
                  "Inyo County")
mariposa <- plot_fire(dat_test,
                      ymd("2020-09-01"), 
                      ymd("2020-10-10"),
                      "Mariposa",
                      "Mariposa County")
mono <- plot_fire(dat_test,
                  ymd("2020-09-01"), 
                  ymd("2020-10-10"),
                  "Mono",
                  "Mono County")
nevada <- plot_fire(dat_test,
                    ymd("2020-09-15"), 
                    ymd("2020-10-09"),
                    "Nevada",
                    "Nevada County")
plumas <- plot_fire(dat_test,
                    ymd("2020-07-22"), 
                    ymd("2020-10-09"),
                    "Plumas",
                    "Plumas County")
santa_cruz <- plot_fire(dat_test,
                        ymd("2020-08-25"), 
                        ymd("2020-09-10"),
                        "Santa Cruz",
                        "Santa Cruz County")
siskiyou <- plot_fire(dat_test,
                      ymd("2020-08-24"), 
                      ymd("2020-10-01"),
                      "Siskiyou",
                      "Siskiyou County")

# Arrange fires in grid
ggarrange(inyo, mariposa, mono, nevada, plumas, santa_cruz, siskiyou, 
          ncol = 2,
          nrow = 4,
          common.legend = TRUE)

# Save
ggsave(paste0(path_results, "using_ranger/noHYwithHY_fire_counties.png"),
       width = 10, height = 12)
