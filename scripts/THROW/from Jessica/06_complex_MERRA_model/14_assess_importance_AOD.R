source("work/06_complex_MERRA_model/00_utils.R")

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
# Assess Importance of AOD
# Written by Jessica
# Last edited August 2021
#-------------------------------------------------------------------------------
#### Model performance ####
#-------------------------------------------------------------------------------
# Load predictions
dat_preds <- readRDS(paste0(path_project, "AOD_random_forest_predictions.rds"))

# Load panel
dat_panel <- readRDS(paste0(path_dropbox, "PM25/epa_station_smokePM_full_panel_extendedCovariates.rds"))

# Limit to test data set
dat_test <- dat_preds %>% 
  pivot_longer(cols = starts_with("pred"),
               names_prefix = "pred_",
               names_to = c("spec", "test_fold"),
               names_sep = "_",
               values_to = "pred") %>% 
  mutate(spec = ifelse(spec == "spec1", "baseline", "aod"),
         test_fold = gsub("fold", "", test_fold) %>% as.integer()) %>% 
  filter(fold == test_fold) %>% 
  select(-test_fold) %>% 
  # Get state-month and state-year FEs
  left_join(dat_panel %>% select(epa_id, date, state_month, state_year, county, smoke_day))

# Get non-smoke days
not_smoke_day <- dat_panel %>% 
  drop_na(pm25, smokePM) %>% 
  filter(smoke_day == 0) %>% 
  select(epa_id, date, fold, smokePM, state_month, state_year, county, smoke_day) %>% 
  drop_na() %>% 
  mutate(pred = 0)
dat_test <- dat_test %>% 
  bind_rows(not_smoke_day %>% mutate(spec = "baseline"),
            not_smoke_day %>% mutate(spec = "aod")) %>% 
  mutate(spec = factor(spec, levels = c("baseline", "aod")))

# Get county and state
dat_test <- dat_test %>% 
  mutate(county = as.character(county),
         state_code = substr(county, 1, 2),
         county_code = substr(county, 3, 5)) %>% 
  left_join(fips_codes %>% rename(county_name = county))

# Read in Di et al. and Reid et al. predictions
dat_di <- readRDS(paste0(path_dropbox, "PM25/Di et al 2019/di_anom_df.rds")) %>% 
  select(epa_id, date, pm25_anom_di)
dat_reid <- readRDS(paste0(path_dropbox, "PM25/Reid et al 2021/clean/reid_anom_df.rds")) %>% 
  select(epa_id = id, date, pm25_anom_reid = pred_pm25_anom)

# Join to test data and filter to comparable obs
dat_test <- dat_test %>% 
  pivot_wider(names_from = "spec",
              names_prefix = "pred_",
              values_from = "pred") %>% 
  left_join(dat_di) %>% 
  mutate(pred_di = pmax(if_else(smoke_day == 1, pm25_anom_di, 0), 0),
         pred_di = if_else(is.na(pm25_anom_di), pm25_anom_di, pred_di)) %>% 
  drop_na(pred_di) %>% 
  left_join(dat_reid) %>%
  mutate(pred_reid = pmax(if_else(smoke_day == 1, pm25_anom_reid, 0), 0),
         pred_reid = if_else(is.na(pm25_anom_reid), pm25_anom_reid, pred_reid)) %>% 
  drop_na(pred_reid) %>% 
  select(-pm25_anom_di, -pm25_anom_reid) %>% 
  pivot_longer(cols = starts_with("pred"),
               names_to = "spec",
               names_prefix = "pred_",
               values_to = "pred") %>% 
  mutate(spec = factor(spec, levels = c("baseline", "aod", "di", "reid")))

#-------------------------------------------------------------------------------
# Prepare data frame for plotting metric by specification, evaluation sample, and fold
dat_test_sd <- dat_test %>% 
  filter(smoke_day == 1) %>% 
  add_column(eval_sample = "Smoke")
dat_test_sd_50 <- dat_test %>% 
  filter(smoke_day == 1, smokePM > 50) %>% 
  add_column(eval_sample = "Smoke50")
df_eval <- dat_test %>% 
  add_column(eval_sample = "Full") %>% 
  bind_rows(dat_test_sd, dat_test_sd_50) %>% 
  group_by(spec, eval_sample, fold) %>% 
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
         value = ifelse(round(value, 3) == 0, signif(value, 2), round(value, 3))) %>% 
  ungroup() %>% 
  mutate(metric = factor(metric, levels = c("r2", "wr2", "rmse", "me")))

# Plot metric by model specification, evaluation sample, and fold
ggplot(data = df_eval, 
       mapping = aes(x = eval_sample, y = spec, fill = rank)) + 
  geom_tile() + 
  geom_text(aes(label = value)) + 
  facet_wrap(vars(fold, metric), ncol = 4) +
  scale_fill_gradient(low = "skyblue2", high = "palevioletred2") + 
  labs(caption = "Rank 1 means best performance.") + 
  theme_classic()# + 
# theme(axis.text.x = element_text(angle = 45, hjust=1))

# Save
ggsave(paste0(path_results, "AOD_performance_byfold.png"),
       width = 15, height = 12) # height = 9)

#-------------------------------------------------------------------------------
# Prepare data frame for plotting metric by specification and evaluation sample
# averaged across folds
df_eval <- dat_test %>% 
  add_column(eval_sample = "Full") %>% 
  bind_rows(dat_test_sd, dat_test_sd_50) %>% 
  group_by(spec, eval_sample, fold) %>% 
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
         value = ifelse(round(value, 3) == 0, signif(value, 2), round(value, 3))) %>% 
  ungroup() %>% 
  mutate(metric = factor(metric, levels = c("r2", "wr2", "rmse")))

# Plot metric by training sample and evaluation sample
ggplot(data = df_eval, 
       mapping = aes(x = eval_sample, y = spec, fill = rank)) + 
  geom_tile() + 
  geom_text(aes(label = value)) + 
  facet_wrap(vars(metric), ncol = 4) +
  scale_fill_gradient(low = "skyblue2", high = "palevioletred2") + 
  labs(caption = "Rank 1 means best performance.") + 
  theme_classic()# + 
# theme(axis.text.x = element_text(angle = 45, hjust=1))

# Save
ggsave(paste0(path_results, "AOD_performance_avg.png"),
       width = 12, height = 4) # height = 3)

#-------------------------------------------------------------------------------
#### Time series ####
# Only applicable when not comparing against Di et al. and Reid et al. due to
# fire dates.
#-------------------------------------------------------------------------------
plot_fire <- function(df, start_date, end_date, counties, fire_name, fire_state = NULL, buffer = 0.1) {
  # Adjust 10% buffer for time period
  adj <- (end_date - start_date) * buffer
  start_date <- start_date - adj
  end_date <- end_date + adj
  
  # Get year(s)
  start_year <- year(start_date)
  end_year <- year(end_date)
  fire_year <- start_year
  if (start_year != end_year) fire_year <- sprintf("%s - %s", start_year, end_year)
  
  # Filter to dates of interest
  event <- df %>% 
    filter(start_date <= date, 
           date <= end_date)
  
  # Filter to affected counties and get state name
  if (counties[1] %>% substr(1, 1) %in% as.character(0:9)) {
    event <- event %>% filter(county %in% counties)
    fire_state <- event$state %>% unique() %>% toString()
  } else {
    event <- event %>% 
      filter((county_name %in% paste(counties, "County")) & 
               (state == fire_state))
  }
  
  # Get average PM over course of fire
  event <- event %>% 
    group_by(date, spec) %>% 
    summarize(across(c(smokePM, pred), mean)) %>% 
    ungroup() %>% 
    pivot_wider(names_from = spec,
                names_prefix = "pred_",
                values_from = pred) %>% 
    pivot_longer(cols = c(smokePM, starts_with("pred")),
                 names_to = "spec") %>% 
    mutate(spec = factor(spec, levels = c("smokePM", "pred_baseline", "pred_aod")))
  
  return(
    # Plot time series
    ggplot(data = event, 
           mapping = aes(x = date, 
                         y = value, 
                         color = spec)) + 
      geom_line() + 
      theme_classic() + 
      labs(title = fire_name,
           subtitle = paste(fire_state, fire_year, sep = "\n"),
           x = "",
           y = expression(PM[2.5])) + 
      scale_color_manual(values = c("black", "blue3", "red3"))
  )
}

#-------------------------------------------------------------------------------
#### Nationally representative 2016-2020 ####

# TOO FEW OBSERVATIONS
# Anderson Creek Fire: March 22 - March 30 2016, c(40031, 20007)
# plot_fire(dat_test, 
#           ymd("2016-03-22"),
#           ymd("2016-03-30"),
#           c("40031", "20007"), 
#           "Anderson Creek Fire")
# ggsave(paste0(path_results, "AOD_fire_national_2016-2019_AndersonCreek.png"),
#        width = width, height = height)

# Rice Ridge Fire: July 24 - Sept 7 2017, c(30077, 30063)
plot_fire(dat_test,
          ymd("2017-07-24"),
          ymd("2017-09-07"),
          c("30077", "30063"),
          "Rice Ridge Fire")
ggsave(paste0(path_results, "AOD_fire_national_2016-2019_RiceRidge.png"),
       width = width, height = height)

# NO OBSERVATIONS
# Goodwin Fire: June 24 - July 10 2017, c(04025)
# plot_fire(dat_test,
#           ymd("2017-06-24"),
#           ymd("2017-07-10"),
#           "04025",
#           "Goodwin Fire")
# ggsave(paste0(path_results, "AOD_fire_national_2016-2019_Goodwin.png"),
#        width = width, height = height)

# Bighorn Fire: June 5 - July 23 2020, c(04019)
plot_fire(dat_test,
          ymd("2020-06-05"),
          ymd("2020-07-23"),
          "04019",
          "Bighorn Fire")
ggsave(paste0(path_results, "AOD_fire_national_2016-2019_Bighorn.png"),
       width = width, height = height)

# NOT IN DATA TIME SPAN
# Evans Canyon Fire: Aug 31 - Sept 9 2020, c(53077)
# plot_fire(dat_test,
#           ymd("2020-08-31"),
#           ymd("2020-09-09"),
#           "53077",
#           "Evans Canyon Fire")
# ggsave(paste0(path_results, "AOD_fire_national_2016-2019_EvansCanyon.png"),
#        width = width, height = height)

# NOT IN DATA TIME SPAN
# Mullen Fire: Sept 18 2020 - Jan 4 2021, c(56007, 56001, 08057)
# plot_fire(dat_test, 
#           ymd("2020-09-18"),
#           ymd("2021-01-04"),
#           c("56007", "56001", "08057"),
#           "Mullen Fire")
# ggsave(paste0(path_results, "AOD_fire_national_2016-2019_Mullen.png"),
#        width = width, height = height)

#-------------------------------------------------------------------------------
#### California 2019 ####
# Kincade fire - Sonoma County - 10/22/19-11/9/19
plot_fire(dat_test,
          ymd("2019-10-22"),
          ymd("2019-11-09"),
          "Sonoma",
          "Kincade Fire", 
          fire_state = "CA")
ggsave(paste0(path_results, "AOD_fire_CA_2019_Kincade.png"),
       width = width, height = height)

# Taboose fire - Inyo County - 9/4/19-10/7/19
plot_fire(dat_test,
          ymd("2019-09-04"),
          ymd("2019-10-07"),
          "Inyo",
          "Taboose Fire", 
          fire_state = "CA")
ggsave(paste0(path_results, "AOD_fire_CA_2019_Taboose.png"),
       width = width, height = height)

# TOO FEW OBSERVATIONS
# Maria fire - Ventura County - 10/31/19 - 11/6/19 
# plot_fire(dat_test,
#           ymd("2019-10-31"),
#           ymd("2019-11-06"),
#           "Ventura", 
#           "Maria Fire", 
#           fire_state = "CA")
# ggsave(paste0(path_results, "AOD_fire_CA_2019_Maria.png"),
#        width = width, height = height)

# TOO FEW OBSERVATIONS
# Stagecoach fire - Kern County - August 2019
# plot_fire(dat_test,
#           ymd("2019-08-01"),
#           ymd("2019-08-31"),
#           "Kern",
#           "Stagecoach Fire", 
#           fire_state = "CA")
# ggsave(paste0(path_results, "AOD_fire_CA_2019_Stagecoach.png"),
#        width = width, height = height)

#-------------------------------------------------------------------------------
# What is the upper bound on improved performance?
df_fire <- dat_test %>% 
  group_by(date, state, county_name, spec) %>% 
  summarize(across(c(smokePM, pred), mean)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = spec,
              names_prefix = "pred_",
              values_from = pred) %>% 
  filter((smokePM > pred_aod & pred_aod > pred_baseline) | 
           (smokePM < pred_aod & pred_aod < pred_baseline)) %>% 
  mutate(diff = abs(pred_aod - pred_baseline)) %>% 
  arrange(desc(diff)) %>% 
  filter(diff > 30) %>% 
  # View()
  mutate(year = year(date)) %>% 
  group_by(year, state, county_name) %>%
  summarize(start_date = min(date),
            end_date = max(date)) %>%
  filter(end_date - start_date > days(7),
         end_date - start_date < months(2)) %>% 
  ungroup()

# Plot time series for counties with greatest improved prediction performance
fires <- df_fire %>% 
  split(seq(nrow(df_fire))) %>% 
  lapply(function(fire) {
    return(plot_fire(dat_test,
                     fire$start_date,
                     fire$end_date,
                     gsub(" County", "", fire$county_name),
                     fire$county_name,
                     fire_state = fire$state))
  })

# Arrange fires in grid
nc <- 2
ggarrange(plotlist = fires,
          ncol = nc,
          nrow = ceiling(length(fires)/nc),
          common.legend = TRUE)

# Save
ggsave(paste0(path_results, "AOD_fire_counties.png"),
       width = 12, height = 12)

#-------------------------------------------------------------------------------
#### Variable importance ####
#-------------------------------------------------------------------------------
rm(dat_panel, dat_preds, dat_test, dat_test_sd_50, df_eval, df_fire, fires)

# Locate random forest files
mdls <- grep("^AOD_model", list.files(path_project), value = TRUE)

# Get variable importance
get_importance <- function(filename) {
  x <- readRDS(paste0(path_project, filename))
  filename <- filename %>% strsplit("_|\\.") %>% unlist()
  spec <- ifelse(filename[3] == "spec1", "baseline", "aod")
  fold <- filename[4]
  nc <- nchar(fold)
  fold <- fold %>% substr(nc, nc) %>% as.integer()
  return(x$variable.importance %>% 
           as.data.frame() %>% 
           rownames_to_column(var = "variable") %>% 
           rename(importance = ".") %>% 
           mutate(spec = spec,
                  fold = fold))
}
start_time <- get_start_time()
importance <- mdls %>% 
  map_dfr(get_importance) %>% 
  mutate(spec = factor(spec, levels = c("baseline", "aod")))
print_time(start_time)

# Save variable importance data frame
saveRDS(importance, paste0(path_project, "AOD_random_forest_importance.rds"))

#-------------------------------------------------------------------------------
# Prepare data frame for plotting importance
df_imp <- importance %>% 
  group_by(variable, spec) %>% 
  summarize(importance = mean(importance)) %>% 
  ungroup() %>% 
  mutate(feature_interest = ifelse(variable == "aod", variable, "other") %>% 
           factor(levels = c("other", "aod")))

# Plot variable importance averaged across folds
ggplot(data = df_imp,
       mapping = aes(x = importance,
                     y = reorder_within(variable, importance, spec),
                     color = feature_interest)) + 
  geom_point() + 
  facet_wrap(vars(spec), ncol = 2, scales = "free") + 
  scale_y_reordered() + 
  scale_color_manual(values = c("black", "red3")) + 
  theme_bw() + 
  labs(y = "") + 
  theme(legend.position = "")

# Save
ggsave(paste0(path_results, "AOD_importance_avgfolds.png"),
       width = width, height = height)

#-------------------------------------------------------------------------------
# Prepare data frame for plotting importance by fold
df_imp <- importance %>% 
  mutate(feature_interest = ifelse(variable == "aod", variable, "other") %>% 
           factor(levels = c("other", "aod")))

# Plot variable importance by fold
ggplot(data = df_imp,
       mapping = aes(x = importance,
                     y = reorder_within(variable, importance, interaction(fold, spec)),
                     color = feature_interest)) + 
  geom_point() + 
  facet_wrap(vars(fold, spec), ncol = 2, scales = "free") + 
  scale_y_reordered() + 
  scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) + 
  scale_color_manual(values = c("black", "red3")) + 
  theme_bw() + 
  labs(y = "") + 
  theme(legend.position = "")

# Save
ggsave(paste0(path_results, "AOD_importance_byfold.png"),
       width = 12, height = 15)
