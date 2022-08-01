source("work/06_complex_MERRA_model/00_utils.R")

library(fixest)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(ggpubr)
library(sf)
library(tigris)
library(lubridate)
library(devtools)
# Define function for setting coord_cartesian by facet panel manually
source_gist(id = "6057f7995c117bb787495dc14a228d5d",
            filename = "coord_cartesian_panels.R")

width <- 10
height <- 6

#-------------------------------------------------------------------------------
# Assess Performance Without and With Spatial Re-weighting
# Written by Jessica
# Last edited September 2021
# 
# "specification" here actually just refers to weighting scheme during model
# training.
#-------------------------------------------------------------------------------
# Load predictions
dat_preds <- readRDS(paste0(path_project, "spatial_reweighting_random_forest_predictions.rds"))

# Limit to test data set
dat_test <- dat_preds %>% 
  pivot_longer(cols = starts_with("pred"),
               names_prefix = "pred_",
               names_to = c("spec", "test_fold"),
               names_sep = "_",
               values_to = "pred") %>% 
  mutate(spec = ifelse(spec == "spec1", "unweighted", "weighted"),
         test_fold = gsub("fold", "", test_fold) %>% as.integer()) %>% 
  filter(fold == test_fold) %>% 
  select(-test_fold) %>% 
  unite("state_month", state, month, remove = FALSE) %>% 
  mutate(year = year(date)) %>% 
  unite("state_year", state, year, remove = FALSE)

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
  mutate(spec = factor(spec, levels = c("unweighted", "weighted", "di", "reid")))

# Create different evaluation samples
dat_test_sd <- dat_test %>% 
  filter(smoke_day == 1) %>% 
  add_column(eval_sample = "Smoke")
dat_test_sd_50 <- dat_test %>% 
  filter(smoke_day == 1, smokePM > 50) %>% 
  add_column(eval_sample = "Smoke50")

#-------------------------------------------------------------------------------
#### Overall ####
# Prepare data frame for plotting metric by specification and evaluation sample
df_eval <- dat_test %>% 
  add_column(eval_sample = "Full") %>% 
  bind_rows(dat_test_sd, dat_test_sd_50) %>% 
  group_by(spec, eval_sample) %>% 
  summarize(r2 = r2(feols(smokePM ~ pred,
                          data = cur_data()), "r2"),
            wr2 = r2(feols(smokePM ~ pred | epa_id + state_month + state_year,
                           data = cur_data()), "wr2"),
            rmse = sqrt(mean((smokePM - pred)^2))) %>% 
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
  mutate(metric = factor(metric, levels = c("r2", "wr2", "rmse")),
         eval_sample = factor(eval_sample, levels = c("Full", "Smoke", "Smoke50")))

# Plot metric by training sample and evaluation sample
ggplot(data = df_eval, 
       mapping = aes(x = eval_sample, y = spec, fill = rank)) + 
  geom_tile() + 
  geom_text(aes(label = value)) + 
  facet_wrap(vars(metric), ncol = 4) +
  scale_fill_gradient(low = "skyblue2", high = "palevioletred2") + 
  labs(caption = "Rank 1 means best performance.") + 
  theme_classic()

# Save
ggsave(paste0(path_results, "spatial_reweighting_performance_overall.png"),
       width = 12, height = 4)

#-------------------------------------------------------------------------------
#### By fold ####
# Prepare data frame for plotting metric by specification, evaluation sample, and fold
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
  mutate(metric = factor(metric, levels = c("r2", "wr2", "rmse", "me")),
         eval_sample = factor(eval_sample, levels = c("Full", "Smoke", "Smoke50")))

# Plot metric by model specification, evaluation sample, and fold
ggplot(data = df_eval, 
       mapping = aes(x = eval_sample, y = spec, fill = rank)) + 
  geom_tile() + 
  geom_text(aes(label = value)) + 
  facet_wrap(vars(fold, metric), ncol = 4) +
  scale_fill_gradient(low = "skyblue2", high = "palevioletred2") + 
  labs(caption = "Rank 1 means best performance.") + 
  theme_classic()

# Save
ggsave(paste0(path_results, "spatial_reweighting_performance_byfold.png"),
       width = 15, height = 12)

#-------------------------------------------------------------------------------
#### R2 train vs test for unweighted and weighted ####
# Get train and test data
dat_full <- dat_preds  %>% 
  pivot_longer(cols = starts_with("pred"),
               names_prefix = "pred_",
               names_to = c("spec", "test_fold"),
               names_sep = "_",
               values_to = "pred") %>% 
  mutate(spec = ifelse(spec == "spec1", "unweighted", "weighted"),
         test_fold = gsub("fold", "", test_fold) %>% as.integer(),
         set = ifelse(fold == test_fold, "test", "train") %>% factor(levels = c("train", "test"))) %>% 
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
  mutate(spec = factor(spec, levels = c("unweighted", "weighted", "di", "reid"))) %>% 
  filter(spec %in% c("unweighted", "weighted"))

# Calculate R2 by train vs test
df_pooled <- dat_full %>% 
  group_by(spec, set) %>% 
  summarize(r2 = r2(feols(smokePM ~ pred,
                          data = cur_data()), "r2")) %>% 
  ungroup()

# Calculate R2 by train vs test and test fold
df_test_fold <- dat_full %>% 
  group_by(spec, test_fold, set) %>% 
  summarize(r2 = r2(feols(smokePM ~ pred,
                          data = cur_data()), "r2")) %>% 
  ungroup()

# Calculate R2 by train vs test, test fold, and train fold
# df_fold <- dat_full %>% 
#   group_by(spec, fold, test_fold, set) %>% 
#   summarize(r2 = r2(feols(smokePM ~ pred,
#                           data = cur_data()), "r2")) %>% 
#   ungroup()

# Plot
ggplot() + 
  geom_point(data = df_test_fold, mapping = aes(set, r2, color = spec)) +
  # geom_line(data = df_test_fold, mapping = aes(set, r2, group = interaction(spec, test_fold)), color = "red") + 
  # geom_point(data = df_pooled, mapping = aes(set, r2)) + 
  geom_line(data = df_pooled, mapping = aes(set, r2, group = spec, color = spec)) + 
  theme_classic()

# Save
ggsave(paste0(path_results, "spatial_reweighting_performance_test_train.png"),
       width = 15, height = 12)

#-------------------------------------------------------------------------------
#### At stations with greatest change between unweighted and weighted ####
# 242 stations (86%) are excluded due to insufficient variation in any 
# evaluation sample
complete_stations <- dat_test %>% 
  add_column(eval_sample = "Full") %>% 
  bind_rows(dat_test_sd, dat_test_sd_50) %>% 
  group_by(epa_id, eval_sample, spec) %>% 
  summarize(unique_smokePM = length(unique(smokePM))) %>% 
  ungroup() %>% 
  filter(unique_smokePM > 2) %>% 
  group_by(epa_id, spec) %>% 
  summarize(complete = all(c("Full", "Smoke", "Smoke50") %in% eval_sample)) %>% 
  ungroup() %>% 
  filter(complete) %>% 
  pull(epa_id) %>% 
  unique()

# Would require variation even in particular months that are all non-smoke days
# Too stringent
# complete_stations <- dat_test %>% 
#   add_column(eval_sample = "Full") %>% 
#   bind_rows(dat_test_sd, dat_test_sd_50) %>% 
#   group_by(epa_id, eval_sample, spec, state_year, state_month) %>% 
#   summarize(variance_smokePM = var(smokePM),
#             variance_pred = var(pred)) %>% 
#   ungroup(state_month, state_year) %>% 
#   summarize(variance_smokePM_keep = !(any(c(0, NA) %in% variance_smokePM)),
#             variance_pred_keep = !(any(c(0, NA) %in% variance_pred))) %>% 
#   ungroup() %>% 
#   filter(variance_smokePM_keep, variance_pred_keep) %>% 
#   group_by(epa_id, spec) %>%
#   summarize(complete = all(c("Full", "Smoke", "Smoke50") %in% eval_sample)) %>%
#   ungroup() %>%
#   filter(complete) %>%
#   pull(epa_id) %>% 
#   unique()

# Will end up with no variation in within some units
# Problem for within R2
# complete_stations <- dat_test_sd_50 %>% 
#   group_by(epa_id, eval_sample, spec, state_year, state_month) %>% 
#   summarize(variance_smokePM = var(smokePM),
#             variance_pred = var(pred)) %>% 
#   ungroup(state_month, state_year) %>% 
#   summarize(variance_smokePM_keep = !(any(c(0, NA) %in% variance_smokePM)),
#             variance_pred_keep = !(any(c(0, NA) %in% variance_pred))) %>% 
#   ungroup() %>% 
#   filter(variance_smokePM_keep, variance_pred_keep) %>% 
#   pull(epa_id) %>% 
#   unique()

# Prepare data frame for plotting metric by specification, evaluation sample, and station
df_eval <- dat_test %>%
  add_column(eval_sample = "Full") %>%
  bind_rows(dat_test_sd, dat_test_sd_50) %>%
  filter(epa_id %in% complete_stations) %>%
  group_by(spec, eval_sample, epa_id) %>%
  summarize(r2 = r2(feols(smokePM ~ pred,
                          data = cur_data()), "r2"),
            rmse = sqrt(mean((smokePM - pred)^2)),
            me = mean(smokePM - pred)) %>%
  ungroup() %>%
  pivot_longer(cols = c(r2, rmse, me),
               names_to = "metric") %>%
  group_by(metric, eval_sample, epa_id) %>%
  arrange(abs(value)) %>%
  mutate(rank = case_when(metric == "r2" ~ rev(row_number()),
                          metric == "rmse" ~ row_number(),
                          metric == "me" ~ row_number()),
         value = ifelse(round(value, 3) == 0, signif(value, 2), round(value, 3))) %>%
  ungroup() %>%
  mutate(metric = factor(metric, levels = c("r2", "rmse", "me")),
         eval_sample = factor(eval_sample, levels = c("Full", "Smoke", "Smoke50")))

# Within R2 not possible to look at by station
# df_eval <- dat_test %>%
#   add_column(eval_sample = "Full") %>%
#   bind_rows(dat_test_sd, dat_test_sd_50) %>%
#   filter(epa_id %in% complete_stations) %>%
#   group_by(spec, eval_sample, epa_id) %>%
#   summarize(r2 = r2(feols(smokePM ~ pred,
#                           data = cur_data()), "r2"),
#             wr2 = r2(feols(smokePM ~ pred | state_month + state_year,
#                            data = cur_data()), "wr2"),
#             rmse = sqrt(mean((smokePM - pred)^2)),
#             me = mean(smokePM - pred)) %>%
#   ungroup() %>%
#   pivot_longer(cols = c(r2, wr2, rmse, me),
#                names_to = "metric") %>%
#   group_by(metric, eval_sample, epa_id) %>%
#   arrange(abs(value)) %>%
#   mutate(rank = case_when(metric == "r2" ~ rev(row_number()),
#                           metric == "wr2" ~ rev(row_number()),
#                           metric == "rmse" ~ row_number(),
#                           metric == "me" ~ row_number()),
#          value = ifelse(round(value, 3) == 0, signif(value, 2), round(value, 3))) %>%
#   ungroup() %>%
#   mutate(metric = factor(metric, levels = c("r2", "wr2", "rmse", "me")),
#          eval_sample = factor(eval_sample, levels = c("Full", "Smoke", "Smoke50")))

# Set metric and evaluation sample for looking at greatest change
change_metric <- "rmse"
change_eval_sample <- "Smoke"
num_change_stations <- 3

# Filter to stations with greatest improvement in smoke day RMSE
change_stations <- df_eval %>% 
  filter(metric == change_metric,
         eval_sample == change_eval_sample,
         spec %in% c("unweighted", "weighted")) %>% 
  select(-rank) %>% 
  pivot_wider(names_from = spec) %>% 
  mutate(change = ifelse(metric == "r2", weighted - unweighted, unweighted - weighted)) %>% 
  slice_max(change, n = num_change_stations) %>% 
  pull(epa_id)
df_eval_stations <- df_eval %>% filter(epa_id %in% change_stations)

# Plot metric by model specification, evaluation sample, and fold
ggplot(data = df_eval_stations, # %>% filter(metric == "r2", epa_id %in% unique(epa_id)[37:40]), 
       mapping = aes(x = eval_sample, y = spec, fill = rank)) + 
  geom_tile() + 
  geom_text(aes(label = value)) + 
  facet_wrap(vars(epa_id, metric), ncol = 3) +
  scale_fill_gradient(low = "skyblue2", high = "palevioletred2") + 
  labs(caption = "Rank 1 means best performance.") + 
  theme_classic()

# Save
ggsave(paste0(path_results, "spatial_reweighting_performance_bystation_improve.png"),
       width = 15, height = 12)

# Filter to stations with greatest worsening in smoke day RMSE
change_stations <- df_eval %>% 
  filter(metric == change_metric,
         eval_sample == change_eval_sample,
         spec %in% c("unweighted", "weighted")) %>% 
  select(-rank) %>% 
  pivot_wider(names_from = spec) %>% 
  mutate(change = ifelse(metric == "r2", weighted - unweighted, unweighted - weighted)) %>% 
  slice_min(change, n = num_change_stations) %>% 
  pull(epa_id)
df_eval_stations <- df_eval %>% filter(epa_id %in% change_stations)

# Plot metric by model specification, evaluation sample, and fold
ggplot(data = df_eval_stations, # %>% filter(metric == "r2", epa_id %in% unique(epa_id)[37:40]), 
       mapping = aes(x = eval_sample, y = spec, fill = rank)) + 
  geom_tile() + 
  geom_text(aes(label = value)) + 
  facet_wrap(vars(epa_id, metric), ncol = 3) +
  scale_fill_gradient(low = "skyblue2", high = "palevioletred2") + 
  labs(caption = "Rank 1 means best performance.") + 
  theme_classic()

# Save
ggsave(paste0(path_results, "spatial_reweighting_performance_bystation_worsen.png"),
       width = 15, height = 12)

#-------------------------------------------------------------------------------
#### By number of smoke day observations ####
# Throw out station-evaluation samples that don't have variation in smoke PM
no_smoke_stations <- dat_test %>% 
  group_by(epa_id) %>% 
  summarize(unique_smokePM = length(unique(smokePM))) %>% 
  ungroup() %>% 
  filter(unique_smokePM > 2) %>% 
  pull(epa_id) %>% 
  unique()
no_smoke_stations_sd <- dat_test_sd %>% 
  group_by(epa_id) %>% 
  summarize(unique_smokePM = length(unique(smokePM))) %>% 
  ungroup() %>% 
  filter(unique_smokePM > 2) %>% 
  pull(epa_id) %>% 
  unique()
no_smoke_stations_sd_50 <- dat_test_sd_50 %>% 
  group_by(epa_id) %>% 
  summarize(unique_smokePM = length(unique(smokePM))) %>% 
  ungroup() %>% 
  filter(unique_smokePM > 2) %>% 
  pull(epa_id) %>% 
  unique()

# Prepare data frame for plotting metric by specification, evaluation sample, and 
# number of smoke day observations at a station
df_eval <- dat_test %>% 
  filter(epa_id %in% no_smoke_stations) %>% 
  add_column(eval_sample = "Full") %>%
  bind_rows(dat_test_sd %>% filter(epa_id %in% no_smoke_stations_sd), 
            dat_test_sd_50 %>% filter(epa_id %in% no_smoke_stations_sd_50)) %>%
  group_by(spec, eval_sample, epa_id) %>%
  summarize(r2 = r2(feols(smokePM ~ pred,
                          data = cur_data()), "r2"),
            rmse = sqrt(mean((smokePM - pred)^2)),
            me = mean(smokePM - pred)) %>%
  ungroup() %>%
  pivot_longer(cols = c(r2, rmse, me),
               names_to = "metric") %>%
  group_by(metric, eval_sample, epa_id) %>%
  arrange(abs(value)) %>%
  mutate(rank = case_when(metric == "r2" ~ rev(row_number()),
                          metric == "rmse" ~ row_number(),
                          metric == "me" ~ row_number()),
         value = ifelse(round(value, 3) == 0, signif(value, 2), round(value, 3))) %>%
  ungroup() %>%
  mutate(metric = factor(metric, levels = c("r2", "rmse", "me")),
         eval_sample = factor(eval_sample, levels = c("Full", "Smoke", "Smoke50")))
df_eval <- df_eval %>% left_join(dat_test_sd %>% count(epa_id))

# Plot metric by model specification, evaluation sample, and number of smoke day
# observations as a scatter
ggplot(data = df_eval,
       mapping = aes(x = n, y = value)) + 
  geom_point(mapping = aes(color = spec), size = 1, alpha = 0.8) + 
  facet_wrap(vars(eval_sample, metric), scales = "free") + 
  theme_classic() + 
  labs(x = "Number of smoke day observations",
       y = "")

# Save
ggsave(paste0(path_results, "spatial_reweighting_performance_bysmokedays_scatter.png"),
       width = 12, height = 10)

# Plot metric by model specification, evaluation sample, and number of smoke day
# observations smoothed
p <- ggplot(data = df_eval,
            mapping = aes(x = n, y = value)) + 
  geom_smooth(se = FALSE, mapping = aes(color = spec), size = 0.5) + 
  geom_rug(sides = "b") + 
  facet_wrap(vars(eval_sample, metric), scales = "free") + 
  theme_classic() + 
  labs(x = "Number of smoke day observations",
       y = "")

p + coord_cartesian_panels(
  panel_limits = tribble(
    ~eval_sample, ~metric, ~ymin, ~ymax,
    "Full", "r2", 0.35, 0.95,
    "Full", "rmse", 0, 5,
    "Full", "me", -0.3, 0.6,
    "Smoke", "r2", 0.15, 0.95,
    "Smoke", "rmse", 2, 11,
    "Smoke", "me", -2, 3,
    "Smoke50", "r2", 0.1, 1,
    "Smoke50", "rmse", 0, 90,
    "Smoke50", "me", 0, 63
  )
)

# Save
ggsave(paste0(path_results, "spatial_reweighting_performance_bysmokedays_smooth.png"),
       width = 15, height = 12)

#-------------------------------------------------------------------------------
#### R2 by station mapped ####
# Get state shapes
us_states <- states()
west_coast <- us_states %>% filter(STUSPS %in% c("CA", "OR", "WA"))

# Focus on R2 for unweighted and weighted
df_eval <- df_eval %>% 
  filter(spec %in% c("unweighted", "weighted"),
         metric == "r2") %>% 
  left_join(dat_preds %>% select(epa_id, lon, lat) %>% distinct()) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Plot number of smoke day observations by evaluation sample
ggplot(df_eval %>% filter(spec == "unweighted", eval_sample == "Full")) + 
  geom_sf(data = west_coast) + 
  geom_sf(aes(color = n)) + 
  facet_wrap(vars(eval_sample), nrow = 1) + 
  theme_void() + 
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = NA)) + 
  scale_color_gradient(low = "gray20", high = "red")

# Save
ggsave(paste0(path_results, "spatial_reweighting_performance_map_n.png"),
       width = 15, height = 7)

# Plot stations on map by weighting scheme and evaluation sample
ggplot(df_eval) + 
  geom_sf(data = west_coast) + 
  geom_sf(aes(color = value)) + 
  facet_wrap(vars(eval_sample, spec), nrow = 1) + 
  theme_void() + 
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = NA)) + 
  labs(color = "R2")

# p_full <- ggplot(df_eval %>% filter(eval_sample == "Full")) + 
#   geom_sf(data = west_coast) + 
#   geom_sf(aes(color = value)) + 
#   facet_wrap(vars(eval_sample, spec)) + 
#   theme_void() + 
#   theme(plot.background = element_rect(color = "black", fill = NA, size = 1))
# p_sd <- ggplot(df_eval %>% filter(eval_sample == "Smoke")) + 
#   geom_sf(data = west_coast) + 
#   geom_sf(aes(color = value)) + 
#   facet_wrap(vars(eval_sample, spec)) + 
#   theme_void() + 
#   theme(plot.background = element_rect(color = "black", fill = NA, size = 1))
# p_sd_50 <- ggplot(df_eval %>% filter(eval_sample == "Smoke50")) + 
#   geom_sf(data = west_coast) + 
#   geom_sf(aes(color = value)) + 
#   facet_wrap(vars(eval_sample, spec)) + 
#   theme_void() + 
#   theme(plot.background = element_rect(color = "black", fill = NA, size = 1))
# ggarrange(p_full, p_sd, p_sd_50,
#           nrow = 1,
#           # labels = c(levels(df_eval$eval_sample)),
#           # vjust = 2,
#           common.legend = TRUE)

# Save
ggsave(paste0(path_results, "spatial_reweighting_performance_map_R2.png"),
       width = 15, height = 7)

# Look at the change in R2
df_change <- df_eval %>% 
  select(spec, eval_sample, epa_id, value) %>% 
  st_drop_geometry() %>% 
  pivot_wider(names_from = spec) %>% 
  left_join(df_eval %>% select(epa_id) %>% distinct()) %>% 
  st_as_sf()

# Plot
ggplot(df_change) + 
  geom_sf(data = west_coast) + 
  geom_sf(aes(color = weighted - unweighted)) + 
  facet_wrap(vars(eval_sample), nrow = 1) + 
  theme_void() + 
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "white", color = NA)) + 
  labs(color = "R2_weighted - R2_unweighted") + 
  scale_color_gradient2(midpoint = 0, low="blue", mid="white", high="red")#,
                        # limits = c(-0.3, 0.3), oob = scales::squish)

# Save
ggsave(paste0(path_results, "spatial_reweighting_performance_map_R2_change.png"),
       width = 15, height = 7)
