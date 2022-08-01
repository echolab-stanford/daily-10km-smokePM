source("./scripts/0_config.R")

smoke <- list.files(paste0(data_path, "/smoke_days"), 
                    full.names = TRUE) %>% 
  map_dfr(function(x) readRDS(x) %>% select(-total, -light, -medium, -dense, -note_smoke_date_repaired_geometry, -note_smoke_date_empty_data)) %>% 
  rename(grid_id_10km = id_grid) 

smoke_fill <- smoke %>%
  arrange(grid_id_10km, date) %>% 
  group_by(grid_id_10km) %>%
  mutate(lag1 = lag(smoke_day, 1), 
         lag2 = lag(smoke_day, 2), 
         lead1 = lead(smoke_day, 1), 
         lead2 = lead(smoke_day, 2)) %>% 
  filter(is.na(smoke_day)) %>% 
  rowwise %>% 
  mutate(smoke_day_fill = case_when(!is.na(smoke_day) ~ list(c(smoke_day)),
                                    is.na(smoke_day) & !is.na(lag1) & !is.na(lead1) ~ list(c(lag1, lead1)), 
                                    T ~ list(c(lag2, lag1, lead1, lead2))),
         smoke_day_fill = mean(smoke_day_fill, na.rm = T)) 

# default to zero if all neighbors weren't also smoke days. we only need to keep track of the 1s
smoke_fill %<>% 
  ungroup %>%
  mutate(smoke_day_fill = (smoke_day_fill == 1)*1) %>% 
  select(grid_id_10km, date, smoke_day_fill) %>% 
  filter(smoke_day_fill == 1)

# save RDS of all smoke days
smoke %>% 
  left_join(smoke_fill, by = c("grid_id_10km", "date")) %>% 
  mutate(smoke_day = ifelse(is.na(smoke_day), smoke_day_fill, smoke_day)) %>% 
  filter(smoke_day == 1) %>% 
  select(grid_id_10km, date, smoke_day, note_smoke_date_not_online) %>% 
  saveRDS(paste0(data_path, "/3_intermediate/all_smoke_days.rds"))