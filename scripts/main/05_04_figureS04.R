source("scripts/setup/00_01_load_packages.R")
source("scripts/setup/00_02_load_functions.R")
source("scripts/setup/00_03_load_settings.R")

# ------------------------------------------------------------------------------
# Written by: Jessica Li
# Plots supplemental figure 4.
# ------------------------------------------------------------------------------
max_nchar = 35

start_date = "20060101"
end_date = "20201231"
all_dates = seq.Date(ymd(start_date), ymd(end_date), by = "day")
duration_days = 6

# Fire points
fire_dates_not_online = readRDS(file.path(path_data, "fire", "fire_dates_not_online.rds"))
fire_dates_empty_data = readRDS(file.path(path_data, "fire", "fire_dates_empty_data.rds"))
fire_dates_clusters_too_small = readRDS(file.path(path_data, "fire", "fire_dates_clusters_too_small.rds"))

# HYSPLIT points
hysplit_dates_without_start_duration = seq.Date(ymd(start_date), ymd("20060418"), by = "day")
hysplit_dates_not_online = ymd(readRDS(file.path(path_data, "HYSPLIT", "miscellaneous", "hysplit_dates_not_online.rds")))
hysplit_dates_gis_corrupt = ymd(readRDS(file.path(path_data, "HYSPLIT", "miscellaneous", "hysplit_dates_gis_corrupt.rds")))
hysplit_dates_oddly_empty = ymd(readRDS(file.path(path_data, "HYSPLIT", "miscellaneous", "hysplit_dates_oddly_empty.rds")))
hysplit_na_dates_original = c(hysplit_dates_without_start_duration, hysplit_dates_not_online, hysplit_dates_gis_corrupt, hysplit_dates_oddly_empty)
hysplit_na_dates = unique(unlist(lapply(hysplit_na_dates_original, function(x) seq.Date(x, x + days(duration_days), by = "day"))))
class(hysplit_na_dates) = "Date"
hysplit_na_dates_affected = setdiff(hysplit_na_dates, hysplit_na_dates_original)
class(hysplit_na_dates_affected) = "Date"
hysplit_dates_without_start_duration = setdiff(hysplit_dates_without_start_duration, hysplit_dates_not_online)
class(hysplit_dates_without_start_duration) = "Date"

# Smoke plumes
smoke_dates_not_online = ymd(readRDS(file.path(path_data, "smoke", "smoke_dates_not_online.rds")))
smoke_dates_empty_data = ymd(readRDS(file.path(path_data, "smoke", "smoke_dates_empty_data.rds")))
smoke_dates_repaired_geometry = ymd(readRDS(file.path(path_data, "smoke", "smoke_dates_repaired_geometry.rds")))

# Datasets
dataset = create_node_df(
  n = 3,
  label = c("Fire points", "HYSPLIT points", "Smoke plumes")
)

# Reasons for missingness
reasons = create_node_df(
  n = 9,
  label = str_wrap(c("Not online", "Empty data", "Clusters too small", 
                     "No start time nor duration", "Not online", 
                     "Corrupt shapefile", "Oddly empty", "Affected by previous missing date", 
                     "Not online"), max_nchar) %>% 
    paste0("\n", 
           sapply(list(fire_dates_not_online, fire_dates_empty_data, fire_dates_clusters_too_small,
                       hysplit_dates_without_start_duration, hysplit_dates_not_online,
                       hysplit_dates_gis_corrupt, hysplit_dates_oddly_empty, hysplit_na_dates_affected,
                       smoke_dates_not_online), length))
)

# How missingness gets treated
treatments = create_node_df(
  n = 4,
  label = c("Set distance = NA,\narea = 0, num_points = 0",
            "Exclude from training;\nfill using temporal NN for prediction",
            "Treat trajectory point counts as MIA",
            "Exclude from medians and training;\nfill using temporal NN for prediction") %>% 
    paste0("\n", 
           c(length(setdiff(fire_dates_not_online, smoke_dates_not_online)) + length(fire_dates_empty_data) + length(fire_dates_clusters_too_small), 
             length(intersect(fire_dates_not_online, smoke_dates_not_online)),
             sum(sapply(list(hysplit_dates_without_start_duration, hysplit_dates_not_online, 
                             hysplit_dates_gis_corrupt, hysplit_dates_oddly_empty, 
                             hysplit_na_dates_affected),
                        length)),
             length(smoke_dates_not_online)))
)
nodes = combine_ndfs(dataset, reasons, treatments)

# Connect nodes
edges = create_edge_df(
  from = c(1, 1, 1, 2, 2, 2, 2, 2, 3, 4, 4, 5, 6, 7, 8, 9, 10, 11, 12),
  to = c(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 13, 13, 15, 15, 15, 15, 15, 16)
)

# Make graph
g = create_graph(
  nodes_df = nodes,
  edges_df = edges,
  attr_theme = "lr"
) %>% 
  set_node_attrs("shape", "rectangle") %>% 
  set_node_attrs("fillcolor", "white") %>% 
  set_node_attrs("fixedsize", F) %>% 
  set_node_attrs("fontcolor", "black") %>% 
  set_node_attrs("color", "black") %>% 
  set_edge_attrs("color", "black")

# Preview
render_graph(g)

# Save
export_graph(g, file_name = file.path(path_figures, "figureS04.pdf"), file_type = "pdf")
