#-------------------------------------------------------------------------------
# Utilities
# Modified by Jessica from Sam and Anne's code
# Last edited July 2021
#-------------------------------------------------------------------------------
# Define paths
if (Sys.info()["user"] == "jessssli") {
  path_dropbox <- "~/BurkeLab Dropbox/Data/"
  path_github <- "~/Documents/GitHub/smoke_PM_prediction/"
  path_results <- paste0(path_github, "work/05_get_HYSPLIT_height/results/")
  path_project <- paste0(path_dropbox, "../Projects/smoke_PM_prediction/get_HYSPLIT_height/")
}

# Set up audio
audio <- TRUE

# Load packages
if (audio) {library(beepr)}

#-------------------------------------------------------------------------------
#### Define functions ####
# Alerts
beep_alert <- function(message = "Just finished running!", interval = 0.8) {
  beep(); Sys.sleep(interval); beep(); Sys.sleep(interval);  beep()
  system(paste("say",  message))
}

quick_beep_alert <- function(interval = 0.2) {
  beep(); Sys.sleep(interval); beep()
}

# Timers
print_time <- function(start, unit = "auto", message = "Time elapsed:") {
  end <- Sys.time()
  d <- difftime(time1 = end, time2 = start, units = unit)
  t <- round(d, digits = 1)
  u <- units(d)
  
  print(paste("Start time:", start))
  print(paste("End time:", end))
  message <- paste(message, t, u)
  print(message)
  return(d)
}

get_start_time <- function(message = "Time started:") {
  t <- Sys.time()
  print(paste(message, t))
  return(t)
}

# Data frame explorers
find_na_rows <- function(df) {
  return(df[rowSums(is.na(df)) > 0,])
}

find_na_cols <- function(df, header = FALSE) {
  return(if (header) names(df[,colSums(is.na(df)) > 0])
         else df[,colSums(is.na(df)) > 0])
}

find_dups <- function(df) {
  return(df[duplicated(df),])
}

print_unique <- function(df, n = 10, increasing = NULL) {
  for (vbl in names(df)) {
    vals <- unique(df[[vbl]])
    if (!is.null(increasing)) {
      stopifnot(is.logical(increasing))
      vals <- if (increasing) sort(vals) else sort(vals, decreasing = TRUE)
    }
    print(vbl)
    print(vals[1:min(n, length(vals))])
  }
}

random_sample <- function(df, n = 10) {
  return(df[sample(1:nrow(df), n),])
}

# Parallelization helpers
split_chunks <- function(v, chunks) {
  return(split(v, cut(seq_along(v), chunks, labels = FALSE)))
}

# Plotting helpers from GitHub repo drlib
reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

scale_y_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_y_discrete(labels = function(x) gsub(reg, "", x), ...)
}
