#-------------------------------------------------------------------------------
# Utilities
# Modified by Jessica from Sam and Anne's code
# Last edited June 2021
#-------------------------------------------------------------------------------
# Define paths
if (Sys.info()["user"] == "jessssli") {
  path_dropbox <- "~/BurkeLab Dropbox/Data/"
  path_github <- "~/Documents/GitHub/purple-air-infiltration/"
  path_results <- paste0(path_github, "work/get_HYSPLIT_height/results/")
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

# in progress
function(df) {
  return(which(is.na(df), arr.ind=TRUE))
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

# Results extractors
get_b_r2 <- function(mdls) {
  library(dplyr)
  library(purrr)
  return(mdls %>%
           lapply(summary) %>%
           lapply("[", c("coefficients", "r.squared")) %>%
           map_dfr(data.frame) %>%
           rename(b = coefficients.Estimate,
                  r2 = r.squared) %>%
           select(b, r2))
}

# IN PROGRESS breaking into helpers
get_summaries <- function(mdls) {
  return(lapply(mdls, summary))
}

get_b <- function(mdls) {
  return(lapply(get_summaries(mdls), "[", "coefficients"))
}

get_coeffs <- function(mdls) {
  return(map_dfr(get_summaries(mdls), "[", "coefficients"))
}

# get_r2 <- function(mdls) {
#   return(1)
# }
