path_seagate = "/Volumes/Seagate PD JBL/smoke_PM_prediction/"

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