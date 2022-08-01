source("work/08_figures/00_utils.R")

library(R.utils)

#-------------------------------------------------------------------------------
# Download JPG for Chosen Date
# Written by Jessica
# Last edited January 2022
#-------------------------------------------------------------------------------
jpg_url = paste0("https://satepsanone.nesdis.noaa.gov/pub/FIRE/HMS/hms_backup/", substr(chosen_date, 1, 4), "/JPEG/hms", chosen_date, ".jpg")
jpg.gz_url = paste0(jpg_url, ".gz")

error = try({
  filename = paste0(path_figures, basename(jpg_url))
  download.file(jpg_url, filename)
})
if (class(error) == "try-error") {
  filename = paste0(path_figures, basename(jpg.gz_url))
  download.file(jpg.gz_url, filename)
  gunzip(filename, overwrite = T)
}
