library(RCurl)
library(R.utils)
library(XML)
library(rgdal)
library(tools)

# function to deal with the file structure when unzipping
move_file = function(path) {
  name = str_split(path, "/")[[1]]
  name = name[length(name)]
  new_name = gsub("hms_pts.", "hms_fire", name)
  file.copy(path, ".")
  file.rename(name, new_name)
}

setwd("~/BurkeLab Dropbox/Data/fire")

url_base <- "https://satepsanone.nesdis.noaa.gov/pub/volcano/FIRE/HMS_ARCHIVE/"
yrs <- 2006:2020

# loop through years
for (y in 1:length(yrs)) {
  
  # load the page of links
  url_info <- getURL(paste(url_base, yrs[y], "/GIS/FIRE/", sep = ""))
  
  url <- htmlTreeParse(url_info)
  url <- url[[1]]
  url <- url[[2]]
  url <- url[[2]]
  url <- url[4:length(url)]
  
  files <- rep(NA, length(url))
  
  # fix the file names to download
  for (i in 1:length(files)) {
    files[i] <- as.character(url[i]$tr[2]$td[1]$a)[2]
  }
  
  # only take the ones that are zip files
  files = files[grepl(".zip", files)]
  
  # download them
  for (i in 1:length(files)) {
    try(download.file(
      url = paste(url_base, yrs[y], "/GIS/FIRE/", files[i], sep = ""),
      destfile = files[i]
    ))
  }
}

# get list of files that are downloaded and still zipped
files = list.files()
files = files[grepl(".zip", files)]

# go through them to unzip
prog = txtProgressBar(min=0, max=length(files), initial=0, char="-", style=3)
for (i in 1:length(files)) {
  
  # if it's a gz file use untar
  if(grepl(".gz", files[i])) {
    files_to_move = untar(files[i], list=T)
    folder = str_split(files_to_move[[1]], "/")[[1]][1]
    untar(files[i])
    
    move_file(files_to_move)
    unlink(folder, recursive=T)
    file.remove(files[i])
  } else {
    # much simpler + faster method if normal zip
    utils::unzip(zipfile = files[i], overwrite = T, junkpaths = T)
    file.remove(files[i])
  }
  
  if (i %% floor(length(files)/100) == 0) {setTxtProgressBar(prog, i)}
}


# go through and fix any naming convention issues
files = list.files()
files = files[grepl("pts.", files)]

for (i in  1: length(files)) {
  new_name = gsub("hms_pts.", "hms_fire", files[i])
  file.rename(files[i], new_name)
}


# download dates that didn't get downloaded
all_dates = seq.Date(as.Date(paste0(min(yrs), "-01-01")), 
                     as.Date(paste0(max(yrs), "-12-31")), by = "day")
have_dates = as.Date(gsub("hms_fire", "", file_path_sans_ext(list.files(pattern = "hms_fire"))),
                     format = "%Y%m%d")
missing_dates = setdiff(all_dates, have_dates)
class(missing_dates) = "Date"
missing_dates = gsub("-", "", missing_dates)


# loop through years
not_online <- c()
for (y in 1:length(yrs)) {
  
  # load the page of links
  url_info <- getURL(paste(url_base, yrs[y], "/GIS/FIRE/", sep = ""))
  
  url <- htmlTreeParse(url_info)
  url <- url[[1]]
  url <- url[[2]]
  url <- url[[2]]
  url <- url[4:length(url)]
  
  files <- rep(NA, length(url))
  
  # fix the file names to download
  for (i in 1:length(files)) {
    files[i] <- as.character(url[i]$tr[2]$td[1]$a)[2]
  }
  
  # only take the ones that are currently not downloaded
  missing_dates_y = grep(yrs[y], missing_dates, value = T)
  print(missing_dates_y)
  if (length(missing_dates_y) > 0) {
    files = files[grepl(paste(missing_dates_y, collapse = "|"), files)]
    files = files[!grepl(".zip", files)] # avoid zip files this time
    print(files)
    
    # download them
    if (length(files) > 0) {
      for (i in 1:length(files)) {
        try(download.file(
          url = paste(url_base, yrs[y], "/GIS/FIRE/", files[i], sep = ""),
          destfile = files[i]
        ))
        
        # unzip
        if (grepl("\\.gz$", files[i])) try(gunzip(files[i]))
      }
    }
    not_online_y <- setdiff(missing_dates_y, unique(file_path_sans_ext(gsub("hms_fire|.gz", "", files))))
    not_online <- c(not_online, not_online_y)
  }
}
print(paste("Dates not available online:", length(not_online)))
for (i in not_online) print(i)
