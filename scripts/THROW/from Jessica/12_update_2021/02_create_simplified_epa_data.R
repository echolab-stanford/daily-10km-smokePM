rm(list = ls())
library(raster)
library(tidyverse)
library(sf)
library(rgdal)
library(data.table)

# Written by Sam Heft-Neal

####################################################################################
# read in the individual epa state-month-year files and combine them 
####################################################################################

yrs <- 2006:2019
states <- c("al", "ak", "az", "ar", "ca", "co", "ct", "de", "fl", "ga", "hi", "id", "il", "in", "ia", "ks", "ky", 
            "la", "me", "md", "ma", "mi", "mn", "ms", "mo", "mt", "ne", "nv", "nh", "nj", "nm", "ny", "nc", "nd", 
            "oh", "ok", "or", "pa", "ri", "sc", "sd", "tn", "tx", "ut", "vt", "va", "wa", "wv", "wi", "wy")
dat <- as.list(rep(NA, length(yrs)*length(states)))
k = 1


for (i in 1:length(yrs)){
    for (j in 1:length(states)) {
        dat[[k]] <- data.frame(read_csv(paste("data/pm25/epa/epa_data_daily_", states[j], "_", yrs[i], ".csv", sep = "")))
        k = k + 1
    }
}
dat <- data.frame(data.table::rbindlist(dat))

#add date variables
dat$month <- as.numeric(substr(dat$Date, 1, 2))
dat$day <- as.numeric(substr(dat$Date, 4, 5))
dat$year <- as.numeric(substr(dat$Date, 7, 10))

dat$month[dat$year <100]<-NA
dat$day[dat$year <100]<-NA
dat$year[dat$year <100]<-NA

dat$year[is.na(dat$year)]<- paste("20",unlist(lapply(strsplit(dat$Date[is.na(dat$year)],"/"),function(x){x[3]})) ,sep="")
dat$month[is.na(dat$month)]<- unlist(lapply(strsplit(dat$Date[is.na(dat$month)],"/"),function(x){x[1]}))
dat$day[is.na(dat$day)]<- unlist(lapply(strsplit(dat$Date[is.na(dat$day)],"/"),function(x){x[2]}))


datmth <- dat$month
datmth[nchar(datmth)==1]<-paste("0",datmth[nchar(datmth)==1], sep= "")
datdy <- dat$day
datdy[nchar(datdy)==1]<-paste("0",datdy[nchar(datdy)==1], sep= "")

#change the format of the date variable
dat$date <- paste(datmth, datdy, dat$year, sep = "")
names(dat)[names(dat)=="SITE_LATITUDE"]<-"lat"
names(dat)[names(dat)=="SITE_LONGITUDE"]<-"lon"
names(dat)[names(dat)=="Daily.Mean.PM2.5.Concentration"]<-"pm25"
dat <- dat %>%  dplyr::select(-Date, -UNITS, -DAILY_OBS_COUNT, -PERCENT_COMPLETE, -STATE_CODE, -STATE)

#rename and select variables
names(dat)[names(dat)=="AQS_SITE_ID"]<-"site_id"
names(dat)[names(dat)=="DAILY_AQI_VALUE"]<-"aqi"
names(dat)[names(dat)=="AQS_PARAMETER_CODE"]<-"aqs_code"
names(dat)[names(dat)=="AQS_PARAMETER_DESC"]<-"aqs_description"
names(dat)[names(dat)=="CBSA_CODE"]<-"cbsa_code"
names(dat)[names(dat)=="CBSA_NAME"]<-"cbsa_name"
names(dat)[names(dat)=="COUNTY_CODE"]<-"county_code"
names(dat)[names(dat)=="COUNTY"]<-"county"

dat <- dat %>% dplyr::select(date, year, month, day, date, lat, lon, county, county_code, 
                             cbsa_code, cbsa_name, pm25, aqi, aqs_code, aqs_description, cbsa_code, cbsa_name)

write_rds(dat, path = "data/clean/epa_station_level_pm25_data.rds")