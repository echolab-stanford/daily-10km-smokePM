library(census.tools)
library(RSelenium)


#rD = rsDriver(geckover="0.29.0", port=2121L, browser="chrome", 
#              chromever="88.0.4324.96", check=T)

rD = rsDriver(browser="firefox", port=3945L, verbose=F) #easier to do setup for firefox
remDr = rD[["client"]]
years = 1999:2021

for (i in 1:length(years)) {
  year = years[i]
  download_epaPM(remDr, years=year, 
                 path=paste0("~/BurkeLab Dropbox/Data/PM25/EPA/", 
                             year, "/"))
}
