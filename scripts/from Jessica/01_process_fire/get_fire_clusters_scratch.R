f_coords = fire %>% lapply("[", c("Lon", "Lat"))
f_west = sapply(f_coords, function(x) any(x$Lon < west))

f = lapply(fire, "[", c("Lon", "Lat"))
# 20140705 is empty file
# 20080527 half the points are empty points
f = f[which(!(names(f) %in% c("20140705", "20080527")))]
f_nrow = sapply(f, nrow)
f_west = sapply(f, function(x) sum(x$Lon < west))
f_east = sapply(f, function(x) sum(x$Lon > east))
f_south = sapply(f, function(x) sum(x$Lat < south))
f_north = sapply(f, function(x) sum(x$Lat > north))
sum(f_west)
sum(f_east)
sum(f_south)
sum(f_north)

any(f_east == f_nrow)
any(f_south == f_nrow)
which(f_east == f_nrow)
which(f_south == f_nrow)

summary(f_east)
summary(f_south)

which(f_east == max(f_east))
which(f_south == max(f_south))

x = f[["20110209"]]
# x[which(x$Lon > east),]
x[which(x$Lat < south),]
























































library(dplyr)

north = 72 + 20
south = 14.6 - 20
east = -50 + 40
west = -170 - 40

fire = readRDS("~/BurkeLab Dropbox/Data/fire/hms_fires.RDS")
f = lapply(fire, "[", c("Lon", "Lat"))
f = f[which(!(names(f) %in% c("20140705", "20080527")))]
f_nrow = sapply(f, nrow)
f_west = sapply(f, function(x) sum(x$Lon < west))
f_east = sapply(f, function(x) sum(x$Lon > east))
f_south = sapply(f, function(x) sum(x$Lat < south))
f_north = sapply(f, function(x) sum(x$Lat > north))
sum(f_west)
sum(f_east)
sum(f_south)
sum(f_north)

any(f_east == f_nrow)
any(f_south == f_nrow)
which(f_east == f_nrow)
which(f_south == f_nrow)

summary(f_east)
summary(f_south)

which(f_west > 0)
which(f_east > 0)
which(f_south > 0)
which(f_north > 0)

plot(f[names(which(f_west > 0))][[1]])
plot(f[names(which(f_east > 0))][[1]])
plot(f[names(which(f_east > 0))][[2]])
plot(f[names(which(f_east > 0))][[3]])
plot(f[names(which(f_south > 0))][[1]])
plot(f[names(which(f_south > 0))][[2]])
plot(f[names(which(f_south > 0))][[3]])
plot(f[names(which(f_south > 0))][[4]])
plot(f[names(which(f_north > 0))][[1]])

