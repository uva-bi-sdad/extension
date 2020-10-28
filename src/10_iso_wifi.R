library(readr)
library(dplyr)
library(osrm)
library(sf)
library(purrr)


#
# Read in ----------------------------------------------------------------------------
#

data <- read_rds("/home/tp2sk/Git/extension/data/working/wifi/final_wifi.rds")


#
# OSRM backend instance for Virginia locations on the SDAD server  -------------------------------
#

options(osrm.server = "http://104.248.112.16:5000/", osrm.profile = "driving")


#
# Test resolution ----------------------------------------------------------------------------
#

# Rprof()
# test1 <- osrmIsochrone(
#   loc = data[1, ],
#   breaks = 10,
#   res = 30,
#   returnclass = "sf")
# Rprof(NULL)
# summaryRprof()
# 
# Rprof()
# test2 <- osrmIsochrone(
#   loc = data[1, ],
#   breaks = 10,
#   res = 100,
#   returnclass = "sf")
# Rprof(NULL)
# summaryRprof()
# 
# Rprof()
# test3 <- osrmIsochrone(
#   loc = data[1, ],
#   breaks = 10,
#   res = 200,
#   returnclass = "sf")
# Rprof(NULL)
# summaryRprof()
# 
# Rprof()
# test4 <- osrmIsochrone(
#   loc = data[1, ],
#   breaks = 10,
#   res = 250,
#   returnclass = "sf")
# Rprof(NULL)
# summaryRprof()
# 
# Rprof()
# test5 <- osrmIsochrone(
#   loc = data[1, ],
#   breaks = 10,
#   res = 300,
#   returnclass = "sf")
# Rprof(NULL)
# summaryRprof()
# 
# plot(st_geometry(test1), type = "l", col = "yellow")
# plot(st_geometry(test2), add = T, type = "b", col = "orange")
# plot(st_geometry(test3), add = T, type = "b", col = "red")
# plot(st_geometry(test4), add = T, type = "b", col = "pink")
# plot(st_geometry(test5), add = T, type = "b", col = "purple")


#
# Get isochrones  --------------------------------------------------------------
#

# wifi_10 <- map_dfr(c(1:nrow(data)), ~osrmIsochrone(
#   loc = data[.x, ],
#   breaks = 10,
#   res = 200,
#   returnclass = "sf"
# ))
# 
# write_rds(wifi_10, "./data/working/wifi/wifi_10.rds")

# wifi_15 <- map_dfr(c(1:nrow(data)), ~osrmIsochrone(
#   loc = data[.x, ],
#   breaks = 15,
#   res = 200,
#   returnclass = "sf"
# ))
# 
# write_rds(wifi_15, "/home/tp2sk/Git/extension/data/working/wifi/wifi_15.rds")

