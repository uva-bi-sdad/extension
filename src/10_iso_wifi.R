library(readr)
library(dplyr)
library(osrm)
library(sf)
library(cartography)
library(purrr)


#
# Read in ----------------------------------------------------------------------------
#


data <- read_rds("./data/working/wifi/final_wifi.rds")


#
# OSRM backend instance for Virginia locations on the SDAD server  -------------------------------
#

options(osrm.server = "http://104.248.112.16:5000/", osrm.profile = "driving")


#
# Test resolution ----------------------------------------------------------------------------
#

Rprof()
test1 <- osrmIsochrone(
  loc = data[1, ],
  breaks = 10,
  res = 30,
  returnclass = "sf")
Rprof(NULL)
summaryRprof()

Rprof()
test2 <- osrmIsochrone(
  loc = data[1, ],
  breaks = 10,
  res = 300,
  returnclass = "sf")
Rprof(NULL)
summaryRprof()

plot(st_geometry(data[1, ]))
plot(st_geometry(test1), add = T)
plot(st_geometry(test2), add = T)

remove(test1)
remove(test2)


#
# Get isochrones  --------------------------------------------------------------
#

wifi_10 <- map_dfr(c(1:nrow(data)), ~osrmIsochrone(
  loc = data[.x, ],
  breaks = 10,
  res = 300,
  returnclass = "sf"
))

wifi_15 <- map_dfr(c(1:nrow(data)), ~osrmIsochrone(
  loc = data[.x, ],
  breaks = 15,
  res = 300,
  returnclass = "sf"
))


#
# Write  --------------------------------------------------------------
#

write_rds(wifi_10, "./data/working/wifi/final_wifi_10.rds")
write_rds(wifi_15, "./data/working/wifi/final_wifi_15.rds")

