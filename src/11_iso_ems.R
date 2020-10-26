library(readr)
library(dplyr)
library(osrm)
library(sf)
library(purrr)


#
# Read in ----------------------------------------------------------------------------
#

data <- read_rds("/home/tp2sk/Git/extension/data/working/ems/final_ems.rds")


#
# OSRM backend instance for Virginia locations on the SDAD server  -------------------------------
#

options(osrm.server = "http://104.248.112.16:5000/", osrm.profile = "driving")


#
# Get isochrones  --------------------------------------------------------------
#

ems_8 <- map_dfr(c(1:nrow(data)), ~osrmIsochrone(
  loc = data[.x, ],
  breaks = 8,
  res = 200,
  returnclass = "sf"
))

write_rds(ems_8, "/home/tp2sk/Git/extension/data/working/ems/ems_8.rds")

ems_10 <- map_dfr(c(1:nrow(data)), ~osrmIsochrone(
  loc = data[.x, ],
  breaks = 10,
  res = 200,
  returnclass = "sf"
))

write_rds(ems_10, "/home/tp2sk/Git/extension/data/working/ems/ems_10.rds")

ems_12 <- map_dfr(c(1:nrow(data)), ~osrmIsochrone(
  loc = data[.x, ],
  breaks = 12,
  res = 200,
  returnclass = "sf"
))

write_rds(ems_12, "/home/tp2sk/Git/extension/data/working/ems/ems_12.rds")