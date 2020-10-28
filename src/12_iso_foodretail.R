library(readr)
library(dplyr)
library(osrm)
library(sf)
library(purrr)


#
# Read in ----------------------------------------------------------------------------
#

# The nonmissing DF
data <- read_rds("/home/tp2sk/Git/extension/data/working/foodretail/foodretail_nonmiss.rds")

# Split across 4 days
data1 <- data[1:2009, ]
data2 <- data[2010:4019, ]
data3 <- data[4020:6029, ]
data4 <- data[6030:8037, ]

# Clean up
remove(data)


#
# OSRM backend instance for Virginia locations on the SDAD server  -------------------------------
#

options(osrm.server = "http://104.248.112.16:5000/", osrm.profile = "driving")


#
# Get isochrones  --------------------------------------------------------------
#

# 10 minute isochrones PART 1
foodretail_10_pt1 <- map_dfr(c(1:nrow(data1)), ~osrmIsochrone(
  loc = data1[.x, ],
  breaks = 10,
  res = 200,
  returnclass = "sf"
))

write_rds(foodretail_10_pt1, "/home/tp2sk/Git/extension/data/working/foodretail/foodretail_10_nonmiss_pt1.rds")

# 10 minute isochrones PART 2
foodretail_10_pt2 <- map_dfr(c(1:nrow(data2)), ~osrmIsochrone(
  loc = data2[.x, ],
  breaks = 10,
  res = 200,
  returnclass = "sf"
))

write_rds(foodretail_10_pt2, "/home/tp2sk/Git/extension/data/working/foodretail/foodretail_10_nonmiss_pt2.rds")

# 10 minute isochrones PART 3
foodretail_10_pt3 <- map_dfr(c(1:nrow(data3)), ~osrmIsochrone(
  loc = data3[.x, ],
  breaks = 10,
  res = 200,
  returnclass = "sf"
))

write_rds(foodretail_10_pt3, "/home/tp2sk/Git/extension/data/working/foodretail/foodretail_10_nonmiss_pt3.rds")

# 10 minute isochrones PART 4
foodretail_10_pt4 <- map_dfr(c(1:nrow(data4)), ~osrmIsochrone(
  loc = data4[.x, ],
  breaks = 10,
  res = 200,
  returnclass = "sf"
))

write_rds(foodretail_10_pt4, "/home/tp2sk/Git/extension/data/working/foodretail/foodretail_10_nonmiss_pt4.rds")

# 15 minute isochrones PART 1
foodretail_15_pt1 <- map_dfr(c(1:nrow(data1)), ~osrmIsochrone(
  loc = data1[.x, ],
  breaks = 10,
  res = 200,
  returnclass = "sf"
))

write_rds(foodretail_15_pt1, "/home/tp2sk/Git/extension/data/working/foodretail/foodretail_15_nonmiss_pt1.rds")

# 15 minute isochrones PART 2
foodretail_15_pt2 <- map_dfr(c(1:nrow(data2)), ~osrmIsochrone(
  loc = data2[.x, ],
  breaks = 10,
  res = 200,
  returnclass = "sf"
))

write_rds(foodretail_15_pt2, "/home/tp2sk/Git/extension/data/working/foodretail/foodretail_15_nonmiss_pt2.rds")

# 15 minute isochrones PART 3
foodretail_15_pt3 <- map_dfr(c(1:nrow(data3)), ~osrmIsochrone(
  loc = data3[.x, ],
  breaks = 10,
  res = 200,
  returnclass = "sf"
))

write_rds(foodretail_15_pt3, "/home/tp2sk/Git/extension/data/working/foodretail/foodretail_15_nonmiss_pt3.rds")

# 15 minute isochrones PART 4
foodretail_15_pt4 <- map_dfr(c(1:nrow(data4)), ~osrmIsochrone(
  loc = data4[.x, ],
  breaks = 10,
  res = 200,
  returnclass = "sf"
))

write_rds(foodretail_15_pt4, "/home/tp2sk/Git/extension/data/working/foodretail/foodretail_15_nonmiss_pt4.rds")
