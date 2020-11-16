# install.packages('readr', repos = 'http://cran.us.r-project.org')
# install.packages('dplyr', repos = 'http://cran.us.r-project.org')
# install.packages('osrm', repos = 'http://cran.us.r-project.org')
# install.packages('sf', repos = 'http://cran.us.r-project.org')
# install.packages('purrr', repos = 'http://cran.us.r-project.org')
# install.packages('lwgeom', repos = 'http://cran.us.r-project.org')

library(readr)
library(dplyr)
library(osrm)
library(sf)
library(purrr)
library(lwgeom)


#
# Read in ----------------------------------------------------------------------------
#

# The nonmissing DF
#data <- read_rds("/home/tp2sk/Git/extension/data/working/foodretail/foodretail_nonmiss.rds")
data_mis <- read_rds("/home/tp2sk/Git/extension/data/working/foodretail/foodretail_missing_coded.rds")

#st_crs(data) <- 4326
st_crs(data_mis) <- 4326
#data <- st_transform(data, 4326)
data_mis <- st_transform(data_mis, 4326)

# Split across 5 days
#data1 <- data[1:1607, ]
#data2 <- data[1608:3215, ]
#data3 <- data[3216:4823, ]
#data4 <- data[4824:6431, ]
#data5 <- data[6432:8037, ]

# Clean up
#remove(data)


#
# OSRM backend instance for Virginia locations on the SDAD server  -------------------------------
#

options(osrm.server = "http://104.248.112.16:5000/", osrm.profile = "driving")


#
# Get isochrones  --------------------------------------------------------------
#

# 10 minute isochrones PART 1
# foodretail_10_pt1 <- map_dfr(c(1:nrow(data1)), ~osrmIsochrone(
#   loc = data1[.x, ],
#   breaks = 10,
#   res = 200,
#   returnclass = "sf"
# ))
# 
# write_rds(foodretail_10_pt1, "/home/tp2sk/Git/extension/data/working/foodretail/foodretail_10_nonmiss_pt1.rds")

# # 10 minute isochrones PART 2
# foodretail_10_pt2 <- map_dfr(c(1:nrow(data2)), ~osrmIsochrone(
#   loc = data2[.x, ],
#   breaks = 10,
#   res = 200,
#   returnclass = "sf"
# ))
# 
# write_rds(foodretail_10_pt2, "/home/tp2sk/Git/extension/data/working/foodretail/foodretail_10_nonmiss_pt2.rds")

# # 10 minute isochrones PART 3
# foodretail_10_pt3 <- map_dfr(c(1:nrow(data3)), ~osrmIsochrone(
#   loc = data3[.x, ],
#   breaks = 10,
#   res = 200,
#   returnclass = "sf"
# ))
# 
# write_rds(foodretail_10_pt3, "/home/tp2sk/Git/extension/data/working/foodretail/foodretail_10_nonmiss_pt3.rds")

# # 10 minute isochrones PART 4
# foodretail_10_pt4 <- map_dfr(c(1:nrow(data4)), ~osrmIsochrone(
#   loc = data4[.x, ],
#   breaks = 10,
#   res = 200,
#   returnclass = "sf"
# ))
# 
# write_rds(foodretail_10_pt4, "/home/tp2sk/Git/extension/data/working/foodretail/foodretail_10_nonmiss_pt4.rds")

# # 10 minute isochrones PART 5
# foodretail_10_pt5 <- map_dfr(c(1:nrow(data5)), ~osrmIsochrone(
#   loc = data5[.x, ],
#   breaks = 10,
#   res = 200,
#   returnclass = "sf"
# ))
# 
# write_rds(foodretail_10_pt5, "/home/tp2sk/Git/extension/data/working/foodretail/foodretail_10_nonmiss_pt5.rds")

# # # 10 minute isochrones MISS
# foodretail_10_mis <- map_dfr(c(1:nrow(data_mis)), ~osrmIsochrone(
#   loc = data_mis[.x, ],
#   breaks = 10,
#   res = 200,
#   returnclass = "sf"
# ))
# 
# write_rds(foodretail_10_mis, "/home/tp2sk/Git/extension/data/working/foodretail/foodretail_10_mis.rds")

# 15 minute isochrones PART 1
# foodretail_15_pt1 <- map_dfr(c(1:nrow(data1)), ~osrmIsochrone(
#   loc = data1[.x, ],
#   breaks = 10,
#   res = 200,
#   returnclass = "sf"
# ))
# 
# write_rds(foodretail_15_pt1, "/home/tp2sk/Git/extension/data/working/foodretail/foodretail_15_nonmiss_pt1.rds")

# # 15 minute isochrones PART 2
# foodretail_15_pt2 <- map_dfr(c(1:nrow(data2)), ~osrmIsochrone(
#   loc = data2[.x, ],
#   breaks = 10,
#   res = 200,
#   returnclass = "sf"
# ))
# 
# write_rds(foodretail_15_pt2, "/home/tp2sk/Git/extension/data/working/foodretail/foodretail_15_nonmiss_pt2.rds")

# # 15 minute isochrones PART 3
# foodretail_15_pt3 <- map_dfr(c(1:nrow(data3)), ~osrmIsochrone(
#   loc = data3[.x, ],
#   breaks = 10,
#   res = 200,
#   returnclass = "sf"
# ))
# 
# write_rds(foodretail_15_pt3, "/home/tp2sk/Git/extension/data/working/foodretail/foodretail_15_nonmiss_pt3.rds")

# 15 minute isochrones PART 4
# foodretail_15_pt4 <- map_dfr(c(1:nrow(data4)), ~osrmIsochrone(
#   loc = data4[.x, ],
#   breaks = 10,
#   res = 200,
#   returnclass = "sf"
# ))
# 
# write_rds(foodretail_15_pt4, "/home/tp2sk/Git/extension/data/working/foodretail/foodretail_15_nonmiss_pt4.rds")

# # # 15 minute isochrones PART 5
# foodretail_15_pt5 <- map_dfr(c(1:nrow(data5)), ~osrmIsochrone(
#   loc = data5[.x, ],
#   breaks = 10,
#   res = 200,
#   returnclass = "sf"
# ))
# 
# write_rds(foodretail_15_pt5, "/home/tp2sk/Git/extension/data/working/foodretail/foodretail_15_nonmiss_pt5.rds")

# # 15 minute isochrones MISS
# foodretail_15_mis <- map_dfr(c(1:nrow(data_mis)), ~osrmIsochrone(
#   loc = data_mis[.x, ],
#   breaks = 15,
#   res = 200,
#   returnclass = "sf"
# ))
# 
# write_rds(foodretail_15_mis, "/home/tp2sk/Git/extension/data/working/foodretail/foodretail_15_mis.rds")
