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

# ems_8 <- map_dfr(c(1:nrow(data)), ~osrmIsochrone(
#   loc = data[.x, ],
#   breaks = 8,
#   res = 200,
#   returnclass = "sf"
# ))

# write_rds(ems_8, "/home/tp2sk/Git/extension/data/working/ems/ems_8.rds")

# ems_10 <- map_dfr(c(1:nrow(data)), ~osrmIsochrone(
#   loc = data[.x, ],
#   breaks = 10,
#   res = 200,
#   returnclass = "sf"
# ))
# 
# write_rds(ems_10, "/home/tp2sk/Git/extension/data/working/ems/ems_10.rds")

# ems_12 <- map_dfr(c(1:nrow(data)), ~osrmIsochrone(
#   loc = data[.x, ],
#   breaks = 12,
#   res = 200,
#   returnclass = "sf"
# ))
# 
# write_rds(ems_12, "/home/tp2sk/Git/extension/data/working/ems/ems_12.rds")


#
# Join back to original data  --------------------------------------------------------------
#

ems_8 <- read_rds("./data/working/ems/ems_8.rds") %>% select(max, geometry) %>% rename(isominute = max)
ems_10 <- read_rds("./data/working/ems/ems_10.rds") %>% select(max, geometry) %>% rename(isominute = max)
ems_12 <- read_rds("./data/working/ems/ems_12.rds") %>% select(max, geometry) %>% rename(isominute = max)

data <- data %>% st_drop_geometry()

data_8 <- cbind(data, ems_8)
data_8 <- st_as_sf(data_8)

data_10 <- cbind(data, ems_10)
data_10 <- st_as_sf(data_10)

data_12 <- cbind(data, ems_12)
data_12 <- st_as_sf(data_12)

st_crs(data_8) <- 4326
data_8 <- st_transform(data_8, 4326)

st_crs(data_10) <- 4326
data_10 <- st_transform(data_10, 4326)

st_crs(data_12) <- 4326
data_12 <- st_transform(data_12, 4326)

#
# Write  --------------------------------------------------------------
#

write_rds(data_8, "/home/tp2sk/Git/extension/data/working/ems/final_ems_8.rds")
write_rds(data_10, "/home/tp2sk/Git/extension/data/working/ems/final_ems_10.rds")
write_rds(data_12, "/home/tp2sk/Git/extension/data/working/ems/final_ems_12.rds")


#
# Test --------------------------------------------------------------
#

data <- read_rds("/home/tp2sk/Git/extension/data/working/ems/final_ems.rds")
plot(st_geometry(data_10[29, ]))
plot(st_geometry(data[29, ]), col = "red", add = TRUE)

