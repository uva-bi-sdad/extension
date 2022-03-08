library(readr, repos = 'http://cran.us.r-project.org')
library(dplyr, repos = 'http://cran.us.r-project.org')
library(osrm, repos = 'http://cran.us.r-project.org')
library(sf, repos = 'http://cran.us.r-project.org')
library(purrr, repos = 'http://cran.us.r-project.org')


#
# Read in ----------------------------------------------------------------------------
#

data <- read_rds("/home/tp2sk/Git/extension/data/working/wifi/final_wifi.rds")


#
# OSRM backend instance for Virginia locations on the SDAD server  -------------------------------
#

options(osrm.server = Sys.getenv("OSRM_SERVER"), osrm.profile = "driving")


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


#
# Join back to original data  --------------------------------------------------------------
#

wifi_10 <- read_rds("./data/working/wifi/wifi_10.rds") %>% select(max, geometry) %>% rename(isominute = max)
wifi_15 <- read_rds("./data/working/wifi/wifi_15.rds") %>% select(max, geometry) %>% rename(isominute = max)

data <- data %>% st_drop_geometry()

data_10 <- cbind(data, wifi_10)
data_10 <- st_as_sf(data_10)

data_15 <- cbind(data, wifi_15)
data_15 <- st_as_sf(data_15)


#
# Write  --------------------------------------------------------------
#

write_rds(data_10, "/home/tp2sk/Git/extension/data/working/wifi/final_wifi_10.rds")
write_rds(data_15, "/home/tp2sk/Git/extension/data/working/wifi/final_wifi_15.rds")


#
# Test --------------------------------------------------------------
#

data <- read_rds("/home/tp2sk/Git/extension/data/working/wifi/final_wifi.rds")
plot(st_geometry(data_10[29, ]))
plot(st_geometry(data[29, ]), col = "red", add = TRUE)
