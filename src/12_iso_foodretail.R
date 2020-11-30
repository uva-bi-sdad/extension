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
library(tidycensus)
library(stringr)


#
# Read in ----------------------------------------------------------------------------
#

# The nonmissing DF
# data <- read_rds("/home/tp2sk/Git/extension/data/working/foodretail/foodretail_nonmiss.rds")
# data_mis <- read_rds("/home/tp2sk/Git/extension/data/working/foodretail/foodretail_missing_coded.rds")
# 
# #st_crs(data) <- 4326
# #st_crs(data_mis) <- 4326
# #data <- st_transform(data, 4326)
# #data_mis <- st_transform(data_mis, 4326)
# 
# # Split across 5 days
# data1 <- data[1:1607, ]
# data2 <- data[1608:3215, ]
# data3 <- data[3216:4823, ]
# data4 <- data[4824:6431, ]
# data5 <- data[6432:8037, ]

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


#
# Join back to original data  --------------------------------------------------------------
#

# Read in and prepare: Isos
food_10_pt1 <- read_rds("./data/working/foodretail/foodretail_10_nonmiss_pt1.rds") %>% select(max, geometry) %>% rename(isominute = max)
food_10_pt2 <- read_rds("./data/working/foodretail/foodretail_10_nonmiss_pt2.rds") %>% select(max, geometry) %>% rename(isominute = max)
food_10_pt3 <- read_rds("./data/working/foodretail/foodretail_10_nonmiss_pt3.rds") %>% select(max, geometry) %>% rename(isominute = max)
food_10_pt4 <- read_rds("./data/working/foodretail/foodretail_10_nonmiss_pt4.rds") %>% select(max, geometry) %>% rename(isominute = max)
food_10_pt5 <- read_rds("./data/working/foodretail/foodretail_10_nonmiss_pt5.rds") %>% select(max, geometry) %>% rename(isominute = max)
food_10_mis <- read_rds("./data/working/foodretail/foodretail_10_mis.rds") %>% select(max, geometry) %>% rename(isominute = max)
  
food_15_pt1 <- read_rds("./data/working/foodretail/foodretail_15_nonmiss_pt1.rds") %>% select(max, geometry) %>% rename(isominute = max)
food_15_pt2 <- read_rds("./data/working/foodretail/foodretail_15_nonmiss_pt2.rds") %>% select(max, geometry) %>% rename(isominute = max)
food_15_pt3 <- read_rds("./data/working/foodretail/foodretail_15_nonmiss_pt3.rds") %>% select(max, geometry) %>% rename(isominute = max)
food_15_pt4 <- read_rds("./data/working/foodretail/foodretail_15_nonmiss_pt4.rds") %>% select(max, geometry) %>% rename(isominute = max)
food_15_pt5 <- read_rds("./data/working/foodretail/foodretail_15_nonmiss_pt5.rds") %>% select(max, geometry) %>% rename(isominute = max)
food_15_mis <- read_rds("./data/working/foodretail/foodretail_15_mis.rds") %>% select(max, geometry) %>% rename(isominute = max)

# Read in and prepare: original
data <- read_rds("/home/tp2sk/Git/extension/data/working/foodretail/foodretail_nonmiss.rds")
data_mis <- read_rds("/home/tp2sk/Git/extension/data/working/foodretail/foodretail_missing_coded.rds")

data <- data %>% st_drop_geometry()
data_mis <- data_mis %>% st_drop_geometry()

data1 <- data[1:1607, ]
data2 <- data[1608:3215, ]
data3 <- data[3216:4823, ]
data4 <- data[4824:6431, ]
data5 <- data[6432:8037, ]

# Join
data1_joined_10 <- cbind(data1, food_10_pt1)
data2_joined_10 <- cbind(data2, food_10_pt2)
data3_joined_10 <- cbind(data3, food_10_pt3)
data4_joined_10 <- cbind(data4, food_10_pt4)
data5_joined_10 <- cbind(data5, food_10_pt5)
datamis_joined_10 <- cbind(data_mis, food_10_mis)
datamis_joined_10$fulladdress <- NULL
datamis_joined_10$confidence <- NULL
data_10 <- rbind(data1_joined_10, data2_joined_10, data3_joined_10, data4_joined_10, data5_joined_10, datamis_joined_10)
data_10 <- st_as_sf(data_10)

data1_joined_15 <- cbind(data1, food_15_pt1)
data2_joined_15 <- cbind(data2, food_15_pt2)
data3_joined_15 <- cbind(data3, food_15_pt3)
data4_joined_15 <- cbind(data4, food_15_pt4)
data5_joined_15 <- cbind(data5, food_15_pt5)
datamis_joined_15 <- cbind(data_mis, food_15_mis)
datamis_joined_15$fulladdress <- NULL
datamis_joined_15$confidence <- NULL
data_15 <- rbind(data1_joined_15, data2_joined_15, data3_joined_15, data4_joined_15, data5_joined_15, datamis_joined_15)
data_15 <- st_as_sf(data_15)


#
# Write  --------------------------------------------------------------
#

write_rds(data_10, "/home/tp2sk/Git/extension/data/working/foodretail/final_foodretail_10.rds")
write_rds(data_15, "/home/tp2sk/Git/extension/data/working/foodretail/final_foodretail_15.rds")


#
# Add in GEOIDs  --------------------------------------------------------------
#

# Read in
data_10 <- read_rds("./data/working/foodretail/final_foodretail_10.rds")
data_15 <- read_rds("./data/working/foodretail/final_foodretail_15.rds")

# Prepare GEOIDs
countyfips <- get(data("fips_codes")) %>% filter(state == "VA")
countyfips$county <- str_replace_all(countyfips$county, " County", "")
countyfips$county <- str_replace_all(countyfips$county, "city", "City")

countyfips$fips <- paste0(countyfips$state_code, countyfips$county_code)
countyfips$state <- NULL
countyfips$state_name <- NULL

# These are all cities but they do not have "city" in the name in the original dataset. 
# Have to fix to get a match. Tricky! There are:
# Roanoke and Roanoke City, Richmond and Richmond City, Fairfax and Fairfax City, Franklin and Franklin City
# Assume that if it's not specified to be a city, it refers to the county -- won't work because 
# of entries like "Virginia Beach", which is only a city (no Virginia Beach County exists). 
# Assume the other way around but be careful about existing distinctions (Roanoke, Richmond, Fairfax, Franklin).

table(data_10$county)
setdiff(data_10$county, countyfips$county)
setdiff(countyfips$county, data_10$county)

needreplacement <- setdiff(data_10$county, countyfips$county)

for(i in needreplacement){
  data_10$county <- ifelse(data_10$county == i, paste0(i, " ", "City"), data_10$county)
  data_15$county <- ifelse(data_15$county == i, paste0(i, " ", "City"), data_15$county)
}

# Join
data_10 <- left_join(data_10, countyfips, by = "county") # any(is.na(data_10$county))
data_15 <- left_join(data_15, countyfips, by = "county") # any(is.na(data_15$county))

# Now that it's safe, add names to GEOIDs
countyfips <- get(data("fips_codes")) %>% filter(state == "VA")
countyfips$county <- str_replace_all(countyfips$county, "city", "City")
countyfips$fips <- paste0(countyfips$state_code, countyfips$county_code)

countyfips <- countyfips %>% select(-state, -state_name) %>% rename(countyname = county)

# Join
data_10 <- left_join(data_10, countyfips, by = "fips") 
data_10$county <- NULL

data_15 <- left_join(data_15, countyfips, by = "fips") %>% select(-county)
data_15$county <- NULL


#
# Write  --------------------------------------------------------------
#

write_rds(data_10, "/home/tp2sk/Git/extension/data/working/foodretail/final_foodretail_10.rds")
write_rds(data_15, "/home/tp2sk/Git/extension/data/working/foodretail/final_foodretail_15.rds")


#
# Test --------------------------------------------------------------
#

test_data <- read_rds("/home/tp2sk/Git/extension/data/working/foodretail/foodretail_nonmiss.rds")
plot(st_geometry(data_10[29, ]))
plot(st_geometry(test_data[29, ]), col = "red", add = TRUE)

test_data <- read_rds("/home/tp2sk/Git/extension/data/working/foodretail/foodretail_missing_coded.rds")
plot(st_geometry(data_10[8687, ]))
plot(st_geometry(test_data[650, ]), col = "red", add = TRUE)

