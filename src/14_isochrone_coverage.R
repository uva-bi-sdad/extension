library(sf)
library(tidyverse)
library(leaflet)

#
# Coverage -------------------------------------------------
#
rural <- read.csv("./data/original/srhp_rurality_2020/omb_srhp_rurality.csv")
rural <- rural %>%
  filter(RuralUrban == "R")
rural$FIPS <- as.character(rural$FIPS)

properties <- readRDS("./data/working/corelogic/final_corelogic.rds")
st_crs(properties) <- 4326
properties <- st_transform(properties, 4326)
properties <- properties %>%
  filter(fips_code == "51001")

wifi_10 <- readRDS("./data/working/wifi/final_wifi_10.rds")
st_crs(wifi_10) <- 4326
wifi_10 <- st_transform(wifi_10, 4326)
wifi_10 <- inner_join(wifi_10, rural, by = c("GEOID" = "FIPS"))
wifi_10 <- wifi_10 %>%
  filter(GEOID == 51001)

plot(st_geometry(properties))
plot(st_geometry(wifi_10[3,]), add = T, col = "red")

st_is_valid(wifi_10)
wifi_10 <- st_make_valid(wifi_10)

for(i in 1:nrow(wifi_10)){
  int <- st_intersection(properties, wifi_10[i,])
  cov <- (nrow(int)/nrow(properties))*100
  wifi_10$coverage_10[i] <- cov
}

wifi_10_union <- st_union(wifi_10[1,],wifi_10[2,])
for(i in 3:nrow(wifi_10)){
wifi_10_union <- st_union(wifi_10_union,wifi_10[i,])
}
plot(st_geometry(wifi_10_union))

# this uses to union from the loop - this is what we did for patrick
# so I feel that maybe this is a better estimate of coverage
wifi_10_union_int_a <- st_intersection(wifi_10_union, properties)
wifi_10_cov_a <- (nrow(wifi_10_union_int_a)/nrow(properties))*100

# this uses the data frame without st_union - 72.389 percent covered, which makes no sense given
# how small the individual coverage estimates are
# wifi_10_union_int_b <- st_intersection(wifi_10, properties)
# wifi_10_cov_b <- (nrow(wifi_10_union_int_b)/nrow(properties))*100

# to make this loop work through more than 1 county we need to add a bigger loop that
# subsets for a rural county based on FIPS in rural and GEOID in wifi_10
# and then subsets properties with fips_code and then runs the two separate loops and saves them somewhere
