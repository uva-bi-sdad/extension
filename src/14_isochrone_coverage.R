library(sf)
library(tidyverse)
library(leaflet)

#
# Coverage -------------------------------------------------
#

ems_points <- readRDS("./data/working/ems/final_ems.rds")

rural <- read.csv("./data/original/srhp_rurality_2020/omb_srhp_rurality.csv")
rural <- rural %>%
  filter(RuralUrban == "R")
rural$FIPS <- as.character(rural$FIPS)

properties <- readRDS("./data/working/corelogic/final_corelogic.rds")
st_crs(properties) <- 4326
properties <- st_transform(properties, 4326)
properties <- properties %>%
  filter(fips_code == "51001")

ems_8 <- readRDS("./data/working/ems/final_ems_8.rds")
st_crs(ems_8) <- 4326
ems_8 <- st_transform(ems_8, 4326)
ems_8 <- inner_join(ems_8, rural, by = c("geoid" = "FIPS"))
ems_8 <- ems_8 %>%
  filter(county == "Accomack")

plot(st_geometry(properties))
plot(st_geometry(ems_8_test), add = T, col = "red")

st_is_valid(ems_8)
ems_8 <- st_make_valid(ems_8)

# Warning message:
#   In st_is_longlat(x) :
#   bounding box has potentially an invalid value range for longlat data
#   The values of range seem inaccurate, and are certainly different than the range of accomack county

for(i in 1:nrow(ems_8)){
  int <- st_intersection(properties, ems_8[i])
  cov <- (nrow(int)/nrow(properties))*100
  ems_8$coverage_8[i] <- cov
}