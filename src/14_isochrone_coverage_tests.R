library(sf)
library(tidyverse)
library(leaflet)
library(waldo)
library(purrr)


#
# Prepare -------------------------------------------------
#

# Select rural counties only (according to VDH)
rural <- read_csv("./data/original/srhp_rurality_2020/omb_srhp_rurality.csv", 
                  col_names = TRUE, col_types = list(col_character(), col_factor(), col_factor()))
rural <- rural %>% filter(RuralUrban == "R")

# Read in property data
properties <- read_rds("./data/working/corelogic/final_corelogic.rds")
properties <- properties %>% filter(properties$fips_code %in% rural$FIPS)
st_crs(properties)

# Read in isochrones: WIFI 10
wifi_10 <- read_rds("./data/working/wifi/final_wifi_10.rds")
wifi_10 <- wifi_10 %>% filter(wifi_10$GEOID %in% rural$FIPS)

# Read in isochrones: WIFI 15
wifi_15 <- read_rds("./data/working/wifi/final_wifi_15.rds")
wifi_15 <- wifi_15 %>% filter(wifi_15$GEOID %in% rural$FIPS)

# Make sure that the projections are equal
compare(st_crs(properties), st_crs(wifi_10))
compare(st_crs(properties), st_crs(wifi_15))


#
# Select a test case -------------------------------------------------
#

# Filter
accomack_properties <- properties %>%
  filter(fips_code == "51001")

accomack_wifi_10 <- wifi_10 %>%
  filter(GEOID == "51001")

# Check
plot(st_geometry(accomack_properties), pch = 19, cex = 0.2)
plot(st_geometry(accomack_wifi_10[3, ]), add = T, col = "red", pch = 25, cex = 0.2)


#
# Test -------------------------------------------------
#

int <- st_intersection(accomack_properties, accomack_wifi_10[1, ])
cov <- (nrow(int) / nrow(accomack_properties)) * 100

plot(st_geometry(accomack_properties), pch = 19, cex = 0.2)
plot(st_geometry(int), add = T, col = "red", pch = 25, cex = 0.2)



#
# Backup and clean later -------------------------------------------------
#

# Morgan
for(i in 1:nrow(wifi_10)){
  int <- st_intersection(properties, wifi_10[i, ])
  cov <- (nrow(int) / nrow(properties)) * 100
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
