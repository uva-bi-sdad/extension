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
properties <- read_rds("./data/working/corelogic/final_corelogic_forprocessing.rds")
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
# Union -------------------------------------------------
#

# Test
accomack_union <- st_union(accomack_wifi_10)
plot(st_geometry(accomack_wifi_10))
     
plot(st_geometry(accomack_properties), pch = 19, cex = 0.2)
plot(st_geometry(accomack_wifi_10), add = T, col = "red")

test_wifi <- wifi_10 %>%
  group_by(GEOID) %>% 
  summarize(geometry = st_union(geometry)) %>%
  ungroup()
plot(st_geometry(test_wifi))

test_property <- properties %>%
  group_by(fips_code) %>% 
  summarize(geometry = st_union(geometry)) %>%
  ungroup()
plot(st_geometry(test_property))

wifi_fips <- intersect(wifi_10$GEOID, properties$fips_code)

# Loop through: Wifi 10
for(i in wifi_fips[1:3]){
  
  propertydata <- properties %>% 
    filter(fips_code == i)
  
  wifidata <- wifi_10 %>% 
    filter(GEOID == i)
  
  whichgeoid <- wifidata$GEOID[1]
  wifidata <- wifidata %>% summarize(geometry = st_union(geometry))
  wifidata$GEOID <- whichgeoid
  
  int <- st_intersection(propertydata, wifidata)
  cov <- (nrow(int) / nrow(propertydata)) * 100
  
  wifidata$wifi_countywide_coverage_10 <- cov
    
  assign(paste0("wifi_10_countywide_coverage_", i), wifidata)
}
