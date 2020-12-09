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
# PREP -------------------------------------------------
#

# 5 counties have no wifi hotspots. Filter that out so I don't have to deal with exceptions in the loop.
setdiff(properties$fips_code, wifi_10$GEOID)
setdiff(wifi_10$GEOID, properties$fips_code)
wifi_fips <- intersect(wifi_15$GEOID, properties$fips_code)


#
# Scale: SINGLE -------------------------------------------------
#

# Loop through: Wifi 10
for(i in wifi_fips){
  
  propertydata <- properties %>% filter(fips_code == i)
  wifidata <- wifi_10 %>% filter(GEOID == i)
  
  for(j in 1:nrow(wifidata)){
    int <- st_intersection(propertydata, wifidata[j, ])
    cov <- (nrow(int) / nrow(propertydata)) * 100
    wifidata$wifi_coverage_10[j] <- cov
    
    assign(paste0("wifi_10_coverage_", i), wifidata)
  }
  
}

# Clean up
remove(cov)
remove(i)
remove(j)
remove(wifidata)
remove(int)
remove(propertydata)

# Loop through: Wifi 15
for(i in wifi_fips){
  
  propertydata <- properties %>% filter(fips_code == i)
  wifidata <- wifi_15 %>% filter(GEOID == i)
  
  for(j in 1:nrow(wifidata)){
    int <- st_intersection(propertydata, wifidata[j, ])
    cov <- (nrow(int) / nrow(propertydata)) * 100
    wifidata$wifi_coverage_15[j] <- cov
    
    assign(paste0("wifi_15_coverage_", i), wifidata)
  }
  
}

# Clean up
remove(cov)
remove(i)
remove(j)
remove(wifidata)
remove(int)
remove(propertydata)


#
# Save -------------------------------------------------
#

wifi_10_coverage_final <- mget(ls(pattern = "^wifi_10_coverage_")) %>% bind_rows()
wifi_15_coverage_final <- mget(ls(pattern = "^wifi_15_coverage_")) %>% bind_rows()

write_rds(wifi_10_coverage_final, "./data/working/wifi/final_wifi_10_coverage.rds")
write_rds(wifi_15_coverage_final, "./data/working/wifi/final_wifi_15_coverage.rds")


#
# Scale: UNION -------------------------------------------------
#

# Loop through: Wifi 10
for(i in wifi_fips){
  
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

# Clean up
remove(cov)
remove(i)
remove(wifidata)
remove(int)
remove(propertydata)

# Loop through: Wifi 15
for(i in wifi_fips){
  
  propertydata <- properties %>% 
    filter(fips_code == i)
  
  wifidata <- wifi_15 %>% 
    filter(GEOID == i)
  
  whichgeoid <- wifidata$GEOID[1]
  wifidata <- wifidata %>% summarize(geometry = st_union(geometry))
  wifidata$GEOID <- whichgeoid
  
  int <- st_intersection(propertydata, wifidata)
  cov <- (nrow(int) / nrow(propertydata)) * 100
  
  wifidata$wifi_countywide_coverage_15 <- cov
  
  assign(paste0("wifi_15_countywide_coverage_", i), wifidata)
}

# Clean up
remove(cov)
remove(i)
remove(wifidata)
remove(int)
remove(propertydata)


#
# Save -------------------------------------------------
#

wifi_10_countywide_coverage_final <- mget(ls(pattern = "^wifi_10_countywide_coverage_")) %>% bind_rows()
wifi_15_countywide_coverage_final <- mget(ls(pattern = "^wifi_15_countywide_coverage_")) %>% bind_rows()

write_rds(wifi_10_countywide_coverage_final, "./data/working/wifi/final_wifi_10_countywide_coverage.rds")
write_rds(wifi_15_countywide_coverage_final, "./data/working/wifi/final_wifi_15_countywide_coverage.rds")

