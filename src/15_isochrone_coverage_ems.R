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

# Read in isochrones: EMS 8
ems_8 <- read_rds("./data/working/ems/final_ems_8.rds")
ems_8 <- ems_8 %>% filter(ems_8$geoid %in% rural$FIPS)

# Read in isochrones: EMS 10
ems_10 <- read_rds("./data/working/ems/final_ems_10.rds")
ems_10 <- ems_10 %>% filter(ems_10$geoid %in% rural$FIPS)

# Read in isochrones: EMS 12
ems_12 <- read_rds("./data/working/ems/final_ems_12.rds")
ems_12 <- ems_12 %>% filter(ems_12$geoid %in% rural$FIPS)

# Make sure that the projections are equal
compare(st_crs(properties), st_crs(ems_8))
compare(st_crs(properties), st_crs(ems_10))
compare(st_crs(properties), st_crs(ems_12))


#
# Select a test case -------------------------------------------------
#

# Filter
accomack_properties <- properties %>%
  filter(fips_code == "51001")

accomack_ems_10 <- ems_10 %>%
  filter(geoid == "51001")

# Check
plot(st_geometry(accomack_properties), pch = 19, cex = 0.2)
plot(st_geometry(accomack_ems_10[3, ]), add = T, col = "red", pch = 25, cex = 0.2)


#
# Test -------------------------------------------------
#

int <- st_intersection(accomack_properties, accomack_ems_10[1, ])
cov <- (nrow(int) / nrow(accomack_properties)) * 100

plot(st_geometry(accomack_properties), pch = 19, cex = 0.2)
plot(st_geometry(int), add = T, col = "red", pch = 25, cex = 0.2)


#
# PREP -------------------------------------------------
#

# 1 county has no ems stations. Filter that out so I don't have to deal with exceptions in the loop.
setdiff(properties$fips_code, ems_10$geoid)
setdiff(ems_10$geoid, properties$fips_code)
ems_fips <- intersect(ems_12$geoid, properties$fips_code)


#
# Scale: SINGLE -------------------------------------------------
#

# Loop through: EMS 8
for(i in ems_fips){
  
  propertydata <- properties %>% filter(fips_code == i)
  emsdata <- ems_8 %>% filter(geoid == i)
  
  for(j in 1:nrow(emsdata)){
    int <- st_intersection(propertydata, emsdata[j, ])
    cov <- (nrow(int) / nrow(propertydata)) * 100
    emsdata$ems_coverage_8[j] <- cov
    
    assign(paste0("ems_8_coverage_", i), emsdata)
  }
  
}

# Clean up
remove(cov)
remove(i)
remove(j)
remove(emsdata)
remove(int)
remove(propertydata)

# Loop through: EMS 10
for(i in ems_fips){
  
  propertydata <- properties %>% filter(fips_code == i)
  emsdata <- ems_10 %>% filter(geoid == i)
  
  for(j in 1:nrow(emsdata)){
    int <- st_intersection(propertydata, emsdata[j, ])
    cov <- (nrow(int) / nrow(propertydata)) * 100
    emsdata$ems_coverage_10[j] <- cov
    
    assign(paste0("ems_10_coverage_", i), emsdata)
  }
  
}

# Clean up
remove(cov)
remove(i)
remove(j)
remove(emsdata)
remove(int)
remove(propertydata)

# Loop through: EMS 12
for(i in ems_fips){
  
  propertydata <- properties %>% filter(fips_code == i)
  emsdata <- ems_12 %>% filter(geoid == i)
  
  for(j in 1:nrow(emsdata)){
    int <- st_intersection(propertydata, emsdata[j, ])
    cov <- (nrow(int) / nrow(propertydata)) * 100
    emsdata$ems_coverage_12[j] <- cov
    
    assign(paste0("ems_12_coverage_", i), emsdata)
  }
  
}

# Clean up
remove(cov)
remove(i)
remove(j)
remove(emsdata)
remove(int)
remove(propertydata)


#
# Save -------------------------------------------------
#

ems_8_coverage_final <- mget(ls(pattern = "^ems_8_coverage_")) %>% bind_rows()
ems_10_coverage_final <- mget(ls(pattern = "^ems_10_coverage_")) %>% bind_rows()
ems_12_coverage_final <- mget(ls(pattern = "^ems_12_coverage_")) %>% bind_rows()

write_rds(ems_8_coverage_final, "./data/working/ems/final_ems_8_coverage.rds")
write_rds(ems_10_coverage_final, "./data/working/ems/final_ems_10_coverage.rds")
write_rds(ems_12_coverage_final, "./data/working/ems/final_ems_12_coverage.rds")


#
# Scale: UNION -------------------------------------------------
#

# Loop through: EMS 8
for(i in ems_fips){
  
  propertydata <- properties %>% 
    filter(fips_code == i)
  
  emsdata <- ems_8 %>% 
    filter(geoid == i)
  
  whichgeoid <- emsdata$geoid[1]
  emsdata <- emsdata %>% summarize(geometry = st_union(geometry))
  emsdata$geoid <- whichgeoid
  
  int <- st_intersection(propertydata, emsdata)
  cov <- (nrow(int) / nrow(propertydata)) * 100
  
  emsdata$ems_countywide_coverage_8 <- cov
  
  assign(paste0("ems_8_countywide_coverage_", i), emsdata)
}

# Clean up
remove(cov)
remove(i)
remove(emsdata)
remove(int)
remove(propertydata)

# Loop through: EMS 10
for(i in ems_fips){
  
  propertydata <- properties %>% 
    filter(fips_code == i)
  
  emsdata <- ems_10 %>% 
    filter(geoid == i)
  
  whichgeoid <- emsdata$geoid[1]
  emsdata <- emsdata %>% summarize(geometry = st_union(geometry))
  emsdata$geoid <- whichgeoid
  
  int <- st_intersection(propertydata, emsdata)
  cov <- (nrow(int) / nrow(propertydata)) * 100
  
  emsdata$ems_countywide_coverage_10 <- cov
  
  assign(paste0("ems_10_countywide_coverage_", i), emsdata)
}

# Clean up
remove(cov)
remove(i)
remove(emsdata)
remove(int)
remove(propertydata)

# Loop through: EMS 12
for(i in ems_fips){
  
  propertydata <- properties %>% 
    filter(fips_code == i)
  
  emsdata <- ems_12 %>% 
    filter(geoid == i)
  
  whichgeoid <- emsdata$geoid[1]
  emsdata <- emsdata %>% summarize(geometry = st_union(geometry))
  emsdata$geoid <- whichgeoid
  
  int <- st_intersection(propertydata, emsdata)
  cov <- (nrow(int) / nrow(propertydata)) * 100
  
  emsdata$ems_countywide_coverage_12 <- cov
  
  assign(paste0("ems_12_countywide_coverage_", i), emsdata)
}

# Clean up
remove(cov)
remove(i)
remove(emsdata)
remove(int)
remove(propertydata)


#
# Save -------------------------------------------------
#

ems_8_countywide_coverage_final <- mget(ls(pattern = "^ems_8_countywide_coverage_")) %>% bind_rows()
ems_10_countywide_coverage_final <- mget(ls(pattern = "^ems_10_countywide_coverage_")) %>% bind_rows()
ems_12_countywide_coverage_final <- mget(ls(pattern = "^ems_12_countywide_coverage_")) %>% bind_rows()

write_rds(ems_8_countywide_coverage_final, "./data/working/ems/final_ems_8_countywide_coverage.rds")
write_rds(ems_10_countywide_coverage_final, "./data/working/ems/final_ems_10_countywide_coverage.rds")
write_rds(ems_12_countywide_coverage_final, "./data/working/ems/final_ems_12_countywide_coverage.rds")

