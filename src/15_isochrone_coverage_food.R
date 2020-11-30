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

# Read in isochrones: Retail 10
food_10 <- read_rds("./data/working/foodretail/final_foodretail_10.rds")
food_10 <- food_10 %>% filter(food_10$fips %in% rural$FIPS)

# Read in isochrones: Retail 15
food_15 <- read_rds("./data/working/foodretail/final_foodretail_15.rds")
food_15 <- food_15 %>% filter(food_15$fips %in% rural$FIPS)

# Make sure that the projections are equal
compare(st_crs(properties), st_crs(food_10))
compare(st_crs(properties), st_crs(food_15))


#
# Select a test case -------------------------------------------------
#

# Filter
accomack_properties <- properties %>%
  filter(fips_code == "51001")

accomack_food_10 <- food_10 %>%
  filter(fips == "51001")

# Check
plot(st_geometry(accomack_properties), pch = 19, cex = 0.2)
plot(st_geometry(accomack_food_10[3, ]), add = T, col = "red", pch = 25, cex = 0.2)


#
# Test -------------------------------------------------
#

int <- st_intersection(accomack_properties, accomack_food_10[1, ])
cov <- (nrow(int) / nrow(accomack_properties)) * 100

plot(st_geometry(accomack_properties), pch = 19, cex = 0.2)
plot(st_geometry(int), add = T, col = "red", pch = 25, cex = 0.2)


#
# Scale -------------------------------------------------
#

# 2 counties have no food retailers? "51640" "51580" Filter out from loop.
setdiff(properties$fips_code, food_10$fips)
setdiff(food_10$fips, properties$fips_code)
food_fips <- intersect(food_10$fips, properties$fips_code)

# Galax "51640" and Covington "51580" 
galax_food <- food_10 %>% filter(str_detect(city, "Galax") == TRUE | 
                           str_detect(countyname, "Galax") == TRUE |
                             str_detect(mail_city, "Galax") == TRUE)

covington_food <- food_10 %>% filter(str_detect(city, "Covington") == TRUE | 
                                   str_detect(countyname, "Covington") == TRUE |
                                   str_detect(mail_city, "Covington") == TRUE)

galax_prop <- properties %>% filter(fips_code == "51640")
covington_prop <- properties %>% filter(fips_code == "51580")

remove(galax_prop)
remove(covington_prop)
remove(galax_food)
remove(covington_food)

# Loop through: Food 10
for(i in food_fips){
  
  propertydata <- properties %>% filter(fips_code == i)
  fooddata <- food_10 %>% filter(GEOID == i)
  
  for(j in 1:nrow(fooddata)){
    int <- st_intersection(propertydata, fooddata[j, ])
    cov <- (nrow(int) / nrow(propertydata)) * 100
    fooddata$food_coverage_10[j] <- cov
    
    assign(paste0("food_10_coverage_", i), fooddata)
  }
  
}

# Clean up
remove(cov)
remove(i)
remove(j)
remove(fooddata)
remove(int)
remove(propertydata)

# Loop through: Food 15
for(i in food_fips){
  
  propertydata <- properties %>% filter(fips_code == i)
  fooddata <- food_15 %>% filter(GEOID == i)
  
  for(j in 1:nrow(fooddata)){
    int <- st_intersection(propertydata, fooddata[j, ])
    cov <- (nrow(int) / nrow(propertydata)) * 100
    fooddata$food_coverage_15[j] <- cov
    
    assign(paste0("food_15_coverage_", i), fooddata)
  }
  
}


#
# Save -------------------------------------------------
#

food_10_coverage_final <- mget(ls(pattern = "^food_10_coverage_")) %>% bind_rows()
food_15_coverage_final <- mget(ls(pattern = "^food_15_coverage_")) %>% bind_rows()

write_rds(food_10_coverage_final, "./data/working/food/final_food_10_coverage.rds")
write_rds(food_15_coverage_final, "./data/working/food/final_food_15_coverage.rds")