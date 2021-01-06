library(sf)
library(tigris)
library(tidyverse)
library(stringr)
library(janitor)
library(readr)
library(tidycensus)

#
# Read in --------------------------------------------------------------
#

data1 <- read_rds("/home/tp2sk/Git/extension/data/working/foodretail/foodretail_nonmiss.rds")
data2 <- read_rds("/home/tp2sk/Git/extension/data/working/foodretail/foodretail_missing_coded.rds") %>%
  select(-fulladdress, -confidence)

data <- rbind(data1, data2)
data <- st_transform(data, 4326)

remove(data1)
remove(data2)


#
# Resolve naming for matches ----------------------------------------------------------------------------
#

# Check these:
table(data$county)
# Bedford city --> Bedford County
# Bristol city --> Bristol city (not present)
# Covington city --> Covington city (not present)
# Galax city --> Galax city (not present) 
# Hopewell city --> (not present)
# Manassas city --> not present, all noted as Manassas Park city

# Select
data <- data %>% select(-business_id, -address2, -phone_primary, -url, 
                        -mail_address, -mail_city, -mail_state, -mail_zip, -email, 
                        -date_stamp, -last_updated, -geo_method)

# Add clean county name: fix names for matching first
data$county <- ifelse(str_detect(data$county, "City"), data$county, paste0(data$county, " County"))
data$county <- str_replace_all(data$county, "City", "city")
  
data$county <- str_replace_all(data$county, "Virginia Beach County", "Virginia Beach city")
data$county <- str_replace_all(data$county, "Alexandria County", "Alexandria city")
data$county <- str_replace_all(data$county, "Petersburg County", "Petersburg city")
data$county <- str_replace_all(data$county, "Newport News County", "Newport News city")
data$county <- str_replace_all(data$county, "Charlottesville County", "Charlottesville city")
data$county <- str_replace_all(data$county, "Poquoson County", "Poquoson city")
data$county <- str_replace_all(data$county, "Radford County", "Radford city")
data$county <- str_replace_all(data$county, "Harrisonburg County", "Harrisonburg city")
data$county <- str_replace_all(data$county, "Hampton County", "Hampton city")
data$county <- str_replace_all(data$county, "Salem County", "Salem city")
data$county <- str_replace_all(data$county, "Staunton County", "Staunton city")
data$county <- str_replace_all(data$county, "Suffolk County", "Suffolk city")
data$county <- str_replace_all(data$county, "Waynesboro County", "Waynesboro city")
data$county <- str_replace_all(data$county, "Winchester County", "Winchester city")
data$county <- str_replace_all(data$county, "Chesapeake County", "Chesapeake city")
data$county <- str_replace_all(data$county, "Colonial Heights County", "Colonial Heights city")
data$county <- str_replace_all(data$county, "Norfolk County", "Norfolk city")
data$county <- str_replace_all(data$county, "Emporia County", "Emporia city")
data$county <- str_replace_all(data$county, "Falls Church County", "Falls Church city")
data$county <- str_replace_all(data$county, "Fredericksburg County", "Fredericksburg city")
data$county <- str_replace_all(data$county, "Lynchburg County", "Lynchburg city")
data$county <- str_replace_all(data$county, "Manassas Park County", "Manassas Park city")
data$county <- str_replace_all(data$county, "Portsmouth County", "Portsmouth city")
data$county <- str_replace_all(data$county, "Williamsburg County", "Williamsburg city")
data$county <- str_replace_all(data$county, "Danville County", "Danville city")
data$county <- str_replace_all(data$county, "Lexington County", "Lexington city")
data$county <- str_replace_all(data$county, "Martinsville County", "Martinsville city")
data$county <- str_replace_all(data$county, "Buena Vista County", "Buena Vista city")
data$county <- str_replace_all(data$county, "Norton County", "Norton city")

data$county <- str_replace_all(data$county, "Charles city", "Charles City County")
data$county <- str_replace_all(data$county, "James city", "James City County")

# Read in clean names
countyfips <- get(data("fips_codes")) %>% filter(state == "VA")
countyfips$FIPS <- paste0(countyfips$state_code, countyfips$county_code)
countyfips <- countyfips %>% select(county, FIPS)

# Check again
setdiff(data$county, countyfips$county)
setdiff(countyfips$county, data$county)

final_retail <- left_join(data, countyfips, by = "county")


#
# Select rural counties only (according to VDH) -------------------------------------
#

rural <- read_csv("./data/original/srhp_rurality_2020/omb_srhp_rurality.csv", 
                  col_names = TRUE, col_types = list(col_character(), col_factor(), col_factor()))
rural <- rural %>% filter(RuralUrban == "R") %>% select(FIPS)

final_retail <- final_retail %>% filter(final_retail$FIPS %in% rural$FIPS)

table(final_retail$profiles)


#
# Write -------------------------------------
#

write_rds(final_retail, "./data/working/foodretail/final_foodretail_forapp.rds")


