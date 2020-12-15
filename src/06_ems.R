library(sf)
library(tigris)
library(tidyverse)
library(stringr)
library(janitor)


#
# Read in ----------------------------------------------------------------------------
#

ems <- st_read("./data/original/hifld_ems_2010/ems.shp")

table(st_is_valid(ems))
st_is_longlat(ems)

# Drop weird/corrupt geometries that came with this and make back into a spatial object later after it's filtered
ems <- st_drop_geometry(ems)


#
# Clean ----------------------------------------------------------------------------
#

# get only virginia ems, rename fips to geoid for merging
ems <- ems %>%
  filter(STATE == "VA") %>%
  rename(GEOID = FIPS) %>%
  clean_names()

# Capitalization
ems$name <- str_to_title(ems$name)
ems$address <- str_to_title(ems$address)
ems$address2 <- str_to_title(ems$address2)
ems$city <- str_to_title(ems$city)
ems$county <- str_to_title(ems$county)
ems$naicsdescr <- str_to_sentence(ems$naicsdescr)
ems$geoprec <- str_to_title(ems$geoprec)
ems$phoneloc <- str_to_lower(ems$phoneloc)
ems$state_id <- str_to_lower(ems$state_id)
ems$level <- str_to_lower(ems$level)
ems$specialty <- str_to_lower(ems$specialty)
ems$emslicense <- str_to_lower(ems$emslicense)
ems$frst <- str_to_lower(ems$frst)
ems$perm_id <- str_to_lower(ems$perm_id)
ems$gnis_id <- str_to_lower(ems$gnis_id)
ems$directions <- str_to_lower(ems$directions)
ems$emergtitle <- str_to_lower(ems$emergtitle)
ems$owner <- str_to_lower(ems$owner)


#
# Filter to medical only ----------------------------------------------------------------------------
#

# filter for only medical services locations, of which there are 7 types
# unique(ems_data$naicsdescr)
# I am only filtering for "RESCUE SERVICES, MEDICAL", "AIR AMBULANCE SERVICES", "AMBULANCE AND FIRE SERVICE COMBINED", "AMBULANCE SERVICES, AIR OR GROUND", and 
# "FIRE AND RESCUE SERVICE" (but maybe we shouldn't keep this one bc not medical or ambulance but it is rescue)
# we are dropping "FIRE FIGHTING SERVICES AS A COMMERCIAL ACTIVITY" and "FIRE FIGHTER TRAINING SCHOOLS" for obvious reasons
# it only dropped 6 locations in VA

ems <- ems %>%
  filter(naicsdescr != "Fire fighting services as a commercial activity",
         naicsdescr != "Fire fighter training schools")


#
# Back to spatial ----------------------------------------------------------------------------
#

ems <- st_as_sf(ems, coords = c("longitude", "latitude"))
st_crs(ems) <- 4326
ems <- st_transform(ems, crs = 4326) 


#
# Write out ----------------------------------------------------------------------------
#

write_rds(ems, "./data/working/ems/final_ems.rds")


#
# For app ----------------------------------------------------------------------------
#

# Select
final_ems <- final_ems %>% select("objectid", "id", "address", "city", "state", "zip", "county", 
                                  "geoid", "naicsdescr", "name", "geometry") %>%
                           rename("countyname" = "county")


# Add clean county name
countyfips <- get(data("fips_codes")) %>% filter(state == "VA")
countyfips$FIPS <- paste0(countyfips$state_code, countyfips$county_code)
countyfips <- countyfips %>% select(county, FIPS)

final_ems <- left_join(final_ems, countyfips, by = c("geoid" = "FIPS"))

# Write
write_rds(final_ems, "./data/working/ems/final_ems_forapp.rds")

