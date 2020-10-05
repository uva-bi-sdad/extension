library(sf)
library(tigris)
library(tidyverse)
library(stringr)
library(janitor)


#
# Read in ----------------------------------------------------------------------------
#

ems <- st_read("./data/original/hifld_ems_2010/ems.shp")


#
# Clean ----------------------------------------------------------------------------
#

# get only virginia ems, rename fips to geoid for merging
ems_va <- ems %>%
  filter(STATE == "VA") %>%
  rename(GEOID = FIPS) %>%
  clean_names()

# Capitalization
ems_va$name <- str_to_title(ems_va$name)
ems_va$address <- str_to_title(ems_va$address)
ems_va$address2 <- str_to_title(ems_va$address2)
ems_va$city <- str_to_title(ems_va$city)
ems_va$county <- str_to_title(ems_va$county)
ems_va$naicsdescr <- str_to_sentence(ems_va$naicsdescr)
ems_va$geoprec <- str_to_title(ems_va$geoprec)
ems_va$phoneloc <- str_to_lower(ems_va$phoneloc)
ems_va$state_id <- str_to_lower(ems_va$state_id)
ems_va$level <- str_to_lower(ems_va$level)
ems_va$specialty <- str_to_lower(ems_va$specialty)
ems_va$emslicense <- str_to_lower(ems_va$emslicense)
ems_va$frst <- str_to_lower(ems_va$frst)
ems_va$perm_id <- str_to_lower(ems_va$perm_id)
ems_va$gnis_id <- str_to_lower(ems_va$gnis_id)
ems_va$directions <- str_to_lower(ems_va$directions)
ems_va$emergtitle <- str_to_lower(ems_va$emergtitle)
ems_va$owner <- str_to_lower(ems_va$owner)

# get tigris county geometries for virginia
county_va <- counties(state = "VA", year = 2019, class = "sf")

# drop the geometry of the ems locations, but still have lat and long
ems_va <- st_drop_geometry(ems_va)

# left join them
ems_data <- left_join(ems_va, county_va, by = "GEOID")


#
# Filter to medical only ----------------------------------------------------------------------------
#

# filter for only medical services locations, of which there are 7 types
# unique(ems_data$naicsdescr)
# I am only filtering for "RESCUE SERVICES, MEDICAL", "AIR AMBULANCE SERVICES", "AMBULANCE AND FIRE SERVICE COMBINED", "AMBULANCE SERVICES, AIR OR GROUND", and 
# "FIRE AND RESCUE SERVICE" (but maybe we shouldn't keep this one bc not medical or ambulance but it is rescue)
# we are dropping "FIRE FIGHTING SERVICES AS A COMMERCIAL ACTIVITY" and "FIRE FIGHTER TRAINING SCHOOLS" for obvious reasons
# it only dropped 6 locations in VA

ems_data <- ems_data %>%
  filter(naicsdescr != c("Fire fighting services as a commercial activity", "Fire fighter training schools"))
