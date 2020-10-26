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
# Write out ----------------------------------------------------------------------------
#

write_rds(ems, "./data/working/ems/final_ems.rds")