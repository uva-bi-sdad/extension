library(sf)
library(tigris)
library(tidyverse)
library(stringr)

#
# ems and wifi cleaning --------------------------------------
#

wifi <- read.csv("~/git/extension/data/original/cconnect_wifi_2020/WiFi_Hotspot_Locations_add_coordinate_08282020.csv")
ems <- st_read("~/git/extension/data/original/hifld_ems_2010/ems.shp")

#
# ems cleaning --------------------------------------
#

# get only virginia ems, rename fips to geoid for merging
ems_va <- ems %>%
  filter(STATE == "VA") %>%
  rename(GEOID = FIPS)

# rename the locations so they'll look nice
ems_va$NAME <- str_to_title(ems_va$NAME, locale = "en")

# get tigris county geometries for virginia
county_va <- counties(state = "VA", year = 2019)

# drop the geometry of the ems locations, but still have lat and long
ems_va <- st_drop_geometry(ems_va)

# left join them
ems_data <- left_join(ems_va, county_va, by = "GEOID")

# filter for only medical services locations, of which there are 7 types
# unique(ems_data$NAICSDESCR)
# I am only filtering for "RESCUE SERVICES, MEDICAL", "AIR AMBULANCE SERVICES", "AMBULANCE AND FIRE SERVICE COMBINED", "AMBULANCE SERVICES, AIR OR GROUND", and 
# "FIRE AND RESCUE SERVICE" (but maybe we shouldn't keep this one bc not medical or ambulance but it is rescue)
# we are dropping "FIRE FIGHTING SERVICES AS A COMMERCIAL ACTIVITY" and "FIRE FIGHTER TRAINING SCHOOLS" for obvious reasons
# it only dropped 6 locations in VA

ems_data <- ems_data %>%
  filter(NAICSDESCR != c("FIRE FIGHTING SERVICES AS A COMMERCIAL ACTIVITY", "FIRE FIGHTER TRAINING SCHOOLS"))

#
# wifi cleaning --------------------------------------
#

# rename the localities to match how crazy the tigris stuff is
wifi$county <- sub(" County", " County", wifi$Locality)
wifi$county <- sub(" City", " city", wifi$county)
wifi$county <- sub("Charles city County", "Charles City County", wifi$county)
wifi <- rename(wifi, c("NAMELSAD" = "county"))

wifi_data <- left_join(wifi, county_va, by = "NAMELSAD")

#
# write data -----------------------------------------
#


