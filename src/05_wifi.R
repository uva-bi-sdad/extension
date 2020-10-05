library(sf)
library(tigris)
library(tidyverse)
library(stringr)
library(janitor)


#
# Read in ----------------------------------------------------------------------------
#

wifi <- read.csv("./data/original/cconnect_wifi_2020/WiFi_Hotspot_Locations_add_coordinate_08282020.csv") %>%
  clean_names()


#
# Get tigris county geometries for Virginia
#

county_va <- counties(state = "VA", year = 2019, class = "sf") %>%
  select(STATEFP, COUNTYFP, GEOID, NAME, NAMELSAD)


#
# Clean ----------------------------------------------------------------------------
#

# Rename the localities to match how crazy the tigris stuff is
wifi$county <- str_to_title(wifi$locality)
county_va$NAMELSAD <- str_to_title(county_va$NAMELSAD)

# Spatial
wifi <- st_as_sf(wifi, coords = c("lon", "lat"))

st_crs(county_va) <- 4326
st_crs(wifi) <- 4326

county_va <- st_transform(county_va, crs = 4326) 

# Join (non-spatial)
wifi_data <- left_join(wifi, county_va, by = c("locality" = "NAMELSAD"))

