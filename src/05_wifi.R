library(sf)
library(tigris)
library(tidyverse)
library(stringr)
library(janitor)
library(readr)


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

# Drop unused columns
wifi$objectid <- NULL
wifi$updated <- NULL

# Standardize names
wifi$locality <- str_to_title(wifi$locality)
county_va$NAMELSAD <- str_to_title(county_va$NAMELSAD)

# Check mismatches
setdiff(wifi$locality, county_va$NAMELSAD)
setdiff(county_va$NAMELSAD, wifi$locality)


#
# Join ----------------------------------------------------------------------------
#

data <- full_join(wifi, county_va, by = c("locality" = "NAMELSAD"))
data$geometry <- NULL


#
# Spatial ----------------------------------------------------------------------------
#

# There are no wifi hotspots coded for independent cities (16 cases). We drop these empty geometries.
test <- data %>% filter(is.na(lon) | is.na(lat))
data <- data %>% filter(!is.na(lon) & !is.na(lat))

# Convert to sf
data <- st_as_sf(data, coords = c("lon", "lat"))

st_crs(data) <- 4326
data <- st_transform(data, crs = 4326) 


#
# Write out ----------------------------------------------------------------------------
#

write_rds(data, "./data/working/wifi/final_wifi.rds")

