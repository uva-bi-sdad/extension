library(tidyverse)
library(viridis)
library(sf)
library(ggthemes)
library(RColorBrewer)
library(ggplot2)
library(tigris)
library(data.table)

options(scipen = 999)

######## USDA Food Desert Atlas Data Geographies #################


#
# Tigris Work ------------------------------------------------------------------------
#

usda_data <- fread("data/original/usda_food_atlas_2017/usda_food_atlas.csv")

usda_data <- usda_data %>%
  filter(State == "Virginia") %>%
  rename(GEOID = CensusTract)

usda_data$GEOID <- as.character(usda_data$GEOID)
usda_data <- as.data.frame(usda_data)

# load in tigris county tract shape files
tract_data <- tracts(51, cb = FALSE, year = 2017)

# change from sp to sf data type
tract_data <- st_as_sf(tract_data)

# join and make sf
usda_geo_data <- usda_data %>%
  left_join(tract_data)
usda_geo_data <- st_as_sf(usda_geo_data)

# Write
usda <- usda_geo_data %>% select(GEOID, State, County, 
                                 STATEFP, COUNTYFP, TRACTCE, NAME, geometry,
                                 lahunv1share, lahunv10share, lakids1share, lakids10share, 
                                 lalowi1share, lalowi10share, lapop1share, lapop10share, 
                                 laseniors1share, laseniors10share)
usda <- st_as_sf(usda)
usda <- usda %>% st_transform(4326)

write_rds(usda, "./data/working/usda/final_usda.rds")
