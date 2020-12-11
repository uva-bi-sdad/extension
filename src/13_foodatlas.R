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


#
# Select rural counties only (according to VDH) -------------------------------------------------------------------------------
#

rural <- read_csv("./data/original/srhp_rurality_2020/omb_srhp_rurality.csv", 
                  col_names = TRUE, col_types = list(col_character(), col_factor(), col_factor()))
rural <- rural %>% filter(RuralUrban == "R") %>% select(FIPS)

usda_geo_data$FIPS <- substr(usda_geo_data$GEOID, 1, 5)

usda_geo_data <- usda_geo_data %>% filter(usda_geo_data$FIPS %in% rural$FIPS)

# Write
usda <- usda_geo_data %>% select(GEOID, State, County, 
                                 STATEFP, COUNTYFP, TRACTCE, NAME, FIPS, geometry,
                                 lahunv1share, lahunv10share, lakids1share, lakids10share, 
                                 lalowi1share, lalowi10share, lapop1share, lapop10share, 
                                 laseniors1share, laseniors10share)

usda <- usda %>% st_transform(4326)

write_rds(usda, "./data/working/usda/final_usda.rds")
