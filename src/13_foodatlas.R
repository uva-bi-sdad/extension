library(tidyverse)
library(viridis)
library(sf)
library(ggthemes)
library(RColorBrewer)
library(ggplot2)
library(tigris)
library(data.table)

options(scipen = 999)

# USDA Food Research Atlas Data


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


#
# Select and transform -------------------------------------------------------------------------------
#

# Select only relevant variables
usda <- usda_geo_data %>% select(GEOID, State, County, 
                                 STATEFP, COUNTYFP, TRACTCE, NAME, NAMELSAD, FIPS, geometry,
                                 lahunv1share, lahunv10share, lakids1share, lakids10share, 
                                 lalowi1share, lalowi10share, lapop1share, lapop10share, 
                                 laseniors1share, laseniors10share)

# These are all shares, so multiply by 100 because we are displaying percentages.
usda <- usda %>% mutate(lahunv1share = lahunv1share * 100,
                        lahunv10share = lahunv10share * 100, 
                        lakids1share = lakids1share * 100, 
                        lakids10share = lakids10share * 100, 
                        lalowi1share = lalowi1share * 100, 
                        lalowi10share = lalowi10share * 100, 
                        lapop1share = lapop1share * 100, 
                        lapop10share = lapop10share * 100,
                        laseniors1share = laseniors1share * 100, 
                        laseniors10share = laseniors10share * 100)

# Projection
usda <- usda %>% st_transform(4326)


#
# Write -------------------------------------------------------------------------------
#

write_rds(usda, "./data/working/usda/final_usda.rds")


