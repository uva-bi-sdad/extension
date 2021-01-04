library(tidycensus)
library(tidyverse)
library(sf)


#
# API key ------------------------------------------------------------------------
#

# installed census api key
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")


#
# Get borders ------------------------------------------------------------------------
#

# Get county FIPS
countyfips <- get(data("fips_codes")) %>% filter(state == "VA")

# Get data from 2014/18 5-year estimates at tract level 
data_countyborder <- get_acs(geography = "county", state = 51, 
                     variables = "B28001_001",
                     year = 2018, survey = "acs5",
                     cache_table = TRUE, output = "wide", geometry = TRUE,
                     keep_geo_vars = TRUE)
data_countyborder <- data_countyborder %>% select(GEOID, NAME.y, geometry)


#
# Select VDH rural counties only ------------------------------------------------------------------------
#

# Read in
rural <- read_csv("./data/original/srhp_rurality_2020/omb_srhp_rurality.csv", 
                  col_names = TRUE, col_types = list(col_character(), col_factor(), col_factor()))
rural <- rural %>% filter(RuralUrban == "R") %>% select(FIPS)

# Join
data_countyborder_final <- data_countyborder %>% filter(data_countyborder$GEOID %in% rural$FIPS)

# Add county names
countyfips$FIPS <- paste0(countyfips$state_code, countyfips$county_code)
countyfips <- countyfips %>% select(county, FIPS)
data_countyborder_final <- left_join(data_countyborder_final, countyfips, by = c("GEOID" = "FIPS"))
data_countyborder_final <- data_countyborder_final %>% select(-NAME.y)

# Projection
data_countyborder_final <- data_countyborder_final %>% st_transform(4326)


#
# Write ------------------------------------------------------------------------
#

write_rds(data_countyborder_final, "./data/working/acs/final_countyborders.rds")
