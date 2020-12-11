library(tidycensus)
library(tidyverse)
library(sf)

# Get ACS technology data



#
# API key ------------------------------------------------------------------------
#

# installed census api key
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")


#
# Select variables ------------------------------------------------------------------------
#

# Select variables
acsvars <- c(
  # types of computers
  "B28001_001", "B28001_002", "B28001_003", "B28001_005","B28001_007", "B28001_009", "B28001_011", 
  # presence and types of internet subscriptions
  "B28002_001", "B28002_003", "B28002_005", "B28002_007", "B28002_009",  "B28002_013"
)


#
# Get data ------------------------------------------------------------------------
#

# Get county FIPS
countyfips <- get(data("fips_codes")) %>% filter(state == "VA")
countyfips <- countyfips$county_code

# Get data from 2014/18 5-year estimates at tract level 
data_bgrp <- get_acs(geography = "block group", state = 51, 
                      variables = acsvars,
                      year = 2018, survey = "acs5",
                      cache_table = TRUE, output = "wide", geometry = TRUE,
                      keep_geo_vars = TRUE)


#
# Calculate ------------------------------------------------------------------------
#

# Tract level 
acs_bgrp <- data_bgrp %>% transmute(
  STATEFP = STATEFP,
  COUNTYFP = COUNTYFP,
  TRACTCE = TRACTCE,
  BLKGRPCE = BLKGRPCE,
  GEOID = GEOID,
  NAME.x = NAME.x,
  geometry = geometry,
  nocomputer = B28001_011E/B28001_001E * 100,
  laptop = B28001_003E/B28001_001E * 100,
  smartphone = B28001_005E/B28001_001E * 100,
  tablet = B28001_007E/B28001_001E * 100,
  othercomputer = B28001_009E/B28001_001E * 100,
  nointernet = B28002_013E/B28002_001E * 100,
  satellite = B28002_009E/B28002_001E * 100,
  cellular = B28002_005E/B28002_001E * 100,
  dialup = B28002_003E/B28002_001E * 100,
  broadband = B28002_007E/B28002_001E * 100
)


#
# Select rural counties only (according to VDH) -------------------------------------------------------------------------------
#

rural <- read_csv("./data/original/srhp_rurality_2020/omb_srhp_rurality.csv", 
                  col_names = TRUE, col_types = list(col_character(), col_factor(), col_factor()))
rural <- rural %>% filter(RuralUrban == "R") %>% select(FIPS)

acs_bgrp$FIPS <- paste0(acs_bgrp$STATEFP, acs_bgrp$COUNTYFP)
  
acs_bgrp_final <- acs_bgrp %>% filter(acs_bgrp$FIPS %in% rural$FIPS)


#
# Write -------------------------------------------------------------------------------
#

acs_bgrp_final <- acs_bgrp_final %>% st_transform(4326)

write_rds(acs_bgrp_final, "./data/working/acs/final_technology.rds")