library(tidycensus)
library(tidyverse)
library(sf)
library(readr)


#
# API key ------------------------------------------------------------------------
#

# installed census api key
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")


#
# Select variables ------------------------------------------------------------------------
#

# total pop
# B01003_001
# % population age 65+
# B01001_020:25 (male), B01001_044:49 (female) / B01001_001    
# % population age <=18
# B01001_003:006 (male), B01001_027:30 (female) / B01001_001
# % population Hispanic or Latino
# B03001_003 / B03001_001
# % population Black
# B02001_003 / B02001_001
# % population over age 25 without a BA degree
# B15003_002:021 / B15003_001
# % population in labor force that is unemployed
# B23025_005 / B23025_002
# % population living under 100% poverty line
# B17001_002 / B17001_001
# % population without health insurance
# (005 + 008 + 011 + 014 + 017 + 020 + 023 + 026 + 029 + 033 + 036 + 039 + 042 + 045 + 048 + 051 + 054 + 057) /  B27001_001

# Select variables
acsvars <- c(
  # total pop
  "B01003_001",
  # age 65 +
  "B01001_020", "B01001_021", "B01001_022", "B01001_023", "B01001_024", "B01001_025",
  "B01001_044", "B01001_045", "B01001_046", "B01001_047", "B01001_048", "B01001_049",
  "B01001_001",
  # age <18
  "B01001_003", "B01001_004", "B01001_005", "B01001_006",
  "B01001_027", "B01001_028", "B01001_029", "B01001_030",
  # Hispanic
  "B03001_003", "B03001_001",
  # Black
  "B02001_003", "B02001_001",
  # Without BA
  "B15003_002", "B15003_003", "B15003_004", "B15003_005", "B15003_006", "B15003_007",
  "B15003_008", "B15003_009", "B15003_010", "B15003_011", "B15003_012", "B15003_013",
  "B15003_014", "B15003_015", "B15003_016", "B15003_017", "B15003_018", "B15003_019",
  "B15003_020", "B15003_021", "B15003_001",
  # Unemployed
  "B23025_005", "B23025_002",
  # In poverty
  "B17001_002", "B17001_001",
  # Without health insurance
  "B27001_005", "B27001_008", "B27001_011", "B27001_014", "B27001_017", "B27001_020", "B27001_023",
  "B27001_026", "B27001_029", "B27001_033", "B27001_036", "B27001_039", "B27001_042", "B27001_045",
  "B27001_048", "B27001_051", "B27001_054", "B27001_057", "B27001_001",
  # type of HI coverage by age, without HI
  "B27010_001", "B27010_017", "B27010_033", "B27010_050", "B27010_066",
  # public HI status by sex by age
  "B27003_001", "B27003_005", "B27003_008", "B27003_011", "B27003_014", "B27003_017", "B27003_020",
  "B27003_023", "B27003_026", "B27003_029", "B27003_033", "B27003_036", "B27003_039", "B27003_042",
  "B27003_045", "B27003_048", "B27003_051", "B27003_054", "B27003_057",
  # private HI status by sex by age
  "B27002_001", "B27002_005", "B27002_008", "B27002_011", "B27002_014", "B27002_017", "B27002_020",
  "B27002_023", "B27002_026", "B27002_029", "B27002_033", "B27002_036", "B27002_039", "B27002_042",
  "B27002_045", "B27002_048", "B27002_051", "B27002_054", "B27002_057",
  # public assistance or snap in past 12 months
  "B19058_001", "B19058_002"
)


#
# Get data ------------------------------------------------------------------------
#

# Get county FIPS
countyfips <- get(data("fips_codes")) %>% filter(state == "VA")
countyfipscode <- countyfips$county_code

# Get data from 2014/18 5-year estimates at tract level
data_tract <- get_acs(geography = "tract", state = 51, county = countyfipscode,
                            variables = acsvars,
                            year = 2018, survey = "acs5",
                            cache_table = TRUE, output = "wide", geometry = TRUE,
                            keep_geo_vars = TRUE)
data_tract <- data_tract %>% filter(!is.na(STATEFP))


#
# Calculate ------------------------------------------------------------------------
#

# Tract level 
acs_tract <- data_tract %>% transmute(
  STATEFP = STATEFP,
  COUNTYFP = COUNTYFP,
  TRACTCE = TRACTCE,
  GEOID = GEOID,
  NAME.x = NAME.x,
  NAME.y = NAME.y,
  ALAND = ALAND,
  AWATER = AWATER,
  geometry = geometry,
  totalpop_trct = B01003_001E,
  age65 = (B01001_020E + B01001_021E + B01001_022E + B01001_023E + B01001_024E + B01001_025E +
             B01001_044E + B01001_045E + B01001_046E + B01001_047E + B01001_048E + B01001_049E) / B01001_001E * 100,
  under18 = (B01001_003E + B01001_004E + B01001_005E + B01001_006E +
               B01001_027E + B01001_028E + B01001_029E + B01001_030E) / B01001_001E * 100,
  hispanic = B03001_003E / B03001_001E * 100,
  black = B02001_003E / B02001_001E * 100,
  noba = (B15003_002E + B15003_003E + B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E +
            B15003_009E + B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E +
            B15003_016E + B15003_017E + B15003_018E + B15003_019E + B15003_020E + B15003_021E) / B15003_001E * 100,
  unempl = B23025_005E / B23025_002E * 100,
  inpov = B17001_002E / B17001_001E * 100,
  nohealthins = (B27001_005E + B27001_008E + B27001_011E + B27001_014E + B27001_017E + B27001_020E + B27001_023E + 
                   B27001_026E + B27001_029E + B27001_033E + B27001_036E + B27001_039E + B27001_042E + B27001_045E +
                   B27001_048E + B27001_051E + B27001_054E + B27001_057E) / B27001_001E * 100,
  publicins = (B27003_005E + B27003_008E + B27003_011E + B27003_014E + B27003_017E + B27003_020E + B27003_023E + 
                 B27003_026E + B27003_029E + B27003_033E + B27003_036E + B27003_039E + B27003_042E + B27003_045E +
                 B27003_048E + B27003_051E + B27003_054E + B27003_057E) / B27003_001E * 100,
  privateins = (B27002_005E + B27002_008E + B27002_011E + B27002_014E + B27002_017E + B27002_020E + B27002_023E + 
                  B27002_026E + B27002_029E + B27002_033E + B27002_036E + B27002_039E + B27002_042E + B27002_045E +
                  B27002_048E + B27002_051E + B27002_054E + B27002_057E) / B27002_001E * 100,
  snap = B19058_002E/B19058_001E * 100
)

# NaNs with 0 denominators recoded to NA.
acs_tract <- acs_tract %>% mutate(
  age65 = ifelse(is.nan(age65), NA, age65),
  under18 = ifelse(is.nan(under18), NA, under18),
  hispanic = ifelse(is.nan(hispanic), NA, hispanic),
  black = ifelse(is.nan(black), NA, black),
  noba = ifelse(is.nan(noba), NA, noba),
  unempl = ifelse(is.nan(unempl), NA, unempl),
  inpov = ifelse(is.nan(inpov), NA, inpov),
  nohealthins = ifelse(is.nan(nohealthins), NA, nohealthins),
  publicins = ifelse(is.nan(publicins), NA, publicins),
  privateins = ifelse(is.nan(privateins), NA, privateins),
  snap = ifelse(is.nan(snap), NA, snap)
)


#
# Select rural counties only (according to VDH) -------------------------------------------------------------------------------
#

# Read in
rural <- read_csv("./data/original/srhp_rurality_2020/omb_srhp_rurality.csv", 
                  col_names = TRUE, col_types = list(col_character(), col_factor(), col_factor()))
rural <- rural %>% filter(RuralUrban == "R") %>% select(FIPS)

# Join
acs_tract$FIPS <- paste0(acs_tract$STATEFP, acs_tract$COUNTYFP)
acs_tract_final <- acs_tract %>% filter(acs_tract$FIPS %in% rural$FIPS)

# Add county names
countyfips$FIPS <- paste0(countyfips$state_code, countyfips$county_code)
countyfips <- countyfips %>% select(county, FIPS)
acs_tract_final <- left_join(acs_tract_final, countyfips, by = "FIPS")

# Prepare areaname variable
acs_tract_final$areaname <- acs_tract_final$NAME.y


#
# Write -------------------------------------------------------------------------------
#

acs_tract_final <- acs_tract_final %>% st_transform(4326)
acs_tract_final <- acs_tract_final %>% select(-ALAND, -AWATER, -TRACTCE, -STATEFP, -COUNTYFP)

write_rds(acs_tract_final, "./data/working/acs/final_socdem.rds")

