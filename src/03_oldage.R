library(tidycensus)
library(tidyverse)
library(sf)


# Get ACS older adult characteristics


# API key ------------------------------------------------------------------------
#

# installed census api key
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")


#
# Select variables ------------------------------------------------------------------------
#

acs_older_vars <- c(
  # Total older adults
  "B01001_020", "B01001_021", "B01001_022", "B01001_023", "B01001_024", "B01001_025",
  "B01001_044", "B01001_045", "B01001_046", "B01001_047", "B01001_048", "B01001_049", "B01001_001",
  # HI males age 65 +
  "B27001_024","B27001_026","B27001_027", "B27001_029",
  # HI females 65+
  "B27001_052","B27001_054","B27001_055" ,"B27001_057",
  # HEALTH STATUS
  "B18103_015", "B18103_016", "B18103_018", "B18103_019", "B18103_034", "B18103_035", "B18103_037", "B18103_038", 
  "B18102_015", "B18102_016", "B18102_018", "B18102_019", "B18102_034", "B18102_035", "B18102_037", "B18102_038", 
  "B18104_012", "B18104_013", "B18104_015", "B18104_016", "B18104_028", "B18104_029", "B18104_031", "B18104_032", 
  "B18105_012", "B18105_013", "B18105_015", "B18105_016", "B18105_028", "B18105_029", "B18105_031", "B18105_032", 
  "B18106_012", "B18106_013", "B18106_015", "B18106_016", "B18106_028", "B18106_029", "B18106_031", "B18106_032", 
  "B18107_009", "B18107_010", "B18107_012", "B18107_013", "B18107_022", "B18107_023", "B18107_025", "B18107_026", 
  "B18101_015", "B18101_016", "B18101_018", "B18101_019", "B18101_034", "B18101_035", "B18101_037", "B18101_038",
  #SNAP Hardship
  "B22001_003","B22001_006",
  #BELOW POVERTY
  "B17001_015","B17001_016", "B17001_029", "B17001_030", "B17001_044", "B17001_045", "B17001_058", "B17001_059",
  #HOUSEHOLDS
  "B11006_001","B11006_002", "B11006_004","B11006_005", "B11006_008",
  #EMPLOYMENT
  "B23001_073", "B23001_074", "B23001_078", "B23001_079", "B23001_083", "B23001_084", "B23001_159", "B23001_160", "B23001_164", 
  "B23001_165", "B23001_169", "B23001_170"
)


#
# Get data ------------------------------------------------------------------------
#

# Get county FIPS
countyfips <- get(data("fips_codes")) %>% filter(state == "VA")
countyfipscode <- countyfips$county_code

# Get data from 2014/18 5-year estimates at tract level
older_data_tract <- get_acs(geography = "tract", state = 51, county = countyfipscode,
                            variables = acs_older_vars,
                            year = 2018, survey = "acs5",
                            cache_table = TRUE, output = "wide", geometry = TRUE,
                            keep_geo_vars = TRUE)


#
# Calculate ------------------------------------------------------------------------
#

# Tract level
acs_older_coded <- older_data_tract %>% transmute(
  STATEFP = STATEFP,
  COUNTYFP = COUNTYFP,
  TRACTCE = TRACTCE,
  GEOID = GEOID,
  NAME.x = NAME.x,
  NAME.y = NAME.y,
  geometry = geometry,
  # TOTAL OLDER ADULTS
  older = (B01001_020E + B01001_021E + B01001_022E + B01001_023E + B01001_024E + B01001_025E +
           B01001_044E + B01001_045E + B01001_046E + B01001_047E + B01001_048E + B01001_049E) / B01001_001E * 100,
  #HEALTH INSURANCE
  nohealthins = (B27001_026E + B27001_029E + B27001_054E + B27001_057E) / (B27001_024E + B27001_027E + B27001_052E + B27001_055E) * 100,
  #HEALTH STATUS
  visdiff = (B18103_016E + B18103_019E + B18103_035E + B18103_038E) / (B18103_015E + B18103_018E + B18103_034E + B18103_037E) * 100,
  heardiff = (B18102_016E + B18102_019E + B18102_035E + B18102_038E) / (B18102_015E + B18102_018E + B18102_034E + B18102_037E) * 100,
  cogdiff = (B18104_013E + B18104_016E + B18104_029E + B18104_032E) / (B18104_012E + B18104_015E + B18104_028E + B18104_031E) * 100,
  ambdiff = (B18105_013E + B18105_016E + B18105_029E + B18105_032E) / (B18105_012E + B18105_015E + B18105_028E + B18105_031E) * 100,
  carediff = (B18106_013E + B18106_016E + B18106_029E + B18106_032E) / (B18106_012E + B18106_015E + B18106_028E + B18106_031E) * 100,
  ildiff = (B18107_010E + B18107_013E + B18107_023E + B18107_026E) / (B18107_009E + B18107_012E + B18107_022E + B18107_025E) * 100,
  disab = (B18101_016E + B18101_019E + B18101_035E + B18101_038E) / (B18101_015E + B18101_018E + B18101_034E + B18101_037E) * 100,
  #HARDSHIPS
  snap = B22001_003E / (B22001_006E + B22001_003E) * 100, # 60+ member households receiving SNAP, of all 60+ member households
  inpov = (B17001_029E + B17001_030E + B17001_015E + B17001_016E) / (B17001_015E + B17001_016E + B17001_029E + B17001_030E + B17001_044E + B17001_045E + B17001_058E + B17001_059E) * 100, #65+ with income below poverty level in past 12 months, of all population 65+ 
  #HOUSEHOLDS
  hhsixty_total = B11006_002E / B11006_001E * 100, # Households with one or more people 60 years and over of all households
  hhsixty_marr = B11006_004E / B11006_002E * 100, # married couple families with one or more people 60 years and over of all 60+ households
  hhsixty_single = B11006_005E / B11006_002E * 100, # single householder 60 years and over of all 60+ households
  hhsixty_nonfam = B11006_008E / B11006_002E * 100, # nonfamily households 60 years and over of all 60+ households
  #EMPLOYMENT: 65+ in labor force (unemployed or employed) of all 65+
  labfor = (B23001_160E + B23001_165E + B23001_170E + B23001_074E + B23001_079E + B23001_084E) / (B23001_159E + B23001_164E + B23001_169E + B23001_073E + B23001_078E + B23001_083E) * 100
)

# NaNs with 0 denominators recoded to NA.
acs_older_coded <- acs_older_coded %>% mutate(
  older = ifelse(is.nan(older), NA, older),
  nohealthins = ifelse(is.nan(nohealthins), NA, nohealthins),
  visdiff = ifelse(is.nan(visdiff), NA, visdiff),
  heardiff = ifelse(is.nan(heardiff), NA, heardiff),
  cogdiff = ifelse(is.nan(cogdiff), NA, cogdiff),
  ambdiff = ifelse(is.nan(ambdiff), NA, ambdiff),
  carediff = ifelse(is.nan(carediff), NA, carediff),
  ildiff = ifelse(is.nan(ildiff), NA, ildiff),
  disab = ifelse(is.nan(disab), NA, disab),
  snap = ifelse(is.nan(snap), NA, snap),
  inpov = ifelse(is.nan(inpov), NA, inpov),
  hhsixty_total = ifelse(is.nan(hhsixty_total), NA, hhsixty_total),
  hhsixty_marr = ifelse(is.nan(hhsixty_marr), NA, hhsixty_marr),
  hhsixty_single = ifelse(is.nan(hhsixty_single), NA, hhsixty_single),
  hhsixty_nonfam = ifelse(is.nan(hhsixty_nonfam), NA, hhsixty_nonfam),
  labfor = ifelse(is.nan(labfor), NA, labfor)
  )

  
#
# Select rural counties only (according to VDH) -------------------------------------------------------------------------------
#

# Read in
rural <- read_csv("./data/original/srhp_rurality_2020/omb_srhp_rurality.csv", 
                  col_names = TRUE, col_types = list(col_character(), col_factor(), col_factor()))
rural <- rural %>% filter(RuralUrban == "R") %>% select(FIPS)

# Join
acs_older_coded$FIPS <- paste0(acs_older_coded$STATEFP, acs_older_coded$COUNTYFP)
acs_older_tract_final <- acs_older_coded %>% filter(acs_older_coded$FIPS %in% rural$FIPS)

# Add county names
countyfips$FIPS <- paste0(countyfips$state_code, countyfips$county_code)
countyfips <- countyfips %>% select(county, FIPS)
acs_older_tract_final <- left_join(acs_older_tract_final, countyfips, by = "FIPS")

# Prepare areaname variable
acs_older_tract_final$areaname <- acs_older_tract_final$NAME.y


#
# Write -------------------------------------------------------------------------------
#

acs_older_tract_final <- acs_older_tract_final %>% st_transform(4326)

write_rds(acs_older_tract_final, "./data/working/acs/final_older.rds")
