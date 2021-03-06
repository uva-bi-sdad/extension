library(dplyr)
library(tidycensus)
library(sf)
library(readxl)
library(janitor)
library(stringr)
library(leaflet)
library(readr)


# Get VA geographies at county and tract level to examine rurality. 


# Resources
# USDA frontier and remote: https://www.ers.usda.gov/data-products/frontier-and-remote-area-codes/
# USDA rural urban commuting area codes: https://www.ers.usda.gov/data-products/rural-urban-commuting-area-codes/
# USDA rural urban continuum codes: https://www.ers.usda.gov/data-products/rural-urban-continuum-codes/
# USDA urban influence codes: https://www.ers.usda.gov/data-products/urban-influence-codes/
# OMB metro/nonmetro: https://www.census.gov/programs-surveys/metro-micro.html
# Index of relative rurality: https://purr.purdue.edu/publications/2960/1

# OMB Areas based on the 2010 standards and Census Bureau data were delineated in February of 2013, and updated in July 
# of 2015, August of 2017, April of 2018, September of 2018, and March of 2020.
# https://www.census.gov/geographies/reference-files/time-series/demo/metro-micro/delineation-files.html

# VA office of rural health: https://www.vdh.virginia.gov/health-equity/division-of-rural-health/
# VDH preferred classification: https://www.vdh.virginia.gov/content/uploads/sites/76/2016/06/2013VSRHP-final.pdf
# VDH preferred classification: https://doi.org/10.1177/0160017605279000
# VA "Are you rural": https://www.vdh.virginia.gov/health-equity/rural-virginia-defined/
# Choosing rural definitions - implications for policy: http://www.rupri.org/Forms/RuralDefinitionsBrief.pdf

# Defining and measuring rurality in the US: https://sites.nationalacademies.org/cs/groups/dbassesite/documents/webpage/dbasse_168031.pdf

# HRSA "are you rural" tool
# Rural Health Grants Eligibility Analyzer
# https://data.hrsa.gov/Content/Documents/tools/rural-health/forhpeligibleareas.pdf -- list for Virginia
# https://data.hrsa.gov/tools/rural-health


#
# READ IN RURALITY CODES ------------------------------------------------------------------------
#

# 2020 OMB metro nonmetro: COUNTY LEVEL ------------------------------------------------------------------
# omb <- read_xls("./data/original/omb_metro_2020/list1_2020.xls", sheet = 1, range = "A3:L1919",
#                  col_names = TRUE, col_types = c(rep("text", 12))) %>%
#  clean_names()

# 2013 National Center for Health Statistics: COUNTY LEVEL------------------------------------------------------------------
nchs <- read_xlsx("./data/original/nchs_urbanrural_2013/NCHSURCodes2013.xlsx", sheet = 1, 
                  col_names = TRUE, col_types = c(rep("text", 9))) %>%
  clean_names() 
nchs <- nchs %>% filter(state_abr == "VA") %>% select(fips_code, county_name, x2013_code) %>%
  rename(nchs_2013 = x2013_code)
nchs$nchs_2013 <- factor(nchs$nchs_2013, labels = c("1 Large central metro", "2 Large fringe metro", "3 Medium metro",
                                                    "4 Small metro", "5 Micropolitan", "6 Noncore"), ordered = T)
nchs <- nchs %>% mutate(nchs_2013_desc = case_when(nchs_2013 == "1 Large central metro" ~ "NCHS-defined central counties of MSAs of 1 million or more population",
                                                   nchs_2013 == "2 Large fringe metro" ~ "NCHS-defined fringe counties of MSAs of 1 million or more population",
                                                   nchs_2013 == "3 Medium metro" ~ "Counties within MSAs of 250,000-999,999 population",
                                                   nchs_2013 == "4 Small metro" ~ "Counties within MSAS of 50,000 to 249,999 population",
                                                   nchs_2013 == "5 Micropolitan" ~ "Counties in micropolitan statistical areas",
                                                   nchs_2013 == "6 Noncore" ~ "Counties not within micropolitan statistical areas"))
nchs$county_name <- str_to_title(nchs$county_name)

# 2010 Index of Relative Rurality: COUNTY LEVEL ------------------------------------------------------------------
irr <- read_xlsx("./data/original/purdue_irr_2010/IRR_2000_2010.xlsx", sheet = 2, range = "A1:C3142",
                 col_names = TRUE, col_types = c("text", "text", "numeric")) %>%
       clean_names()
irr <- irr %>% rename(county_state_name = county_name)

irr <- irr %>% filter(str_detect(fips2010, "^51") & nchar(fips2010) == 5)
irr$county_state_name <- str_to_title(irr$county_state_name)

# Note: counties deleted due to lack of data are 15901 and 51560.

# 2013 Rural-Urban Continuum Codes: COUNTY LEVEL ------------------------------------------------------------------
rucc <- read_xls("./data/original/usda_rucc_2013/ruralurbancodes2013.xls", sheet = 1, 
                 col_names = TRUE, col_types = c(rep("text", 3), rep("numeric", 2), "text")) %>%
        clean_names() %>%
        rename(description_rucc = description)
rucc$rucc_2013 <- factor(rucc$rucc_2013, labels = c("1 Metro", "2 Metro", "3 Metro",
                                                    "4 Nonmetro", "5 Nonmetro", "6 Nonmetro",
                                                    "7 Nonmetro", "8 Nonmetro", "9 Nonmetro"), ordered = T)
rucc <- rucc %>% select(-population_2010)
rucc <- rucc %>% filter(state == "VA")
rucc$county_name <- str_to_title(rucc$county_name)

# 2010 Rural-Urban Commuting Area Codes: TRACT LEVEL ------------------------------------------------------------------
ruca <- read_xlsx("./data/original/usda_ruca_2010/ruca2010revised.xlsx", sheet = 1, skip = 1,
                  col_names = TRUE, col_types = c(rep("text", 4), rep("numeric", 5))) %>%
  clean_names()
ruca <- ruca %>% rename(tractfips = state_county_tract_fips_code_lookup_by_address_at_http_www_ffiec_gov_geocode)
ruca$primary_ruca_code_2010 <- ifelse(ruca$primary_ruca_code_2010 == 99, NA, ruca$primary_ruca_code_2010)

ruca <- ruca %>% select(1:6)
ruca <- ruca %>% filter(select_state == "VA")
ruca$select_county <- str_to_title(ruca$select_county)

# 2013 Urban Influence Codes: COUNTY LEVEL ------------------------------------------------------------------
urbinf <- read_excel("./data/original/usda_urbinfl_2013/UrbanInfluenceCodes2013.xls", sheet = 1, 
                  col_names = TRUE, col_types = c(rep("text", 3), "numeric", rep("text", 2))) %>%
  clean_names() %>%
  filter(state == "VA") %>%
  rename(description_urbinf = description) %>%
  select(-population_2010, -state)
urbinf$uic_2013 <- factor(urbinf$uic_2013, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), ordered = T,
                          labels = c("1 Metro", "2 Metro", "3 Nonmetro",
                                     "4 Nonmetro", "5 Nonmetro", "6 Nonmetro", 
                                     "7 Nonmetro", "8 Nonmetro", "9 Nonmetro",
                                     "10 Nonmetro", "11 Nonmetro", "12 Nonmetro"))
urbinf$county_name <- str_to_title(urbinf$county_name)

# 2013 Isserman: COUNTY LEVEL ------------------------------------------------------------------
isser <- read_xlsx("./data/original/isserman_rural_2013/isserman.xlsx", sheet = 1, 
                   col_names = TRUE, col_types = c(rep("text", 2))) %>%
  clean_names()
isser$countyname <- str_to_title(isser$countyname)
isser$isserman <- str_to_sentence(isser$isserman)
isser$isserman <- factor(isser$isserman, levels = c("Urban", "Mixed urban", "Mixed rural", "Rural"), ordered = T)

  
# 2020 Office of Rural Health strategic plan: COUNTY LEVEL ------------------------------------------------------------------
srhp <- read_csv("./data/original/srhp_rurality_2020/omb_srhp_rurality.csv", 
                col_names = TRUE, col_types = list(col_character(), col_factor(), col_factor())) %>%
  clean_names() %>%
  rename(srhprurality = rural_urban,
         ombrural = metropolitan_micropolitan_statistical_area)

srhp$srhprurality <- ifelse(srhp$srhprurality == "R", "rural", "urban")
srhp$srhprurality <- str_to_sentence(srhp$srhprurality)
srhp$ombrural <- str_to_sentence(srhp$ombrural)
srhp$ombrural <- factor(srhp$ombrural, levels = c("Metropolitan statistical area", "Micropolitan statistical area", "Non-metro"), ordered = T)
srhp$srhprurality <- factor(srhp$srhprurality, levels = c("Urban", "Rural"), ordered = T)


#
# GET GEOMETRIES ------------------------------------------------------------------------
#

# Get all FIPS codes
countyfips <- get(data("fips_codes")) %>% filter(state == "VA")
countyfips <- countyfips$county_code

# Get counties
va_counties <- get_acs(geography = "county", state = 51,
                   variables = "B01003_001",
                   year = 2018, survey = "acs5",
                   cache_table = TRUE, output = "wide", geometry = TRUE,
                   keep_geo_vars = TRUE)
va_counties <- va_counties %>% 
  select(-LSAD, -COUNTYNS, -AFFGEOID, -B01003_001M) %>%
  rename(pop_county = B01003_001E, 
         county_name = NAME.y, county_name_short = NAME.x, county_area = ALAND, county_water = AWATER)

# Get tracts
va_tracts <- get_acs(geography = "tract", state = 51, county = countyfips,
                       variables = "B01003_001",
                       year = 2018, survey = "acs5",
                       cache_table = TRUE, output = "wide", geometry = TRUE,
                       keep_geo_vars = TRUE)
va_tracts <- va_tracts %>% 
  select(-LSAD, -TRACTCE, -AFFGEOID) %>%
  rename(pop_tract = B01003_001E, pop_tract_moe = B01003_001M,
         tract_name = NAME.y, tract_name_short = NAME.x, tract_area = ALAND, tract_water = AWATER)

# Add county names to tract DF
countynames <- va_counties %>% select(STATEFP, COUNTYFP, county_name) %>% st_drop_geometry()
va_tracts <- left_join(va_tracts, countynames, by = c("STATEFP", "COUNTYFP"))


#
# JOIN ------------------------------------------------------------------------
#

# Check Bedford problem
# Bedford County is a United States county located in the Piedmont region of the Commonwealth of Virginia. Its county seat is the town of Bedford, which was an independent city from 1968 until rejoining the county in 2013.
# Bedford is an incorporated town and former independent city located within Bedford County in the U.S. state of Virginia.
setdiff(rucc$fips, va_counties$GEOID) # 51515 is in RUCC but not va_counties -- Bedford City
setdiff(irr$fips2010, va_counties$GEOID) # 51515 is in IRR but not va_counties -- Bedford City
setdiff(srhp$fips, va_counties$GEOID)
setdiff(isser$countyname, rucc$county_name) # Isserman has Clifton Forge City
setdiff(urbinf$county_name, rucc$county_name) 
setdiff(isser$countyname, nchs$county_name) 

test0 <- rucc %>% filter(fips == 51019 | fips == 51515) # Has both
test1 <- srhp %>% filter(fips == 51019 | fips == 51515) # Has county
test2 <- va_counties %>% filter(GEOID == 51019 | GEOID == 51515) # Has county
test3 <- irr %>% filter(fips2010 == 51019 | fips2010 == 51515) # Has both
test4 <- isser %>% filter(countyname == "Bedford County" | countyname == "Bedford City") # Has county twice but one is city

# Counties
va_county <- full_join(va_counties, rucc, by = c("GEOID" = "fips"))
va_county <- left_join(va_county, irr, by = c("GEOID" = "fips2010"))
va_county <- full_join(va_county, isser, by = c("county_name.y" = "countyname"))
va_county <- left_join(va_county, srhp, by = c("GEOID" = "fips"))
va_county <- left_join(va_county, urbinf, by = c("GEOID" = "fips", "county_name.y" = "county_name"))
va_county <- full_join(va_county, nchs, by = c("GEOID" = "fips_code", "county_name.y" = "county_name"))
# Take out Clifton Forge City - no geometry
va_county <- va_county %>% filter(county_name.y != "Clifton Forge City")

# Tracts
va_tract <- left_join(va_tracts, ruca, by = c("GEOID" = "tractfips"))


#
# CHECK RURALITY: County Level, Rural-Urban Continuum Codes ------------------------------------------------------------------------
#

# Rural-Urban Continuum Codes
# Metropolitan Counties (Metropolitan areas are based on the Office of Management and Budget (OMB) delineation as of February 2013.)
# 1	Counties in metro areas of 1 million population or more
# 2	Counties in metro areas of 250,000 to 1 million population
# 3	Counties in metro areas of fewer than 250,000 population

# Nonmetropolitan Counties	
# 4	Urban population of 20,000 or more, adjacent to a metro area
# 5	Urban population of 20,000 or more, not adjacent to a metro area
# 6	Urban population of 2,500 to 19,999, adjacent to a metro area
# 7	Urban population of 2,500 to 19,999, not adjacent to a metro area
# 8	Completely rural or less than 2,500 urban population, adjacent to a metro area
# 9	Completely rural or less than 2,500 urban population, not adjacent to a metro area

# Note: In Virginia, nonmetro independent cities were combined with their counties of origin when computing the Rural-urban continuum Codes.  The following combinations were made: 
# FIPS 	City	              FIPS  County
# 51580	Covington City    	51005	Alleghany County
# 51640	Galax City    	    51035	Carroll County
# 51595	Emporia City  	    51081	Greensville County
# 51690	Martinsville City  	51089	Henry County
# 51590	Danville City       51143	Pittsylvania County
# 51530	Buena Vista City   	51163	Rockbridge County
# 51678	Lexington City      51163	Rockbridge County
# 51620	Franklin City     	51175	Southhampton County

# Distribution
table(va_county$rucc_2013, useNA = "always")

# Plot
pal_rucc <- colorFactor("YlGn", domain = va_county$rucc_2013)

labels_rucc_irr_isser <- lapply(
  paste("<strong>Area: </strong>",
        va_county$county_name.y,
        "<br />",
        "<strong>RUCC 2013: </strong>",
        va_county$rucc_2013,
        "<br />",
        "<strong>RUCC 2013 description: </strong>",
        "<br />",
        va_county$description_rucc,
        "<br />",
        "<strong>Urban influence 2013: </strong>",
        va_county$uic_2013,
        "<br />",
        "<strong>Urban influence description: </strong>",
        "<br />",
        va_county$description_urbinf,
        "<br />",
        "<strong>IRR 2010: </strong>",
        round(va_county$irr2010, 2),
        "<br />",
        "<strong>Isserman 2013: </strong>",
        va_county$isserman,
        "<br />",
        "<strong>VDH ORH 2020: </strong>",
        va_county$srhprurality,
        "<br />",
        "<strong>OMB 2013: </strong>",
        va_county$ombrural
        ),
  htmltools::HTML
)

leaflet(data = va_county)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(fillColor = ~pal_rucc(rucc_2013), 
              fillOpacity = 0.7, 
              stroke = TRUE, weight = 0.5, color = "#202020",
              label = labels_rucc_irr_isser,
              labelOptions = labelOptions(direction = "bottom",
                                          style = list(
                                            "font-size" = "12px",
                                            "border-color" = "rgba(0,0,0,0.5)",
                                            direction = "auto"
                                          )))  %>%
  addLegend("bottomleft",
            pal = pal_rucc,
            values =  ~(rucc_2013),
            title = "RUCC",
            opacity = 0.7)


#
# CHECK RURALITY: County Level, State rural health plan ------------------------------------------------------------------------
#

table(va_county$srhprurality) # The difference in 1 is Bedford city (which is urban)

# Plot
pal_srhp <- colorFactor("YlGn", domain = va_county$srhprurality)

leaflet(data = va_county)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(fillColor = ~pal_srhp(srhprurality), 
              fillOpacity = 0.7, 
              stroke = TRUE, weight = 0.5, color = "#202020",
              label = labels_rucc_irr_isser,
              labelOptions = labelOptions(direction = "bottom",
                                          style = list(
                                            "font-size" = "12px",
                                            "border-color" = "rgba(0,0,0,0.5)",
                                            direction = "auto"
                                          )))  %>%
  addLegend("bottomleft",
            pal = pal_srhp,
            values =  ~(srhprurality),
            title = "SRHP 2020",
            opacity = 0.7)


#
# CHECK RURALITY: County Level, OMB ------------------------------------------------------------------------
#

table(va_county$ombrural)

# Plot
pal_omb <- colorFactor("YlGn", domain = va_county$ombrural)

leaflet(data = va_county)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(fillColor = ~pal_omb(ombrural), 
              fillOpacity = 0.7, 
              stroke = TRUE, weight = 0.5, color = "#202020",
              label = labels_rucc_irr_isser,
              labelOptions = labelOptions(direction = "bottom",
                                          style = list(
                                            "font-size" = "12px",
                                            "border-color" = "rgba(0,0,0,0.5)",
                                            direction = "auto"
                                          )))  %>%
  addLegend("bottomleft",
            pal = pal_omb,
            values =  ~(ombrural),
            title = "OMB 2013",
            opacity = 0.7)


#
# CHECK RURALITY: County Level, Index of Relative Rurality ------------------------------------------------------------------------
#

hist(va_county$irr2010)

# Plot
pal_irr <- colorBin("YlGn", domain = va_county$irr2010, bins = 6)

leaflet(data = va_county)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(fillColor = ~pal_irr(irr2010), 
              fillOpacity = 0.7, 
              stroke = TRUE, weight = 0.5, color = "#202020",
              label = labels_rucc_irr_isser,
              labelOptions = labelOptions(direction = "bottom",
                                          style = list(
                                            "font-size" = "12px",
                                            "border-color" = "rgba(0,0,0,0.5)",
                                            direction = "auto"
                                          )))  %>%
  addLegend("bottomleft",
            pal = pal_irr,
            values =  ~(irr2010),
            title = "IRR",
            opacity = 0.7)

#
# CHECK RURALITY: County Level, Isserman ------------------------------------------------------------------------
#

table(va_county$isserman)

# Plot
pal_isser <- colorFactor("YlGn", domain = va_county$isserman)

leaflet(data = va_county)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(fillColor = ~pal_isser(isserman), 
              fillOpacity = 0.7, 
              stroke = TRUE, weight = 0.5, color = "#202020",
              label = labels_rucc_irr_isser,
              labelOptions = labelOptions(direction = "bottom",
                                          style = list(
                                            "font-size" = "12px",
                                            "border-color" = "rgba(0,0,0,0.5)",
                                            direction = "auto"
                                          )))  %>%
  addLegend("bottomleft",
            pal = pal_isser,
            values =  ~(isserman),
            title = "Isserman",
            opacity = 0.7)


#
# CHECK RURALITY: Tract Level, Rural-Urban Commuting Area Codes ------------------------------------------------------------------------
#

# 1    Metropolitan area core: primary flow within an urbanized area (UA)
# 2    Metropolitan area high commuting: primary flow 30% or more to a UA
# 3    Metropolitan area low commuting: primary flow 10% to 30% to a UA
# 4    Micropolitan area core: primary flow within an Urban Cluster of 10,000 to 49,999 (large UC)
# 5    Micropolitan high commuting: primary flow 30% or more to a large UC
# 6    Micropolitan low commuting: primary flow 10% to 30% to a large UC
# 7    Small town core: primary flow within an Urban Cluster of 2,500 to 9,999 (small UC)
# 8    Small town high commuting: primary flow 30% or more to a small UC
# 9    Small town low commuting: primary flow 10% to 30% to a small UC
# 10   Rural areas: primary flow to a tract outside a UA or UC
# 99   Not coded: Census tract has zero population and no rural-urban identifier information

table(va_tract$primary_ruca_code_2010, useNA = "always")

pal_ruca <- colorNumeric("YlGn", domain = va_tract$primary_ruca_code_2010)

labels_ruca <- lapply(
  paste("<strong>Area: </strong>",
        va_tract$county_name,
        "<br />",
        "<strong>RUCA 2010: </strong>",
        va_tract$primary_ruca_code_2010),
  htmltools::HTML
)

leaflet(data = va_tract)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(fillColor = ~pal_ruca(primary_ruca_code_2010), 
              fillOpacity = 0.7, 
              stroke = TRUE, weight = 0.5, color = "#202020",
              label = labels_ruca,
              labelOptions = labelOptions(direction = "bottom",
                                          style = list(
                                            "font-size" = "12px",
                                            "border-color" = "rgba(0,0,0,0.5)",
                                            direction = "auto"
                                          )))  %>%
  addLegend("bottomleft",
            pal = pal_ruca,
            values =  ~(primary_ruca_code_2010),
            title = "RUCA",
            opacity = 0.7)


#
# CHECK COUNTY RURALITY: ALL ------------------------------------------------------------------------
#

# isserman: rural, mixed rural
# irr2010: > 0.5
# rucc_2013:  Nonmetropolitan Counties	
# 4	Urban population of 20,000 or more, adjacent to a metro area
# 5	Urban population of 20,000 or more, not adjacent to a metro area
# 6	Urban population of 2,500 to 19,999, adjacent to a metro area
# 7	Urban population of 2,500 to 19,999, not adjacent to a metro area
# 8	Completely rural or less than 2,500 urban population, adjacent to a metro area
# 9	Completely rural or less than 2,500 urban population, not adjacent to a metro area)

# va_county <- va_county %>% mutate(score_rucc = ifelse(rucc_2013 == "9 Nonmetro" | rucc_2013 == "8 Nonmetro", 1, 0),
#                                   score_isserman = ifelse(isserman == "Rural" | isserman == "Mixed rural", 1, 0),
#                                   score_irr = ifelse(irr2010 > 0.5, 1, 0),
#                                   score_omb = ifelse(ombrural == "Non-metro" | ombrural == "Micropolitan statistical area", 1, 0),
#                                   score_srhp = ifelse(srhprurality == "Rural", 1, 0),
#                                   score_urbinf = ifelse(uic_2013 != "1 Metro" & uic_2013 != "2 Metro", 1, 0),
#                                   score = score_rucc + score_isserman + score_irr + score_omb + score_srhp + score_urbinf)
# 
# # Check which counties are rural according to X definitions out of 6
# table(va_county$score)


#
# WRITE OUT FOR APP ------------------------------------------------------------------------
#

va_county <- st_transform(va_county, 4269)
write_rds(va_county, "./ruralapp/ruralapp.rds")
