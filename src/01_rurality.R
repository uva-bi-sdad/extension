library(dplyr)
library(tigris)
library(sf)
library(readxl)
library(janitor)
library(stringr)
library(leaflet)


# Get VA geographies at county and tract level to examine rurality. 


#
# READ IN RURALITY CODES ------------------------------------------------------------------------
#

# 2010 Index of Relative Rurality: COUNTY LEVEL ------------------------------------------------------------------
irr <- read_xlsx("./data/original/purdue_irr_2010/IRR_2000_2010.xlsx", sheet = 2, range = "A1:C3142",
                 col_names = TRUE, col_types = c("text", "text", "numeric")) %>%
       clean_names()
irr <- irr %>% rename(county_state_name = county_name)

irr <- irr %>% filter(str_detect(fips2010, "^51") & nchar(fips2010) == 5)
# Note: counties deleted due to lack of data are 15901 and 51560.

# 2013 Rural-Urban Continuum Codes: COUNTY LEVEL ------------------------------------------------------------------
rucc <- read_xls("./data/original/usda_rucc_2013/ruralurbancodes2013.xls", sheet = 1, 
                 col_names = TRUE, col_types = c(rep("text", 3), rep("numeric", 2), "text")) %>%
        clean_names()
rucc$rucc_2013 <- as.factor(rucc$rucc_2013)

rucc <- rucc %>% select(-population_2010)
rucc <- rucc %>% filter(state == "VA")

# 2010 Rural-Urban Commuting Area Codes: TRACT LEVEL ------------------------------------------------------------------
ruca <- read_xlsx("./data/original/usda_ruca_2010/ruca2010revised.xlsx", sheet = 1, skip = 1,
                  col_names = TRUE, col_types = c(rep("text", 4), rep("numeric", 5))) %>%
  clean_names()
ruca <- ruca %>% rename(tractfips = state_county_tract_fips_code_lookup_by_address_at_http_www_ffiec_gov_geocode)
ruca$primary_ruca_code_2010 <- ifelse(ruca$primary_ruca_code_2010 == 99, NA, ruca$primary_ruca_code_2010)

ruca <- ruca %>% select(1:6)
ruca <- ruca %>% filter(select_state == "VA")


#
# GET GEOMETRIES ------------------------------------------------------------------------
#

# Get all FIPS codes
countyfips <- get(data("fips_codes")) %>% filter(state == "VA")
countyfips <- countyfips$county_code

# Counties
va_counties <- counties(state = 51, cb = FALSE, year = 2018, class = "sf")
va_counties <- va_counties %>% select(STATEFP, COUNTYFP, GEOID, NAME, NAMELSAD, ALAND, AWATER)
va_counties <- va_counties %>% rename(county_name = NAMELSAD, county_area = ALAND, county_water = AWATER)

# Tracts
va_tracts <- tracts(state = 51, county = countyfips, cb = FALSE, year = 2018, class = "sf")
va_tracts <- va_tracts %>% select(STATEFP, COUNTYFP, TRACTCE, GEOID, NAME, NAMELSAD, ALAND, AWATER)
va_tracts <- va_tracts %>% rename(tract_name = NAMELSAD, tract_area = ALAND, tract_water = AWATER)

# Add county names to tract DF
countynames <- va_counties %>% select(STATEFP, COUNTYFP, county_name) %>% st_drop_geometry()
va_tracts <- left_join(va_tracts, countynames, by = c("STATEFP", "COUNTYFP"))


#
# JOIN ------------------------------------------------------------------------
#

# Check
setdiff(rucc$fips, va_counties$GEOID) # 51515 is in RUCC but not va_counties -- Bedford City
setdiff(irr$fips2010, va_counties$GEOID) # 51515 is in IRR but not va_counties -- Bedford City

# Counties
va_county <- left_join(rucc, va_counties, by = c("fips" = "GEOID", "county_name"))
va_county <- left_join(va_county, irr, by = c("fips" = "fips2010"))
va_county <- st_as_sf(va_county)

# Tracts
va_tract <- left_join(va_tracts, ruca, by = c("GEOID" = "tractfips", "county_name" = "select_county"))

# Check
# sum(is.na(ruca$primary_ruca_code_2010)) 0
# sum(is.na(va_tract$primary_ruca_code_2010)) 1
test <- va_tract %>% filter(is.na(primary_ruca_code_2010)) # Bedford County
# Bedford County is a United States county located in the Piedmont region of the Commonwealth of Virginia. Its county seat is the town of Bedford, which was an independent city from 1968 until rejoining the county in 2013.
# Bedford is an incorporated town and former independent city located within Bedford County in the U.S. state of Virginia.


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

labels_rucc_irr <- lapply(
  paste("<strong>Area: </strong>",
        va_county$county_name,
        "<br />",
        "<strong>RUCC 2013: </strong>",
        va_county$rucc_2013,
        "<br />",
        "<strong>RUCC 2013 description: </strong>",
        "<br />",
        va_county$description,
        "<br />",
        "<strong>IRR 2010: </strong>",
        round(va_county$irr2010, 2)),
  htmltools::HTML
)

leaflet(data = va_county)%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(fillColor = ~pal_rucc(rucc_2013), 
              fillOpacity = 0.7, 
              stroke = TRUE, weight = 0.5, color = "#202020",
              label = labels_rucc_irr,
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
              label = labels_rucc_irr,
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