library(dplyr)
library(tigris)
library(sf)
library(readxl)
library(janitor)
library(stringr)

# Get VA geographies at county and tract level to examine rurality. 


#
# READ IN RURALITY CODES ------------------------------------------------------------------------
#

# 2010 Index of Relative Rurality ------------------------------------------------------------------
irr <- read_xlsx("./data/original/purdue_irr_2010/IRR_2000_2010.xlsx", sheet = 2, range = "A1:C3142",
                 col_names = TRUE, col_types = c("text", "text", "numeric")) %>%
       clean_names()

irr <- irr %>% filter(str_detect(fips2010, "^51") & nchar(fips2010) == 5)


# Note: counties deleted due to lack of data are 15901 and 51560.

# 2010 Rural-Urban Commuting Area Codes ------------------------------------------------------------------
ruca <- read_xlsx("./data/original/usda_ruca_2010/ruca2010revised.xlsx", sheet = 1, skip = 1,
                  col_names = TRUE, col_types = c(rep("text", 4), rep("numeric", 5))) %>%
        clean_names()

ruca <- ruca %>% select(1:6)
ruca <- ruca %>% filter(select_state == "VA")

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

# 2013 Rural-Urban Continuum Codes ------------------------------------------------------------------
rucc <- read_xls("./data/original/usda_rucc_2013/ruralurbancodes2013.xls", sheet = 1, 
                 col_names = TRUE, col_types = c(rep("text", 3), rep("numeric", 2), "text")) %>%
        clean_names()

rucc <- rucc %>% select(-population_2010)
rucc <- rucc %>% filter(state == "VA")

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


#
# GET GEOMETRIES ------------------------------------------------------------------------
#

# Get all FIPS codes
countyfips <- get(data("fips_codes")) %>% filter(state == "VA")
countyfips <- countyfips$county_code

va_counties <- counties(state = 51, cb = FALSE, year = 2018, class = "sf")
#va_tracts <- tracts(state = 51, county = countyfips, cb = FALSE, year = 2018, class = "sf")


