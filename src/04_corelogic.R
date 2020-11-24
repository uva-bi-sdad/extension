library(RPostgreSQL)
library(dplyr)
library(readr)
library(sf)
library(naniar)
library(leaflet)


#
# Read in current list of "rural" counties ---------------------------------------------------------------------------------------------
#

ruralcty <- read_csv("./data/original/srhp_rurality_2020/omb_srhp_rurality.csv", 
                     col_types = list(col_character(), col_character(), col_character()))
ruralcty <- ruralcty %>% filter(RuralUrban == "R") %>% select(-Metropolitan_MicropolitanStatisticalArea)


#
# Connect to DB ---------------------------------------------------------------------------------------------
#

conn <- dbConnect(drv = PostgreSQL(), dbname = "sdad", 
                  host = "10.250.124.195", 
                  port = 5432, 
                  user = Sys.getenv(x = "db_userid"), 
                  password = Sys.getenv(x = "db_pwd"))


#
# Get Virginia data from the available tables-----------------------------------------------------------
#

# Most info: Get data from _2_51 files ("latest tax data + property characteristics" files split by state, 51 is VA) 
# Data from _1_51, 01-09 files is not geocoded.
# 3901300 observations
data <- dbGetQuery(conn, "SELECT apn__parcel_number_unformatted_, apn_sequence_number, composite_property_linkage_key, original_apn,
                                 online_formatted_parcel_id, previous_parcel_number, previous_parcel_sequence_number, previous_parcel_number_formatted,
                                 tax_year, assessed_year, year_built, effective_year_built, 
                                 parcel_level_latitude, parcel_level_longitude, block_level_latitude, block_level_longitude, fips_code, census_tract, 
                                 township, municipality_code, situs_house_number, situs_street_name, situs_mode, situs_city, situs_state, situs_zip_code,
                                 property_indicator_code, land_use_code, owner_1_corporate_indicator, county_use_description 
                          FROM corelogic_sdad.tax_hist_2_51
                          WHERE state_fips = '51'")


#
# Disconnect ---------------------------------------------------------------------------------------------
#

dbDisconnect(conn)
rm(conn)


#
# Check contents ---------------------------------------------------------------------------------------------
#

names(data)

# Year - tax
table(data$tax_year, useNA = "always")          
table(data$assessed_year, useNA = "always") 

# Year - build
table(data$year_built, useNA = "always")    
table(data$effective_year_built, useNA = "always") 

# Geo identifiers
# mostly empty: block_level_latitude, block_level_longitude, 
# more usable: parcel_level_latitude, parcel_level_longitude, census_tract

pct_complete_var(data) # 17.24
pct_miss_var(data) # 82.75
n_var_complete(data) # 5 variables complete
n_var_miss(data) # 24 have missingness
miss_var_summary(data)

# variable                          n_miss pct_miss
# 1 township                         3901272    100. 
# 2 previous_parcel_number           3884312     99.6
# 3 previous_parcel_sequence_number  3884312     99.6
# 4 owner_1_corporate_indicator      3351372     85.9
# 5 previous_parcel_number_formatted 3282680     84.1
# 6 effective_year_built             3257588     83.5
# 7 year_built                       1250692     32.1
# 8 municipality_code                 792860     20.3
# 9 situs_house_number                756077     19.4
# 10 situs_mode                        574057     14.7


#
# Residential only ---------------------------------------------------------------------------------------------
#

# CoreLogic property indicator code (see codebook)
# 0	MISCELLANEOUS
# 10	SINGLE FAMILY RESIDENCE
# 11	CONDOMINIUM
# 20	COMMERCIAL
# 21	DUPLEX
# 22	APARTMENT
# 23	HOTEL, MOTEL
# 24	COMMERCIAL CONDOMINIUM
# 25	RETAIL
# 26	SERVICE
# 27	OFFICE BUILDING
# 28	WAREHOUSE
# 29	FINANCIAL INSTITUTION
# 30	HOSPITAL
# 31	PARKING
# 32	AMUSEMENT-RECREATION
# 50	INDUSTRIAL
# 51	INDUSTRIAL LIGHT
# 52	INDUSTRIAL HEAVY
# 53	TRANSPORT
# 54	UTILITIES
# 70	AGRICULTURAL
# 80	VACANT
# 90	EXEMPT

data$property_indicator_code <- as.factor(data$property_indicator_code)

# Residential: 10 (Single Family Residence), 11 (Condominium), 21 (Duplex), 22 (Apartment)
# Considered 24 (Commercial Condominium) but cross-checking with land use code, county use code, and googling property addresses
# showed these were in fact all medical offices, business offices, etc. Excluding here.
data <- data %>% filter(property_indicator_code == 10 | # single family residence
                        property_indicator_code == 11 | # condo
                        property_indicator_code == 21 | # duplex
                        property_indicator_code == 22)  # apartment
# Data now has 3242362 rows (dropped 3901300-3242362=658938)

# Sanity check with county use descriptions
# Mostly residential, but also examples of agriculture, medical office, vacant, campground,... 
table(data$county_use_description, useNA = "always")
unique(data$county_use_description)


#
# Sanity check with land use code: WITH COMMERCIAL CONDOS INCLUDED ------------------------
#

# table(data$land_use_code, useNA = "always")
# 100     102     106     109     111     112     115     116     117     131     132     133     135     137     138     151     160 
# 143782  258036    8272     754    1984  179471   19766    3429   30088     113    1368   11530     261   48387    8116    1161     160 
# 163     165     167     206     213     238     247    <NA> 
#   2525193     423      68     626    1971     353    5723       0 

# unique(data$land_use_code)
# "163" "100" "112" "106" "116" "117" "137" "247" "102" "115" "138" "213" "132" "133" "109" "111" "165" "238" "151" "160" "131" "167"
# "135" "206"

# 163	SFR
# 100	Residential (NEC)
# 112	Condominium
# 106	Apartment
# 116	Mid rise condo
# 117	High rise condo
# 137	Mobile home
# 247	Office condo
# 102	Townhouse/rowhouse
# 115	Duplex
# 138	Manufactured home
# 213	Commercial condominium
# 132	Multi family 10 units less
# 133	Multi family dwelling
# 109	Cabin
# 111	Cooperative
# 165	Triplex
# 238	Medical condo
# 151	Quadruplex
# 160	Rural homesite
# 131	Multi family 10 units plus
# 167	Time share
# 135	Mobile home lot
# 206	Condotel

# From here, medical condo (238), condotel (206),  office condo (247), commercial condo (213) are suspicious

# test <- data %>% filter(land_use_code == 238)
# test <- data %>% filter(land_use_code == 213)


#
# Sanity check with land use code: WITHOUT COMMERCIAL CONDOS INCLUDED -------------------------
#

table(data$land_use_code, useNA = "always")
# 100     102     106     109     111     112     115     116     117     131     132     133     135     137     138     151     160 
# 143782  258036    8272     754    1984  179471   19766    3429   30088     113    1368   11530     261   48387    8116    1161     160 
# 163     165     167    <NA> 
# 2525193     423      68       0 

unique(data$land_use_code)
# "163" "100" "112" "106" "116" "117" "137" "102" "115" "138" "132" "133" "109" "111" "165" "151" "160" "131" "167" "135"

# 163	SFR
# 100	Residential (NEC)
# 112	Condominium
# 106	Apartment
# 116	Mid rise condo
# 117	High rise condo
# 137	Mobile home
# 102	Townhouse/rowhouse
# 115	Duplex
# 138	Manufactured home
# 132	Multi family 10 units less
# 133	Multi family dwelling
# 109	Cabin
# 111	Cooperative
# 165	Triplex
# 151	Quadruplex
# 160	Rural homesite
# 131	Multi family 10 units plus
# 167	Time share
# 135	Mobile home lot

# Codes that were filtered out: # 247	Office condo, # 213	Commercial condominium, # 238	Medical condo, # 206	Condotel


#
# Uniqueness --------------------------------------------------------------------
#

# Year
table(data$tax_year)
table(data$assessed_year)

# IDs: Only the composite property linkage key is unique.
any(duplicated(data$composite_property_linkage_key)) # Unique property key, which can be used to link to other files associated with property.
any(duplicated(data$apn__parcel_number_unformatted_)) # Assessor's Parcel Number in an unformatted form. This is most often used by the county and others as a unique key.
any(duplicated(data$apn_sequence_number)) # This internal sequence number is used to ensure uniqueness of the Assessor's Parcel Number.
any(duplicated(data$original_apn)) # Original Assessors Parcel Number (APN) in raw format provided on the assessor tax roll.
any(duplicated(data$online_formatted_parcel_id)) # APN that would link to CoreLogic online products.

# Coords
# Parcel level: CoreLogic proprietary parcel-centroid coordinate that specifies the north-south / east-west position of the center point of a parcel. 
# Block level: CoreLogic derived geographic coordinate that specifies the north-south / east-west position of a point based on United States Postal Service address data for the parcel. 
any(is.na(data$parcel_level_latitude))            
any(is.na(data$parcel_level_longitude))
any(is.na(data$block_level_latitude))
any(is.na(data$block_level_longitude))

any(duplicated(data$parcel_level_latitude))            
any(duplicated(data$parcel_level_longitude))
any(duplicated(data$block_level_latitude))
any(duplicated(data$block_level_longitude))

data$latlong_parcel <- paste0(data$parcel_level_latitude, ", ", data$parcel_level_longitude)
data$latlong_block <- paste0(data$block_level_latitude, ", ", data$parcel_level_longitude)

any(duplicated(data$latlong_parcel))    
any(duplicated(data$latlong_block))    

test1 <- data[duplicated(data$latlong_parcel), ]
test2 <- data[duplicated(data$latlong_block), ]

pct_complete_case(data) # 0
pct_complete_var(data) # 26.7
pct_miss_var(data) # 73.3

n_var_complete(data) # 8 variables complete
n_var_miss(data) # 22 have missingness
print(miss_var_summary(data), n = Inf)

# 11 block_level_latitude              294981    9.10 
# 12 block_level_longitude             294981    9.10 
# 17 parcel_level_latitude              75079    2.32 
# 18 parcel_level_longitude             75079    2.32 


#
# Check for rural counties (per RSHP/OMB only) --------------------------------------------------------------------
#

ruraldata <- data %>% filter(fips_code %in% ruralcty$FIPS)

n_var_complete(ruraldata) # 8 variables complete
n_var_miss(ruraldata) # 22 have missingness
print(miss_var_summary(ruraldata), n = Inf)

#  9 block_level_latitude             201533  26.5   
# 10 block_level_longitude            201533  26.5   
# 16 parcel_level_latitude             31519   4.14  
# 17 parcel_level_longitude            31519   4.14  

# No data: 12,461. >93% of these also missing one or more address fields, so this would be impossible to code.
test_allmiss <- ruraldata %>% filter(is.na(block_level_latitude) & is.na(block_level_longitude) &
                                     is.na(parcel_level_latitude) & is.na(parcel_level_longitude))
print(miss_var_summary(test_allmiss), n = Inf)

# Cases with no data appear in 51 different counties out of 55. Counties with over 100 properties with missing coordinates are below.
table(test_allmiss$fips_code)
print(test_allmiss %>% 
        select(fips_code) %>% group_by(fips_code) %>% 
        summarize(n = n()) %>% arrange(desc(n)), n = Inf)

#  1 51147      1307 Prince Edward County
#  2 51105      1279 Lee County
#  3 51051      1156 Dickenson County
#  4 51163       735 Rockbridge County
#  5 51197       642 Wythe County
#  6 51027       641 Buchanan County
#  7 51143       640 Pittsylvania County
#  8 51193       608 Westmoreland County
#  9 51025       512 Brunswick County
# 10 51083       489 Halifax County
# 11 51037       487 Charlotte County
# 12 51185       458 Tazewell County
# 13 51019       316 Bedford County
# 14 51001       291 Accomack County
# 15 51117       257 Mecklenburg County
# 16 51131       223 Northampton County
# 17 51103       221 Lancaster County
# 18 51035       209 Carroll County
# 19 51195       199 Wise County
# 20 51135       187 Nottoway County
# 21 51141       178 Patrick County
# 22 51017       153 Bath County
# 23 51049       140 Cumberland County
# 24 51159       106 Richmond County
# 25 51021       104 Bland County
# 26 51111       102 Lunenburg County

# Missing parcel but have block: 19,058. These have parts of the address missing. Could maybe piece something together to geocode.
test_parcelmiss <- ruraldata %>% filter(!is.na(block_level_latitude) & !is.na(block_level_longitude) &
                                       is.na(parcel_level_latitude) & is.na(parcel_level_longitude))
print(miss_var_summary(test_parcelmiss), n = Inf) 


#
# Prepare df --------------------------------------------------------------------
#

# 760,573 properties in rural counties.
# Of these, 12,461 (1.64%) have no location data (block or parcel level coordinates).
# Filter to observations that have at least one set of coordinates. 
# There are 748,112 rows with at least one set of coordinates.

ruraldata_filtered <- ruraldata %>% filter((!is.na(block_level_latitude) & !is.na(block_level_longitude)) | 
                                           (!is.na(parcel_level_latitude) & !is.na(parcel_level_longitude)))

# Coordinates are always missing as a set (all parcel, all block).
# More rows are missing block level coordinates than parcel level coordinates.
print(miss_var_summary(ruraldata_filtered), n = Inf) 
# 189,072 rows (25.3%) are missing block level coordinates.
# 19,058 rows (2.55%) are missing parcel level coordinates.

# Use parcel level coordinates. Where parcel level coordinates are unavailable, use block level coordinates.
ruraldata_filtered <- ruraldata_filtered %>% 
                        mutate(latitude = case_when(
                                !is.na(parcel_level_latitude) & !is.na(parcel_level_longitude) ~ parcel_level_latitude,
                                 is.na(parcel_level_latitude) & is.na(parcel_level_longitude) ~ block_level_latitude),
                               longitude = case_when(
                                 !is.na(parcel_level_latitude) & !is.na(parcel_level_longitude) ~ parcel_level_longitude,
                                 is.na(parcel_level_latitude) & is.na(parcel_level_longitude) ~ block_level_longitude)
                               )

any(is.na(ruraldata_filtered$longitude))
any(is.na(ruraldata_filtered$latitude))

# In sum:
# Of 760,573 properties in rural counties, 12,461 (1.64%) have no location data (block or parcel level coordinates). 
# We drop these observations.
# Of the remaining 748,112 rows with at least one set of coordinates, we use parcel level coordinates for the 
# 729,054 properties (97.45%) where these coordinates are available. 
# For the remaining 19,058 rows (2.55%) that are missing parcel level coordinates, we use block level coordinates.

# Check the characteristics of properties that don't have parcel level coordinates.
noparcel <- ruraldata_filtered %>% filter(is.na(parcel_level_latitude) & is.na(parcel_level_longitude))

# Missingness is in all 55 counties. 
table(noparcel$fips_code)
print(noparcel %>% 
        select(fips_code) %>% group_by(fips_code) %>% 
        summarize(n = n()) %>% arrange(desc(n)), n = Inf)

#  1 51147      4749 Prince Edward County
#  2 51027      3774 Buchanan County
#  3 51105      1425 Lee County
#  4 51077       930 Grayson County
#  5 51163       645 Rockbridge County
#  6 51620       615 Franklin City
#  7 51193       505 Westmoreland County
#  8 51185       499 Tazewell County
#  9 51025       492 Brunswick County
# 10 51051       485 Dickenson County
# 11 51015       427 Augusta County
# 12 51678       340 Lexington City
# 13 51017       339 Bath County
# 14 51143       283 Pittsylvania County
# 15 51195       274 Wise County
# 16 51117       232 Mecklenburg County
# 17 51037       224 Charlotte County
# 18 51057       211 Essex County
# 19 51197       209 Wythe County
# 20 51119       200 Middlesex County
 
#
# See what this looks like -------------------------------------------------
#

# A couple are in weird spots/look outside of the rural counties. 
testplot <- ruraldata_filtered %>% select(apn__parcel_number_unformatted_, fips_code, longitude, latitude)
testplot <- st_as_sf(testplot, coords = c("longitude", "latitude"))
plot(st_geometry(testplot), pch = 20)

# Recode obvious errors: Find IDs
labels <- lapply(
  paste("<strong>Unique ID: </strong>",
        testplot$apn__parcel_number_unformatted_
        ),
  htmltools::HTML
)

leaflet(data = testplot[testplot$fips_code == 51005, ], options = leafletOptions())%>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(color = "#202020", radius = 0.5,
              label = labels,
              labelOptions = labelOptions(direction = "bottom",
                                          style = list(
                                            "font-size" = "12px",
                                            "border-color" = "rgba(0,0,0,0.5)",
                                            direction = "auto"
                                          ))) 

# 22 A 7
# 9-5E
# 0090000000041E
# 45-7-63
# 55 17
# 3B 4 27
# 12 5 23
# 068 A 30 A
# 071 A 7 A
# 14-A-19A
# 96-A-31
# 57 (A) 15B
# 57 (A) 58A
# 32 A 22F
# 050 A 26 A
# 079 A 8 A
# 059 3 1 C
# 14-A-44-A
# 04600020000000001A
# 90510195
# 36 17
# 14237


#
# Write out -------------------------------------------------
#

data_writeout <- st_as_sf(ruraldata_filtered, coords = c("longitude", "latitude"))

st_crs(data_writeout) <- 4326
data_writeout <- st_transform(data_writeout, 4326)

write_rds(data_writeout, "./data/working/corelogic/final_corelogic.rds")





