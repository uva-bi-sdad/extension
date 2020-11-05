library(RPostgreSQL)
library(dplyr)
library(readr)
library(sf)
library(naniar)


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


#
# Sanity check with land use code: WITH COMMERCIAL CONDOS INCLUDED ------------------------
#

table(data$land_use_code, useNA = "always")
# 100     102     106     109     111     112     115     116     117     131     132     133     135     137     138     151     160 
# 143782  258036    8272     754    1984  179471   19766    3429   30088     113    1368   11530     261   48387    8116    1161     160 
# 163     165     167     206     213     238     247    <NA> 
#   2525193     423      68     626    1971     353    5723       0 

unique(data$land_use_code)
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

test <- data %>% filter(land_use_code == 238)
test <- data %>% filter(land_use_code == 213)


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



