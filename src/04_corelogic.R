library(RPostgreSQL)
library(dplyr)
library(readr)
library(sf)


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
                                 tax_year, assessed_year, parcel_level_latitude, parcel_level_longitude, census_tract, 
                                 land_use_code, owner_1_corporate_indicator, county_use_description 
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

# Year
table(data$tax_year, useNA = "always")          # 2019
table(data$assessed_year, useNA = "always")     # 2019

# Geo identifiers
# empty: block_level_latitude, block_level_longitude, 
# usable: parcel_level_latitude, parcel_level_longitude, census_tract

# Other information not NA:
# land_use_code, county_use_description, property_indicator_code
# owner_1_full_name, owner_1_last_name, owner_1_corporate_indicator, 
# total_value_calculated


#
# Residential only ---------------------------------------------------------------------------------------------
#

table(data$county_use_description, useNA = "always")
unique(data$county_use_description)

table(data$owner_1_corporate_indicator, useNA = "always")
#      Y     <NA> 
# 549928 3351372 

table(data$land_use_code, useNA = "always")





