library(readr)
library(dplyr)
library(sf)
library(stringr)
library(osrm)
library(lwgeom)
library(purrr)
library(tidycensus)


#
# Read in --------------------------------------------------------------------------------
#

# Final geocoded data
data_geo_1 <- read_rds("./data/working/foodretail/foodretail_nonmiss.rds")
data_geo_2 <- read_rds("./data/working/foodretail/foodretail_missing_coded.rds")


#
# Fix geolocation --------------------------------------------------------------------------------
#

# 40 entries have a location geocoded that's outside the county they're supposedly in. Geocoding was wrong either originally or
# on our attempted geocoding (opencage, which was the last resort)

filterthedata <- function(whichdata) {
  whichdata %>% filter(  (str_detect(business, "Oyster") & county == "Accomack" & str_detect(address1, "13348 Full Measure")) |
                         (str_detect(business, "Mill") & county == "Augusta" & str_detect(address1, "140 Main St")) |
                         (str_detect(business, "Super Save") & county == "Augusta" & str_detect(address1, "51 Franklin St")) |
                         (str_detect(business, "Springdale Water") & county == "Augusta" & str_detect(address1, "340 Old Quarry")) |
                         (str_detect(business, "Lake") & county == "Bath" & str_detect(address1, "8446 Bolars")) |
                         (str_detect(business, "Travel Plaza") & county == "Bland" & str_detect(address1, "264 Arrowhead")) |
                         (str_detect(business, "BP") & county == "Bland" & str_detect(address1, "254 Arrowhead")) |
                         (str_detect(business, "Beverage Control") & county == "Buchanan" & str_detect(address1, "1 Plaza Dr")) |
                         (str_detect(business, "Mini Mart") & county == "Buchanan" & str_detect(address1, "US Route 460")) |
                         (str_detect(business, "Ramey") & county == "Buchanan" & str_detect(address1, "Highway 460")) |
                         (str_detect(business, "Exxon Distributor") & county == "Buchanan" & str_detect(address1, "1403 Three River")) |
                         (str_detect(business, "Old 26") & county == "Charlotte" & str_detect(address1, "9641 RR")) |
                         (str_detect(business, "Duffey") & county == "Charlotte" & str_detect(address1, "2459 Patrick")) |
                         (str_detect(business, "Sweet Frog") & county == "Danville" & str_detect(address1, "165 Holt")) |                        
                         (str_detect(business, "Danville Quality Plus") & county == "Danville" & str_detect(address1, "1090 Franklin Tpke")) |   
                         (str_detect(business, "Town & Country Market") & county == "Greensville" & str_detect(address1, "PO Box 22")) |           
                         (str_detect(business, "Friendly Corners") & county == "Halifax" & str_detect(address1, "2216 Mountain")) |
                         (str_detect(business, "Pointers") & county == "King and Queen" & str_detect(address1, "33 Central")) |
                         (str_detect(business, "Walmart") & county == "Lancaster" & str_detect(address1, "200 Old Fair")) |
                         (str_detect(business, "Food City") & county == "Lee" & str_detect(address1, "205 River")) |
                         (str_detect(business, "Black") & county == "Lee" & str_detect(address1, "138 Western")) |                       
                         (str_detect(business, "Food") & county == "Lee" & str_detect(address1, "177 Dollar")) |
                         (str_detect(business, "Boone") & county == "Lee" & str_detect(address1, "155 Boone")) |
                         (str_detect(business, "Monogram") & county == "Martinsville" & str_detect(address1, "200 Knauss")) |          
                         (str_detect(business, "Jack Donkey") & county == "Mecklenburg" & str_detect(address1, "3450 Britton")) |
                         (str_detect(business, "Country") & county == "Mecklenburg" & str_detect(address1, "19074 Highway")) |
                         (str_detect(business, "Country") & county == "Mecklenburg" & str_detect(address1, "3630 Highway")) |
                         (str_detect(business, "Williams") & county == "Middlesex" & str_detect(address1, "PO Box 503")) |
                         (str_detect(business, "Alcoholic Beverage") & county == "Nottoway" & str_detect(address1, "Hwy 40")) |
                         (str_detect(business, "5 Rider") & county == "Orange" & str_detect(address1, "3849 Twymans")) |
                         (str_detect(business, "Baking It") & county == "Richmond" & str_detect(address1, "5807 Hermitage")) |
                         (str_detect(business, "Crabill") & county == "Shenandoah" & is.na(address1)) |
                         (str_detect(business, "Bogg") & county == "Wise" & str_detect(address1, "4415 Bluegrass")) |
                         (str_detect(business, "GNC") & county == "Wythe" & str_detect(address1, "192 Commonwealth")) |
                         (str_detect(business, "Cliffside") & county == "Russell" & str_detect(address1, "326 Cliffside")) |
                         (str_detect(business, "KWIK") & county == "Russell" & str_detect(address1, "1275 Dante")) |
                         (str_detect(business, "Plus") & county == "Russell" & str_detect(address1, "19382 US Highway")) |
                         (str_detect(business, "Roadrunner Markets") & county == "Russell" & str_detect(address1, "19444 US Highway")) |
                         (str_detect(business, "Supermarket") & county == "Russell" & str_detect(address1, "18898 US Highway")) |
                         (str_detect(business, "Fas Mart") & county == "Russell" & str_detect(address1, "US Highway 19")) |
                         (str_detect(business, "Fas Mart") & county == "Russell" & str_detect(address1, "US Highway 58"))
  )
}

wrong1 <- filterthedata(data_geo_1)
wrong2 <- filterthedata(data_geo_2)


#
# Geocode manually ---------------------------------------------------------------------------------
#

# permanently closed means that google said so, not found means google suggested another location instead of the one
# we were looking for

# File 1
st_geometry(data_geo_1)[data_geo_1$business_id == 4915753] <- st_point(c(-78.14472220226784, 38.33715897325443))
data_geo_1$geo_method[which(data_geo_1$business_id == 4915753)] <- "manual"

st_geometry(data_geo_1)[data_geo_1$business_id == 4913790] <- st_point(c(-77.47073115811244, 37.61017531085498))
data_geo_1$geo_method[which(data_geo_1$business_id == 4913790)] <- "manual"

st_geometry(data_geo_1)[data_geo_1$business_id == 4309665] <- st_point(c(-79.39489243115185, 36.63725324074757))
data_geo_1$geo_method[which(data_geo_1$business_id == 4309665)] <- "manual"

st_geometry(data_geo_1)[data_geo_1$business_id == 3624031] <- st_point(c(-77.3230074599634, 37.47235922069874))
data_geo_1$geo_method[which(data_geo_1$business_id == 3624031)] <- "manual"

st_geometry(data_geo_1)[data_geo_1$business_id == 4916907] <- st_point(c(-79.49755431577212, 37.98748861367166))
data_geo_1$geo_method[which(data_geo_1$business_id == 4916907)] <- "manual"

# File 2
st_geometry(data_geo_2)[data_geo_2$business_id == 4303973] <- st_point(c(-82.1075785, 37.2561095))
data_geo_2$geo_method[which(data_geo_2$business_id == 4303973)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4304003] <- st_point(c(-78.6311307, 37.056289)) # not found
data_geo_2$geo_method[which(data_geo_2$business_id == 4304003)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4305669] <- st_point(c(-83.41134668465621, 36.648206699965016))
data_geo_2$geo_method[which(data_geo_2$business_id == 4305669)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4305901] <- st_point(c(-82.60265359207173, 36.94176690290877)) # not found
data_geo_2$geo_method[which(data_geo_2$business_id == 4305901)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4306019] <- st_point(c(-83.25886463068757, 36.6988842105347))
data_geo_2$geo_method[which(data_geo_2$business_id == 4306019)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4306114] <- st_point(c(-81.13541608465621, 37.17657327088676))
data_geo_2$geo_method[which(data_geo_2$business_id == 4306114)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4308373] <- st_point(c(-82.04278153862484, 36.912388353008616)) # permanently closed
data_geo_2$geo_method[which(data_geo_2$business_id == 4308373)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4308909] <- st_point(c(-78.40331548465622, 38.943424194014874))
data_geo_2$geo_method[which(data_geo_2$business_id == 4308909)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4310397] <- st_point(c(-78.39973866222307, 37.08869549923448))
data_geo_2$geo_method[which(data_geo_2$business_id == 4310397)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4311269] <- st_point(c(-82.00623983216296, 37.21409314310554)) # not found
data_geo_2$geo_method[which(data_geo_2$business_id == 4311269)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4311474] <- st_point(c(-82.3036816153438, 36.87959640271031))
data_geo_2$geo_method[which(data_geo_2$business_id == 4311474)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4311534] <- st_point(c(-81.93112325396865, 36.95947320424529))
data_geo_2$geo_method[which(data_geo_2$business_id == 4311534)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4311952] <- st_point(c(-83.01516811752178, 36.763034365818385))
data_geo_2$geo_method[which(data_geo_2$business_id == 4311952)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4312498] <- st_point(c(-78.97127175985001,36.75786265824452))
data_geo_2$geo_method[which(data_geo_2$business_id == 4312498)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4313073] <- st_point(c(-81.0973135156646, 36.95900696024521)) # permanently closed
data_geo_2$geo_method[which(data_geo_2$business_id == 4313073)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4313526] <- st_point(c(-82.30050214450294, 36.87331498193733))
data_geo_2$geo_method[which(data_geo_2$business_id == 4313526)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4313943] <- st_point(c(-82.16288596931243, 37.31137732651243)) # permanently closed
data_geo_2$geo_method[which(data_geo_2$business_id == 4313943)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4314973] <- st_point(c( -77.55085483068757, 36.67601142433903))
data_geo_2$geo_method[which(data_geo_2$business_id == 4314973)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4315017] <- st_point(c(-78.5450563, 36.712265869414324))
data_geo_2$geo_method[which(data_geo_2$business_id == 4315017)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4316296] <- st_point(c(-82.28570598465622, 36.924398115315114)) # permanently closed
data_geo_2$geo_method[which(data_geo_2$business_id == 4316296)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4316503] <- st_point(c(-79.94926804603136, 37.99405653808035))
data_geo_2$geo_method[which(data_geo_2$business_id == 4316503)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4318726] <- st_point(c(-79.87180705343788, 36.73155206879525))
data_geo_2$geo_method[which(data_geo_2$business_id == 4318726)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4318990] <- st_point(c(-75.85360698465621, 37.635420135908646)) # permanently closed
data_geo_2$geo_method[which(data_geo_2$business_id == 4318990)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4319531] <- st_point(c(-78.70897050032126, 36.943156776729474))
data_geo_2$geo_method[which(data_geo_2$business_id == 4319531)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4321197] <- st_point(c(-76.758272, 37.53994582506903))
data_geo_2$geo_method[which(data_geo_2$business_id == 4321197)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4321783] <- st_point(c(-82.19805600000001, 37.35288906874195))
data_geo_2$geo_method[which(data_geo_2$business_id == 4321783)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4322455] <- st_point(c( -82.30093296137517, 36.87309094459355))
data_geo_2$geo_method[which(data_geo_2$business_id == 4322455)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4323171] <- st_point(c(-82.9894281, 36.7019399552546))
data_geo_2$geo_method[which(data_geo_2$business_id == 4323171)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4324842] <- st_point(c( -82.30352524450281, 36.879732747335595 )) # not found
data_geo_2$geo_method[which(data_geo_2$business_id == 4324842)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4325773] <- st_point(c(-78.9147446, 38.285192847294944)) # permanently closed
data_geo_2$geo_method[which(data_geo_2$business_id == 4325773)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4325872] <- st_point(c( -79.41761238465621, 36.601151321645055))
data_geo_2$geo_method[which(data_geo_2$business_id == 4325872)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4326012] <- st_point(c( -76.57409719259348, 37.63845581567375))
data_geo_2$geo_method[which(data_geo_2$business_id == 4326012)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4327689] <- st_point(c(-81.13541978465622, 37.17642030074994))
data_geo_2$geo_method[which(data_geo_2$business_id == 4327689)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4328038] <- st_point(c(-76.38489943862486, 37.72781309049406))
data_geo_2$geo_method[which(data_geo_2$business_id == 4328038)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4328475] <- st_point(c(-78.14539556907626, 36.60519801799862)) # not found
data_geo_2$geo_method[which(data_geo_2$business_id == 4328475)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4916914] <- st_point(c(-79.2029649, 38.01261162794507))
data_geo_2$geo_method[which(data_geo_2$business_id == 4916914)] <- "manual"


#
# Save back --------------------------------------------------------------------------------
#

# Final geocoded data
write_rds(data_geo_1, "./data/working/foodretail/foodretail_nonmiss.rds")
write_rds(data_geo_2, "./data/working/foodretail/foodretail_missing_coded.rds")

data_geo_2 <- data_geo_2 %>% select(-fulladdress, -confidence)
store_geocoded <- rbind(data_geo_1, data_geo_2)

write_rds(store_geocoded, "./data/working/foodretail/foodretail_all.rds")


#
# Fix follow-up files --------------------------------------------------------------------------------
#

# Make isochrones for incorrect locations again
options(osrm.server = "http://104.248.112.16:5000/", osrm.profile = "driving")

# Filter to incorrect locations again
bussid1 <- c(4915753, 4913790, 4309665, 3624031, 4916907)
bussid2 <- c(4303973, 4304003, 4305669, 4305901, 4306019, 4306114, 4308373, 4308909, 4310397, 4311269, 4311474, 4311534, 
             4311952, 4312498, 4313073, 4313526, 4313943, 4314973, 4315017, 4316296, 4316503, 4318726, 4318990, 4319531, 
             4321197, 4321783, 4322455, 4323171, 4324842, 4325773, 4325872, 4326012, 4327689, 4328038, 4328475, 4916914)

data1 <- data_geo_1 %>% filter(business_id %in% bussid1)
data2 <- data_geo_2 %>% filter(business_id %in% bussid2)

# Read in final individual isochrone data files from 12_iso_foodretail.R
data_10 <- read_rds("./data/working/foodretail/final_foodretail_10.rds")
data_15 <- read_rds("./data/working/foodretail/final_foodretail_15.rds")

# NONMISSING FILE
# 10 minute isochrones from nonmissing (data_geo_1)
foodretail_pt1_10min <- map_dfr(c(1:nrow(data1)), ~osrmIsochrone(
  loc = data1[.x, ],
  breaks = 10,
  res = 200,
  returnclass = "sf"
))

foodretail_pt1_10min$business_id <- bussid1

# 15 minute isochrones from nonmissing (data_geo_1)
foodretail_pt1_15min <- map_dfr(c(1:nrow(data1)), ~osrmIsochrone(
  loc = data1[.x, ],
  breaks = 15,
  res = 200,
  returnclass = "sf"
))

foodretail_pt1_15min$business_id <- bussid1

# Replace rows
st_geometry(data_10)[data_10$business_id == 4915753] <- st_geometry(foodretail_pt1_10min)[foodretail_pt1_10min$business_id == 4915753]
st_geometry(data_10)[data_10$business_id == 4913790] <- st_geometry(foodretail_pt1_10min)[foodretail_pt1_10min$business_id == 4913790]
st_geometry(data_10)[data_10$business_id == 4309665] <- st_geometry(foodretail_pt1_10min)[foodretail_pt1_10min$business_id == 4309665]
st_geometry(data_10)[data_10$business_id == 3624031] <- st_geometry(foodretail_pt1_10min)[foodretail_pt1_10min$business_id == 3624031]
st_geometry(data_10)[data_10$business_id == 4916907] <- st_geometry(foodretail_pt1_10min)[foodretail_pt1_10min$business_id == 4916907]

st_geometry(data_15)[data_15$business_id == 4915753] <- st_geometry(foodretail_pt1_15min)[foodretail_pt1_15min$business_id == 4915753]
st_geometry(data_15)[data_15$business_id == 4913790] <- st_geometry(foodretail_pt1_15min)[foodretail_pt1_15min$business_id == 4913790]
st_geometry(data_15)[data_15$business_id == 4309665] <- st_geometry(foodretail_pt1_15min)[foodretail_pt1_15min$business_id == 4309665]
st_geometry(data_15)[data_15$business_id == 3624031] <- st_geometry(foodretail_pt1_15min)[foodretail_pt1_15min$business_id == 3624031]
st_geometry(data_15)[data_15$business_id == 4916907] <- st_geometry(foodretail_pt1_15min)[foodretail_pt1_15min$business_id == 4916907]


# MISSING FILE
# 10 minute isochrones from missing (data_geo_2)
foodretail_pt2_10min <- map_dfr(c(1:nrow(data2)), ~osrmIsochrone(
  loc = data2[.x, ],
  breaks = 10,
  res = 200,
  returnclass = "sf"
))

foodretail_pt2_10min$business_id <- bussid2

# 15 minute isochrones from missing (data_geo_2)
foodretail_pt2_15min <- map_dfr(c(1:nrow(data2)), ~osrmIsochrone(
  loc = data2[.x, ],
  breaks = 15,
  res = 200,
  returnclass = "sf"
))

foodretail_pt2_15min$business_id <- bussid2

# Replace rows
st_geometry(data_10)[data_10$business_id == 4303973] <- st_geometry(foodretail_pt2_10min)[foodretail_pt2_10min$business_id == 4303973]
st_geometry(data_10)[data_10$business_id == 4304003] <- st_geometry(foodretail_pt2_10min)[foodretail_pt2_10min$business_id == 4304003]
st_geometry(data_10)[data_10$business_id == 4305669] <- st_geometry(foodretail_pt2_10min)[foodretail_pt2_10min$business_id == 4305669]
st_geometry(data_10)[data_10$business_id == 4305901] <- st_geometry(foodretail_pt2_10min)[foodretail_pt2_10min$business_id == 4305901]
st_geometry(data_10)[data_10$business_id == 4306019] <- st_geometry(foodretail_pt2_10min)[foodretail_pt2_10min$business_id == 4306019]
st_geometry(data_10)[data_10$business_id == 4306114] <- st_geometry(foodretail_pt2_10min)[foodretail_pt2_10min$business_id == 4306114]
st_geometry(data_10)[data_10$business_id == 4308373] <- st_geometry(foodretail_pt2_10min)[foodretail_pt2_10min$business_id == 4308373]
st_geometry(data_10)[data_10$business_id == 4308909] <- st_geometry(foodretail_pt2_10min)[foodretail_pt2_10min$business_id == 4308909]
st_geometry(data_10)[data_10$business_id == 4310397] <- st_geometry(foodretail_pt2_10min)[foodretail_pt2_10min$business_id == 4310397]
st_geometry(data_10)[data_10$business_id == 4311269] <- st_geometry(foodretail_pt2_10min)[foodretail_pt2_10min$business_id == 4311269]
st_geometry(data_10)[data_10$business_id == 4311474] <- st_geometry(foodretail_pt2_10min)[foodretail_pt2_10min$business_id == 4311474]
st_geometry(data_10)[data_10$business_id == 4311534] <- st_geometry(foodretail_pt2_10min)[foodretail_pt2_10min$business_id == 4311534]
st_geometry(data_10)[data_10$business_id == 4311952] <- st_geometry(foodretail_pt2_10min)[foodretail_pt2_10min$business_id == 4311952]
st_geometry(data_10)[data_10$business_id == 4312498] <- st_geometry(foodretail_pt2_10min)[foodretail_pt2_10min$business_id == 4312498]
st_geometry(data_10)[data_10$business_id == 4313073] <- st_geometry(foodretail_pt2_10min)[foodretail_pt2_10min$business_id == 4313073]
st_geometry(data_10)[data_10$business_id == 4313526] <- st_geometry(foodretail_pt2_10min)[foodretail_pt2_10min$business_id == 4313526]
st_geometry(data_10)[data_10$business_id == 4313943] <- st_geometry(foodretail_pt2_10min)[foodretail_pt2_10min$business_id == 4313943]
st_geometry(data_10)[data_10$business_id == 4314973] <- st_geometry(foodretail_pt2_10min)[foodretail_pt2_10min$business_id == 4314973]
st_geometry(data_10)[data_10$business_id == 4315017] <- st_geometry(foodretail_pt2_10min)[foodretail_pt2_10min$business_id == 4315017]
st_geometry(data_10)[data_10$business_id == 4316296] <- st_geometry(foodretail_pt2_10min)[foodretail_pt2_10min$business_id == 4316296]
st_geometry(data_10)[data_10$business_id == 4316503] <- st_geometry(foodretail_pt2_10min)[foodretail_pt2_10min$business_id == 4316503]
st_geometry(data_10)[data_10$business_id == 4318726] <- st_geometry(foodretail_pt2_10min)[foodretail_pt2_10min$business_id == 4318726]
st_geometry(data_10)[data_10$business_id == 4318990] <- st_geometry(foodretail_pt2_10min)[foodretail_pt2_10min$business_id == 4318990]
st_geometry(data_10)[data_10$business_id == 4319531] <- st_geometry(foodretail_pt2_10min)[foodretail_pt2_10min$business_id == 4319531]
st_geometry(data_10)[data_10$business_id == 4321197] <- st_geometry(foodretail_pt2_10min)[foodretail_pt2_10min$business_id == 4321197]
st_geometry(data_10)[data_10$business_id == 4321783] <- st_geometry(foodretail_pt2_10min)[foodretail_pt2_10min$business_id == 4321783]
st_geometry(data_10)[data_10$business_id == 4322455] <- st_geometry(foodretail_pt2_10min)[foodretail_pt2_10min$business_id == 4322455]
st_geometry(data_10)[data_10$business_id == 4323171] <- st_geometry(foodretail_pt2_10min)[foodretail_pt2_10min$business_id == 4323171]
st_geometry(data_10)[data_10$business_id == 4324842] <- st_geometry(foodretail_pt2_10min)[foodretail_pt2_10min$business_id == 4324842]
st_geometry(data_10)[data_10$business_id == 4325773] <- st_geometry(foodretail_pt2_10min)[foodretail_pt2_10min$business_id == 4325773]
st_geometry(data_10)[data_10$business_id == 4325872] <- st_geometry(foodretail_pt2_10min)[foodretail_pt2_10min$business_id == 4325872]
st_geometry(data_10)[data_10$business_id == 4326012] <- st_geometry(foodretail_pt2_10min)[foodretail_pt2_10min$business_id == 4326012]
st_geometry(data_10)[data_10$business_id == 4327689] <- st_geometry(foodretail_pt2_10min)[foodretail_pt2_10min$business_id == 4327689]
st_geometry(data_10)[data_10$business_id == 4328038] <- st_geometry(foodretail_pt2_10min)[foodretail_pt2_10min$business_id == 4328038]
st_geometry(data_10)[data_10$business_id == 4328475] <- st_geometry(foodretail_pt2_10min)[foodretail_pt2_10min$business_id == 4328475]
st_geometry(data_10)[data_10$business_id == 4916914] <- st_geometry(foodretail_pt2_10min)[foodretail_pt2_10min$business_id == 4916914]

st_geometry(data_15)[data_15$business_id == 4303973] <- st_geometry(foodretail_pt2_15min)[foodretail_pt2_15min$business_id == 4303973]
st_geometry(data_15)[data_15$business_id == 4304003] <- st_geometry(foodretail_pt2_15min)[foodretail_pt2_15min$business_id == 4304003]
st_geometry(data_15)[data_15$business_id == 4305669] <- st_geometry(foodretail_pt2_15min)[foodretail_pt2_15min$business_id == 4305669]
st_geometry(data_15)[data_15$business_id == 4305901] <- st_geometry(foodretail_pt2_15min)[foodretail_pt2_15min$business_id == 4305901]
st_geometry(data_15)[data_15$business_id == 4306019] <- st_geometry(foodretail_pt2_15min)[foodretail_pt2_15min$business_id == 4306019]
st_geometry(data_15)[data_15$business_id == 4306114] <- st_geometry(foodretail_pt2_15min)[foodretail_pt2_15min$business_id == 4306114]
st_geometry(data_15)[data_15$business_id == 4308373] <- st_geometry(foodretail_pt2_15min)[foodretail_pt2_15min$business_id == 4308373]
st_geometry(data_15)[data_15$business_id == 4308909] <- st_geometry(foodretail_pt2_15min)[foodretail_pt2_15min$business_id == 4308909]
st_geometry(data_15)[data_15$business_id == 4310397] <- st_geometry(foodretail_pt2_15min)[foodretail_pt2_15min$business_id == 4310397]
st_geometry(data_15)[data_15$business_id == 4311269] <- st_geometry(foodretail_pt2_15min)[foodretail_pt2_15min$business_id == 4311269]
st_geometry(data_15)[data_15$business_id == 4311474] <- st_geometry(foodretail_pt2_15min)[foodretail_pt2_15min$business_id == 4311474]
st_geometry(data_15)[data_15$business_id == 4311534] <- st_geometry(foodretail_pt2_15min)[foodretail_pt2_15min$business_id == 4311534]
st_geometry(data_15)[data_15$business_id == 4311952] <- st_geometry(foodretail_pt2_15min)[foodretail_pt2_15min$business_id == 4311952]
st_geometry(data_15)[data_15$business_id == 4312498] <- st_geometry(foodretail_pt2_15min)[foodretail_pt2_15min$business_id == 4312498]
st_geometry(data_15)[data_15$business_id == 4313073] <- st_geometry(foodretail_pt2_15min)[foodretail_pt2_15min$business_id == 4313073]
st_geometry(data_15)[data_15$business_id == 4313526] <- st_geometry(foodretail_pt2_15min)[foodretail_pt2_15min$business_id == 4313526]
st_geometry(data_15)[data_15$business_id == 4313943] <- st_geometry(foodretail_pt2_15min)[foodretail_pt2_15min$business_id == 4313943]
st_geometry(data_15)[data_15$business_id == 4314973] <- st_geometry(foodretail_pt2_15min)[foodretail_pt2_15min$business_id == 4314973]
st_geometry(data_15)[data_15$business_id == 4315017] <- st_geometry(foodretail_pt2_15min)[foodretail_pt2_15min$business_id == 4315017]
st_geometry(data_15)[data_15$business_id == 4316296] <- st_geometry(foodretail_pt2_15min)[foodretail_pt2_15min$business_id == 4316296]
st_geometry(data_15)[data_15$business_id == 4316503] <- st_geometry(foodretail_pt2_15min)[foodretail_pt2_15min$business_id == 4316503]
st_geometry(data_15)[data_15$business_id == 4318726] <- st_geometry(foodretail_pt2_15min)[foodretail_pt2_15min$business_id == 4318726]
st_geometry(data_15)[data_15$business_id == 4318990] <- st_geometry(foodretail_pt2_15min)[foodretail_pt2_15min$business_id == 4318990]
st_geometry(data_15)[data_15$business_id == 4319531] <- st_geometry(foodretail_pt2_15min)[foodretail_pt2_15min$business_id == 4319531]
st_geometry(data_15)[data_15$business_id == 4321197] <- st_geometry(foodretail_pt2_15min)[foodretail_pt2_15min$business_id == 4321197]
st_geometry(data_15)[data_15$business_id == 4321783] <- st_geometry(foodretail_pt2_15min)[foodretail_pt2_15min$business_id == 4321783]
st_geometry(data_15)[data_15$business_id == 4322455] <- st_geometry(foodretail_pt2_15min)[foodretail_pt2_15min$business_id == 4322455]
st_geometry(data_15)[data_15$business_id == 4323171] <- st_geometry(foodretail_pt2_15min)[foodretail_pt2_15min$business_id == 4323171]
st_geometry(data_15)[data_15$business_id == 4324842] <- st_geometry(foodretail_pt2_15min)[foodretail_pt2_15min$business_id == 4324842]
st_geometry(data_15)[data_15$business_id == 4325773] <- st_geometry(foodretail_pt2_15min)[foodretail_pt2_15min$business_id == 4325773]
st_geometry(data_15)[data_15$business_id == 4325872] <- st_geometry(foodretail_pt2_15min)[foodretail_pt2_15min$business_id == 4325872]
st_geometry(data_15)[data_15$business_id == 4326012] <- st_geometry(foodretail_pt2_15min)[foodretail_pt2_15min$business_id == 4326012]
st_geometry(data_15)[data_15$business_id == 4327689] <- st_geometry(foodretail_pt2_15min)[foodretail_pt2_15min$business_id == 4327689]
st_geometry(data_15)[data_15$business_id == 4328038] <- st_geometry(foodretail_pt2_15min)[foodretail_pt2_15min$business_id == 4328038]
st_geometry(data_15)[data_15$business_id == 4328475] <- st_geometry(foodretail_pt2_15min)[foodretail_pt2_15min$business_id == 4328475]
st_geometry(data_15)[data_15$business_id == 4916914] <- st_geometry(foodretail_pt2_15min)[foodretail_pt2_15min$business_id == 4916914]

# Save/replace files from 12_iso_foodretail.R
write_rds(data_10, "./data/working/foodretail/final_foodretail_10.rds")
write_rds(data_15, "./data/working/foodretail/final_foodretail_15.rds")


