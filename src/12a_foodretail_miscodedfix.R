library(readr)
library(dplyr)
library(sf)
library(stringr)
library(osrm)
library(lwgeom)


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
st_geometry(data_geo_2)[data_geo_2$business_id == 4303973] <- st_point(c())
data_geo_2$geo_method[which(data_geo_2$business_id == 4303973)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4304003] <- st_point(c())
data_geo_2$geo_method[which(data_geo_2$business_id == 4304003)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4305669] <- st_point(c())
data_geo_2$geo_method[which(data_geo_2$business_id == 4305669)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4305901] <- st_point(c())
data_geo_2$geo_method[which(data_geo_2$business_id == 4305901)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4306019] <- st_point(c())
data_geo_2$geo_method[which(data_geo_2$business_id == 4306019)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4306114] <- st_point(c())
data_geo_2$geo_method[which(data_geo_2$business_id == 4306114)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4308373] <- st_point(c())
data_geo_2$geo_method[which(data_geo_2$business_id == 4308373)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4308909] <- st_point(c())
data_geo_2$geo_method[which(data_geo_2$business_id == 4308909)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4310397] <- st_point(c())
data_geo_2$geo_method[which(data_geo_2$business_id == 4310397)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4311269] <- st_point(c())
data_geo_2$geo_method[which(data_geo_2$business_id == 4311269)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4311474] <- st_point(c())
data_geo_2$geo_method[which(data_geo_2$business_id == 4311474)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4311534] <- st_point(c())
data_geo_2$geo_method[which(data_geo_2$business_id == 4311534)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4311952] <- st_point(c())
data_geo_2$geo_method[which(data_geo_2$business_id == 4311952)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4312498] <- st_point(c())
data_geo_2$geo_method[which(data_geo_2$business_id == 4312498)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4313073] <- st_point(c())
data_geo_2$geo_method[which(data_geo_2$business_id == 4313073)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4313526] <- st_point(c())
data_geo_2$geo_method[which(data_geo_2$business_id == 4313526)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4313943] <- st_point(c())
data_geo_2$geo_method[which(data_geo_2$business_id == 4313943)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4314973] <- st_point(c())
data_geo_2$geo_method[which(data_geo_2$business_id == 4314973)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4315017] <- st_point(c())
data_geo_2$geo_method[which(data_geo_2$business_id == 4315017)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4316296] <- st_point(c())
data_geo_2$geo_method[which(data_geo_2$business_id == 4316296)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4316503] <- st_point(c())
data_geo_2$geo_method[which(data_geo_2$business_id == 4316503)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4318726] <- st_point(c())
data_geo_2$geo_method[which(data_geo_2$business_id == 4318726)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4318990] <- st_point(c())
data_geo_2$geo_method[which(data_geo_2$business_id == 4318990)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4319531] <- st_point(c())
data_geo_2$geo_method[which(data_geo_2$business_id == 4319531)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4321197] <- st_point(c())
data_geo_2$geo_method[which(data_geo_2$business_id == 4321197)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4321783] <- st_point(c())
data_geo_2$geo_method[which(data_geo_2$business_id == 4321783)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4322455] <- st_point(c())
data_geo_2$geo_method[which(data_geo_2$business_id == 4322455)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4323171] <- st_point(c())
data_geo_2$geo_method[which(data_geo_2$business_id == 4323171)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4324842] <- st_point(c())
data_geo_2$geo_method[which(data_geo_2$business_id == 4324842)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4325773] <- st_point(c())
data_geo_2$geo_method[which(data_geo_2$business_id == 4325773)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4325872] <- st_point(c())
data_geo_2$geo_method[which(data_geo_2$business_id == 4325872)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4326012] <- st_point(c())
data_geo_2$geo_method[which(data_geo_2$business_id == 4326012)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4327689] <- st_point(c())
data_geo_2$geo_method[which(data_geo_2$business_id == 4327689)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4328038] <- st_point(c())
data_geo_2$geo_method[which(data_geo_2$business_id == 4328038)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4328475] <- st_point(c())
data_geo_2$geo_method[which(data_geo_2$business_id == 4328475)] <- "manual"

st_geometry(data_geo_2)[data_geo_2$business_id == 4916914] <- st_point(c())
data_geo_2$geo_method[which(data_geo_2$business_id == 4916914)] <- "manual"









###########

# Final individual isochrone data files from 12_iso_foodretail.R
data_10 <- read_rds("./data/working/foodretail/final_foodretail_10.rds")
data_15 <- read_rds("./data/working/foodretail/final_foodretail_10.rds")

