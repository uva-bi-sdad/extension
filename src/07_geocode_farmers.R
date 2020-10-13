library(dplyr)
library(sf)
library(leaflet)
library(readr)
library(janitor)
library(tidygeocoder)
library(tigris)


#
# Read in --------------------------------------------------
#

# Farmers markets
market <- read_csv("./data/original/marketmaker_va_2020/VirginiaMarketMaker_All Farmers Markets.csv") %>%
  clean_names()


#
# Geocode --------------------------------------------------
#

# Farmers markets
market <- market %>% geocode(street = address1, city = city, county = county, state = state, postalcode = zip, 
                             lat = latitude, long = longitude, method = "cascade")

sum(is.na(market$latitude))
sum(is.na(market$longitude))

market$latitude[which(market$business=="Albemarle Farmers Market")] <- 38.1260986
market$longitude[which(market$business=="Albemarle Farmers Market")] <- -78.4450759
market$geo_method[which(market$business=="Albemarle Farmers Market")] <- "manual"

market$latitude[which(market$business=="Archwood Green Barns")] <- 38.8512019
market$longitude[which(market$business=="Archwood Green Barns")] <- -77.7885119
market$geo_method[which(market$business=="Archwood Green Barns")] <- "manual"

market$latitude[which(market$business=="Arlington Farmers' Market")] <- 38.8696979
market$longitude[which(market$business=="Arlington Farmers' Market")] <- -77.1302593
market$geo_method[which(market$business=="Arlington Farmers' Market")] <- "manual"

market$latitude[which(market$business=="Basye Farmers Market")] <- 38.8174554
market$longitude[which(market$business=="Basye Farmers Market")] <- -78.767881
market$geo_method[which(market$business=="Basye Farmers Market")] <- "manual"

market$latitude[which(market$business=="Blue Ridge Mountain Bounty LLC")] <- 36.8378833
market$longitude[which(market$business=="Blue Ridge Mountain Bounty LLC")] <- -80.9610768
market$geo_method[which(market$business=="Blue Ridge Mountain Bounty LLC")] <- "manual"

market$latitude[which(market$business=="Brambleton Farmers Market")] <- 38.984185
market$longitude[which(market$business=="Brambleton Farmers Market")] <- -77.5361759
market$geo_method[which(market$business=="Brambleton Farmers Market")] <- "manual"

market$latitude[which(market$business=="Chesterfield County Farmer's Market")] <- 37.3770027
market$longitude[which(market$business=="Chesterfield County Farmer's Market")] <- -77.5039215
market$geo_method[which(market$business=="Chesterfield County Farmer's Market")] <- "manual"

market$latitude[which(market$business=="Chippokes Creek Farm")] <- 37.1293719
market$longitude[which(market$business=="Chippokes Creek Farm")] <- -76.7080554
market$geo_method[which(market$business=="Chippokes Creek Farm")] <- "manual"

market$latitude[which(market$business=="Clarke County Farmers' Market (Winter Market)")] <- 39.15033
market$longitude[which(market$business=="Clarke County Farmers' Market (Winter Market)")] <- -77.9836822
market$geo_method[which(market$business=="Clarke County Farmers' Market (Winter Market)")] <- "manual"

market$latitude[which(market$business=="Columbia Pike Farmers Market")] <- 38.8630363
market$longitude[which(market$business=="Columbia Pike Farmers Market")] <- -77.0883078
market$geo_method[which(market$business=="Columbia Pike Farmers Market")] <- "manual"

market$latitude[which(market$business=="Community Market of Blacksburg")] <- 37.2292363
market$longitude[which(market$business=="Community Market of Blacksburg")] <- -80.4143372
market$geo_method[which(market$business=="Community Market of Blacksburg")] <- "manual"


market$latitude[which(market$business=="Forest Farmers Market")] <- 37.364962
market$longitude[which(market$business=="Forest Farmers Market")] <- -79.284974
market$geo_method[which(market$business=="Forest Farmers Market")] <- "manual"


market$latitude[which(market$business=="Ginseng Mountain Farm & Store")] <- 38.4810778
market$longitude[which(market$business=="Ginseng Mountain Farm & Store")] <- -79.5120023
market$geo_method[which(market$business=="Ginseng Mountain Farm & Store")] <- "manual"


market$latitude[which(market$business=="Going for Green Farm")] <- 37.2620951
market$longitude[which(market$business=="Going for Green Farm")] <- -77.2202958
market$geo_method[which(market$business=="Going for Green Farm")] <- "manual"
# didn't have one numeric location on road; me thinks it's the church


market$latitude[which(market$business=="Goldvein Farmers Market")] <- 38.4467081
market$longitude[which(market$business=="Goldvein Farmers Market")] <- -77.6580451
market$geo_method[which(market$business=="Goldvein Farmers Market")] <- "manual"


market$latitude[which(market$business=="GTS Farms")] <- 36.7423769
market$longitude[which(market$business=="GTS Farms")] <- -79.1618296
market$geo_method[which(market$business=="GTS Farms")] <- "manual"


market$latitude[which(market$business=="Independence Farmers Market")] <- 36.6226069
market$longitude[which(market$business=="Independence Farmers Market")] <- -81.1522859
market$geo_method[which(market$business=="Independence Farmers Market")] <- "manual"


market$latitude[which(market$business=="Irvington Farmers Market")] <- 37.6622234
market$longitude[which(market$business=="Irvington Farmers Market")] <- -76.423358
market$geo_method[which(market$business=="Irvington Farmers Market")] <- "manual"


market$business[which(market$business_id==3624083)] <- "Leesburg Market"
market$latitude[which(market$business=="Leesburg Market")] <- 39.1076883
market$longitude[which(market$business=="Leesburg Market")] <- -77.5680112
market$geo_method[which(market$business=="Leesburg Market")] <- "manual"
market <- market[!market$business_id %in% c(3624082, 3624084), ]
# there were three separate values for the same market on diff days but in same location so we combined it into one and removed others

market$latitude[which(market$business=="Lexington Farmers Market")] <- 37.7838112
market$longitude[which(market$business=="Lexington Farmers Market")] <- -79.4430851
market$geo_method[which(market$business=="Lexington Farmers Market")] <- "manual"

market$latitude[which(market$business=="Long Sunday Market")] <- 38.4745832
market$longitude[which(market$business=="Long Sunday Market")] <- -77.4121578
market$geo_method[which(market$business=="Long Sunday Market")] <- "manual"

market$latitude[which(market$business=="Manassas Farmers Market (Saturday)")] <- 38.7501882
market$longitude[which(market$business=="Manassas Farmers Market (Saturday)")] <- -77.4713945
market$geo_method[which(market$business=="Manassas Farmers Market (Saturday)")] <- "manual"

market$latitude[which(market$business=="Nelson Farmer's Market Co-op")] <- 37.8927168
market$longitude[which(market$business=="Nelson Farmer's Market Co-op")] <- -78.8699484
market$geo_method[which(market$business=="Nelson Farmer's Market Co-op")] <- "manual"

market$latitude[which(market$business=="New Market Farmers Market")] <- 38.6478792
market$longitude[which(market$business=="New Market Farmers Market")] <- -78.6806645
market$geo_method[which(market$business=="New Market Farmers Market")] <- "manual"

market$latitude[which(market$business=="North Augusta Farmers Market")] <- 38.1923785
market$longitude[which(market$business=="North Augusta Farmers Market")] <- -79.0153816
market$geo_method[which(market$business=="North Augusta Farmers Market")] <- "manual"

market$latitude[which(market$business=="Shen-Val Farm Market")] <- 39.0620082
market$longitude[which(market$business=="Shen-Val Farm Market")] <- -78.1432245
market$geo_method[which(market$business=="Shen-Val Farm Market")] <- "manual"

market$latitude[which(market$business=="Spotsylvania Farmers Market")] <- 38.2902714
market$longitude[which(market$business=="Spotsylvania Farmers Market")] <- -77.5648695
market$geo_method[which(market$business=="Spotsylvania Farmers Market")] <- "manual"
# there are two spotsylvania locations; I chose the one closest to the same but it's located in fredricksville which is close

market$latitude[which(market$business=="Strasburg Farmers Market")] <- 38.9886095
market$longitude[which(market$business=="Strasburg Farmers Market")] <- -78.3622586
market$geo_method[which(market$business=="Strasburg Farmers Market")] <- "manual"

market$latitude[which(market$business=="Wallops Farmers Market")] <- 37.9346799
market$longitude[which(market$business=="Wallops Farmers Market")] <- -75.4848145
market$geo_method[which(market$business=="Wallops Farmers Market")] <- "manual"

market$latitude[which(market$business=="Wellness Connection Farmers Market")] <- 38.9404382
market$longitude[which(market$business=="Wellness Connection Farmers Market")] <- -77.5424483
market$geo_method[which(market$business=="Wellness Connection Farmers Market")] <- "manual"

market$latitude[which(market$business=="Westwind Flowers")]
market$longitude[which(market$business=="Westwind Flowers")]
market$geo_method[which(market$business=="Westwind Flowers")] <- "manual"
# not even their website had their location; their farm isn't open to the public tho

market$latitude[which(market$business=="Drakes Branch Farmers Market")] <- 36.992656
market$longitude[which(market$business=="Drakes Branch Farmers Market")] <- -78.597787
market$geo_method[which(market$business=="Drakes Branch Farmers Market")] <- "manual"

market$latitude[which(market$business=="Exit 40 Farmers Market")] <- 38.8207582
market$longitude[which(market$business=="Exit 40 Farmers Market")] <- -77.6446705
market$geo_method[which(market$business=="Exit 40 Farmers Market")] <- "manual"

market$latitude[which(market$business=="Fall Line Farms and Local Roots")]
market$longitude[which(market$business=="Fall Line Farms and Local Roots")]
market$geo_method[which(market$business=="Fall Line Farms and Local Roots")] <- "manual"
# there are tons of pickup locations for them; their address on the site is their accountant

market$latitude[which(market$business=="Field to Table, Inc.")]
market$longitude[which(market$business=="Field to Table, Inc.")]
market$geo_method[which(market$business=="Field to Table, Inc.")] <- "manual"
# they also have different locations through va

market$latitude[which(market$business=="Fireside Farm")] <- 38.0414735
market$longitude[which(market$business=="Fireside Farm")] <- -79.7899674
market$geo_method[which(market$business=="Fireside Farm")] <- "manual"

# still missing 3 locations bc there isnt a location listed or because there are multiple locations

#
# Plot --------------------------------------------------
#

virginiacty <- counties(state = "51", year = 2018, class = "sf")

# Farmers Markets

leaflet(market) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = virginiacty, stroke = T, weight = 2, color = "black", fillOpacity = 0) %>%
  addCircleMarkers(stroke = FALSE, fillOpacity = 1, radius = 2)


#
# Write out --------------------------------------------------
#

write.csv(market, "./data/working/geocode/01_market.csv")
