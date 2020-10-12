library(tidygeocoder)
library(dplyr)
library(readxl)
library(sf)
library(leaflet)
library(disco)
library(RColorBrewer)
library(readr)
library(stringr)
library(tigris)


#
# Read in --------------------------------------------------
#

market <- read.csv("./data/original/marketmaker_va_2020/VirginiaMarketMaker_All Farmers Markets.csv")


#
# Geocode --------------------------------------------------
#

market <- market %>% geocode(street = Address1, city = City, county = County, state = State, postalcode= Zip, lat = latitude, long = longitude, method = "cascade")

sum(is.na(market$latitude))

# need manual geocoding for 29 locations (4,5,9,12,18,2,22,24,35,38,44,46,47,51,52,54,56,67,72,84,87,90,91,92,93,94,115,119,149)
market$latitude[4] <- 38.8512019
market$longitude[4] <- -77.7885119
market$geo_method[4] <- "manual"

market$latitude[5] <- 
market$longitude[5] <- 
market$geo_method[5] <- "manual"


#
# Plot --------------------------------------------------
#

virginiacty <- counties(state = "51", year = 2018)
virginiacty <- st_as_sf(virginiacty)

# Farmers Markets

leaflet(market) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = virginiacty, stroke = T, weight = 2, color = "black", fillOpacity = 0) %>%
  addCircleMarkers(stroke = FALSE, fillOpacity = 1, radius = 2)


#
# Write out --------------------------------------------------
#

write.csv(market, "./data/working/geocode/01_market.csv")
