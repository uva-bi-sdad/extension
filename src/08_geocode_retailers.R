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

# Food retailers
# Problems are ok -- they are entries in column Address2 that is otherwise empty, so readr is surprised
store <- read_csv("./data/original/marketmaker_va_2020/VirginiaMarketMaker_All Food Retailers.csv") %>%
  clean_names()


#
# Geocoding --------------------------------------------------
#

# Split
store1 <- store[1:1000, ]
store2 <- store[1001:2000, ]
store3 <- store[2001:3000, ]
store4 <- store[3001:4000, ]
store5 <- store[4001:5000, ]
store6 <- store[5001:6000, ]
store7 <- store[6001:7000, ]
store8 <- store[7001:8000, ]
store9 <- store[8001:8687, ]

# Geocode
store1 <- store1 %>% geocode(street = address1, city = city, county = county, state = state, postalcode = zip, 
                             lat = latitude, long = longitude, method = "cascade")
store2 <- store2 %>% geocode(street = address1, city = city, county = county, state = state, postalcode = zip, 
                             lat = latitude, long = longitude, method = "cascade")
store3 <- store3 %>% geocode(street = address1, city = city, county = county, state = state, postalcode = zip, 
                             lat = latitude, long = longitude, method = "cascade")
store4 <- store4 %>% geocode(street = address1, city = city, county = county, state = state, postalcode = zip, 
                             lat = latitude, long = longitude, method = "cascade")
store5 <- store5 %>% geocode(street = address1, city = city, county = county, state = state, postalcode = zip, 
                             lat = latitude, long = longitude, method = "cascade")
store6 <- store6 %>% geocode(street = address1, city = city, county = county, state = state, postalcode = zip, 
                             lat = latitude, long = longitude, method = "cascade")
store7 <- store7 %>% geocode(street = address1, city = city, county = county, state = state, postalcode = zip, 
                             lat = latitude, long = longitude, method = "cascade")
store8 <- store8 %>% geocode(street = address1, city = city, county = county, state = state, postalcode = zip, 
                             lat = latitude, long = longitude, method = "cascade")
store9 <- store9 %>% geocode(street = address1, city = city, county = county, state = state, postalcode = zip, 
                             lat = latitude, long = longitude, method = "cascade")

write_rds(store1, "./data/working/foodretail/store1.Rds")
write_rds(store2, "./data/working/foodretail/store2.Rds")
write_rds(store3, "./data/working/foodretail/store3.Rds")
write_rds(store4, "./data/working/foodretail/store4.Rds") 
write_rds(store5, "./data/working/foodretail/store5.Rds") 
write_rds(store6, "./data/working/foodretail/store6.Rds") 
write_rds(store7, "./data/working/foodretail/store7.Rds") 
write_rds(store8, "./data/working/foodretail/store8.Rds") 
write_rds(store9, "./data/working/foodretail/store9.Rds") 

store1 <- read_rds("./data/working/foodretail/store1.Rds")
store2 <- read_rds("./data/working/foodretail/store2.Rds")
store3 <- read_rds("./data/working/foodretail/store3.Rds")
store4 <- read_rds("./data/working/foodretail/store4.Rds") 
store5 <- read_rds("./data/working/foodretail/store5.Rds") 
store6 <- read_rds("./data/working/foodretail/store6.Rds") 
store7 <- read_rds("./data/working/foodretail/store7.Rds") 
store8 <- read_rds("./data/working/foodretail/store8.Rds") 
store9 <- read_rds("./data/working/foodretail/store9.Rds") 

# Put back together
store_geocoded <- rbind(store1, store2, store3, store4, store5, store6, store7, store8, store9)

# 650 rows uncoded

# Business ID 4307344 is coded way out of Virginia. See what's up with that
test <- store_geocoded %>% filter(business_id == 4307344)
# Manually recode
store_geocoded$latitude[which(store_geocoded$business_id == 4307344)] <- 36.691195
store_geocoded$longitude[which(store_geocoded$business_id == 4307344)] <- -79.870405
store_geocoded$geo_method[which(store_geocoded$business_id == 4307344)] <- "manual"


#
# Plot --------------------------------------------------
#

virginiacty <- counties(state = "51", year = 2018, class = "sf")

leaflet(store_geocoded) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = virginiacty, stroke = T, weight = 2, color = "black", fillOpacity = 0) %>%
  addCircleMarkers(stroke = FALSE, fillOpacity = 1, radius = 2)


#
# Write out --------------------------------------------------
#

# All
write_rds(store_geocoded, "./data/working/foodretail/foodretail_all.rds")

# Nonmissing
store_geocoded_nonmiss <- store_geocoded %>% filter(!is.na(longitude) & !is.na(latitude))
store_geocoded_nonmiss <- st_as_sf(store_geocoded_nonmiss, coords = c("longitude", "latitude"))
st_crs(store_geocoded_nonmiss) <- 4326
store_geocoded_nonmiss <- st_transform(store_geocoded_nonmiss, crs = 4326) 
write_rds(store_geocoded_nonmiss, "./data/working/foodretail/foodretail_nonmiss.rds")

# Missing
store_geocoded_miss <- store_geocoded %>% filter(is.na(longitude) | is.na(latitude))
write_rds(store_geocoded_miss, "./data/working/foodretail/foodretail_missing.rds")

