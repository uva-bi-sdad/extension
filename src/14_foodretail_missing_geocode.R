library(naniar)
library(dplyr)
library(sf)
library(leaflet)
library(readr)
library(janitor)
library(tidygeocoder)
library(tigris)
library(opencage)

#
# Check Missingness ---------------------------------------------
#

food_na <- readRDS("./data/working/foodretail/foodretail_missing.rds")

pct_complete_case(food_na) # 0
pct_complete_var(food_na) # 57.14

n_var_complete(food_na) # 12 variables complete
n_var_miss(food_na) # 9 have missingness
miss_var_summary(food_na)

# 1 address2         650  100    
# 2 latitude         650  100    
# 3 longitude        650  100    
# 4 email            638   98.2  
# 5 url              240   36.9  
# 6 mail_address      15    2.31 
# 7 mail_zip          13    2    
# 8 phone_primary      3    0.462
# 9 address1           2    0.308
# 10 business_id        0    0    


#
# Add Name to Address ---------------------------------------------
# 

# county and city names look fine
sort(unique(food_na$county))
sort(unique(food_na$city))

# add together business and address1 and business and mail_address
food_na$address3 <- paste(food_na$business, food_na$address1, sep = " ")
food_na$address4 <- paste(food_na$business, food_na$mail_address, sep = " ")

food_na <- food_na %>% geocode(street = address3, city = city, county = county, state = state, postalcode = zip, 
                             lat = latitude, long = longitude, method = "cascade")
# miss_var_summary(food_na)
# found 6
food_na_1 <- food_na[,-c(21:23)]
food_na_1 <- food_na_1 %>% geocode(street = address4, city = city, county = county, state = state, postalcode = zip, 
                               lat = latitude, long = longitude, method = "cascade")
# miss_var_summary(food_na_1)
# found 8

#
# Opencage ------------------------------------
#

food_na_2 <- food_na[,-c(21:23)]
readRenviron("~/.Renviron")
OPENKEY <- Sys.getenv("OPENKEY")
food_oc <- c()
for(i in 1:nrow(food_na_2)){
val <- opencage_forward(placename = food_na_2$address1[i], key = OPENKEY)
food_oc[i] <- val
}

food_oc_null <- c()
for(i in 1:length(food_oc)){
  val <- is.null(food_oc[[i]])
  food_oc_null[i] <- val
}

# missing 57 still
