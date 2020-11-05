library(naniar)
library(dplyr)
library(sf)
library(readr)
library(opencage)
library(purrr)


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
# Opencage ------------------------------------------------------------------------
#

# Make address
food_na$fulladdress <- paste0(food_na$business, ", ", food_na$address1, ", ", food_na$city, ", VA ", food_na$zip)

# Run 
output <- map_df(food_na$fulladdress, opencage_forward, key = "821a184b48df4b0fbc08e75ccfb29e12", countrycode = "US", language = "en", no_annotations = TRUE, limit = 1)
output2 <- data.frame(output$results$query, output$results$confidence, output$results$geometry.lat, output$results$geometry.lng)


#
# Prepare ------------------------------------------------------------------------
#

# Add back in
data <- left_join(food_na, output2, by = c("fulladdress" = "output.results.query"))

# Clean
data$geo_method <- "opencage"
data$latitude <- data$output.results.geometry.lat
data$longitude <- data$output.results.geometry.lng

data <- data %>% select(-output.results.geometry.lat, -output.results.geometry.lng) %>%
                 rename(confidence = output.results.confidence)

# Check
table(data$confidence)


#
# Write ------------------------------------------------------------------------
#

write_rds(data, "./data/working/foodretail/foodretail_missing_coded.rds")

