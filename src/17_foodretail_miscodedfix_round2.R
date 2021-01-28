library(readr)
library(dplyr)
library(sf)
library(stringr)
library(osrm)
library(lwgeom)
library(purrr)
library(tidycensus)


#
# Read in: stray points --------------------------------------------------------------------------------
#

# Final geocoded data
data <- read_rds("./data/working/foodretail/final_foodretail_forapp.rds")


#
# Remove still problematic entries ------------------------------------------------------------------------
#

data <- subset(data, !(business == "Mill Creek Produce" & county == "Augusta County"))
data <- subset(data, !(business == "Shenandoah Valley Markets Inc" & county == "Augusta County"))
data <- subset(data, !(business == "Schwan Food Co" & county == "Augusta County"))
data <- subset(data, !(business == "Duffey's Mini Market" & county == "Charlotte County"))
data <- subset(data, !(business == "Jarratt Fas-Shop Food Mart" & county == "Greensville County"))
data <- subset(data, !(business == "Exxon" & county == "Greensville County"))
data <- subset(data, !(business == "Jack Donkey Farm, LLC" & county == "Mecklenburg County"))
data <- subset(data, !(business == "Holly Grove Marina" & county == "Mecklenburg County"))
data <- subset(data, !(business == "Gator's General Supply" & county == "Mecklenburg County"))
data <- subset(data, !(business == "Circle K" & county == "Mecklenburg County" & city == "Bracey"))
data <- subset(data, !(business == "Alcoholic Beverage Control" & county == "Nottoway County"))
data <- subset(data, !(business == "5 Rider's Farm" & county == "Orange County"))
data <- subset(data, !(business == "Baking It Easy Sweetshop, LLC" & county == "Richmond County"))

write_rds(data, "./data/working/foodretail/final_foodretail_forapp.rds")


#
# 10 minute isochrones --------------------------------------------------------------------------------
#

# Final geocoded data - 10 min isos
data <- read_rds("./data/working/foodretail/final_food_10_coverage.rds")

data <- subset(data, !(business == "Mill Creek Produce" & county == "Augusta County"))
data <- subset(data, !(business == "Shenandoah Valley Markets Inc" & county == "Augusta County"))
data <- subset(data, !(business == "Schwan Food Co" & county == "Augusta County"))
data <- subset(data, !(business == "Duffey's Mini Market" & county == "Charlotte County"))
data <- subset(data, !(business == "Jarratt Fas-Shop Food Mart" & county == "Greensville County"))
data <- subset(data, !(business == "Exxon" & county == "Greensville County"))
data <- subset(data, !(business == "Jack Donkey Farm, LLC" & county == "Mecklenburg County"))
data <- subset(data, !(business == "Holly Grove Marina" & county == "Mecklenburg County"))
data <- subset(data, !(business == "Gator's General Supply" & county == "Mecklenburg County"))
data <- subset(data, !(business == "Circle K" & county == "Mecklenburg County" & city == "Bracey"))
data <- subset(data, !(business == "Alcoholic Beverage Control" & county == "Nottoway County"))
data <- subset(data, !(business == "5 Rider's Farm" & county == "Orange County"))
data <- subset(data, !(business == "Baking It Easy Sweetshop, LLC" & county == "Richmond County"))

write_rds(data, "./data/working/foodretail/final_food_10_coverage.rds")


#
# 15 minute isochrones --------------------------------------------------------------------------------
#

# Final geocoded data - 15 min isos
data <- read_rds("./data/working/foodretail/final_food_15_coverage.rds")

data <- subset(data, !(business == "Mill Creek Produce" & county == "Augusta County"))
data <- subset(data, !(business == "Shenandoah Valley Markets Inc" & county == "Augusta County"))
data <- subset(data, !(business == "Schwan Food Co" & county == "Augusta County"))
data <- subset(data, !(business == "Duffey's Mini Market" & county == "Charlotte County"))
data <- subset(data, !(business == "Jarratt Fas-Shop Food Mart" & county == "Greensville County"))
data <- subset(data, !(business == "Exxon" & county == "Greensville County"))
data <- subset(data, !(business == "Jack Donkey Farm, LLC" & county == "Mecklenburg County"))
data <- subset(data, !(business == "Holly Grove Marina" & county == "Mecklenburg County"))
data <- subset(data, !(business == "Gator's General Supply" & county == "Mecklenburg County"))
data <- subset(data, !(business == "Circle K" & county == "Mecklenburg County" & city == "Bracey"))
data <- subset(data, !(business == "Alcoholic Beverage Control" & county == "Nottoway County"))
data <- subset(data, !(business == "5 Rider's Farm" & county == "Orange County"))
data <- subset(data, !(business == "Baking It Easy Sweetshop, LLC" & county == "Richmond County"))

write_rds(data, "./data/working/foodretail/final_food_15_coverage.rds")


#
# Read in: stray isochrones --------------------------------------------------------------------------------
#

# Final geocoded data
data <- read_rds("./data/working/foodretail/final_foodretail_forapp.rds")

data <- subset(data, !(business == "Springdale Water Gardens" & county == "Augusta County"))
data <- subset(data, !(business == "Super Save" & county == "Augusta County"))
data <- subset(data, !(business == "Virginia Travel Plaza" & county == "Bland County"))
data <- subset(data, !(business == "Sweet Frog" & county == "Danville city"))
data <- subset(data, !(business == "Walmart Supercenter" & county == "Lancaster County"))
data <- subset(data, !(business == "Bracey Slip-In" & county == "Mecklenburg County"))
data <- subset(data, !(business == "Whitby's Country Store" & county == "Mecklenburg County"))
data <- subset(data, !(business == "T M Williams & Son Seafood Inc" & county == "Middlesex County"))
data <- subset(data, !(business == "Stop & Shop Supermarket" & county == "Russell County"))
data <- subset(data, !(business == "Bracey Mini Mart" & county == "Mecklenburg County"))
data <- subset(data, !(business == "Daley & Son Grocery" & county == "Accomack County"))

write_rds(data, "./data/working/foodretail/final_foodretail_forapp.rds")


#
# Read in: stray isochrones 10min --------------------------------------------------------------------------------
#

# Final geocoded data
data <- read_rds("./data/working/foodretail/final_food_10_coverage.rds")

data <- subset(data, !(business == "Springdale Water Gardens" & county == "Augusta County"))
data <- subset(data, !(business == "Super Save" & county == "Augusta County"))
data <- subset(data, !(business == "Virginia Travel Plaza" & county == "Bland County"))
data <- subset(data, !(business == "Sweet Frog" & county == "Danville city"))
data <- subset(data, !(business == "Walmart Supercenter" & county == "Lancaster County"))
data <- subset(data, !(business == "Bracey Slip-In" & county == "Mecklenburg County"))
data <- subset(data, !(business == "Whitby's Country Store" & county == "Mecklenburg County"))
data <- subset(data, !(business == "T M Williams & Son Seafood Inc" & county == "Middlesex County"))
data <- subset(data, !(business == "Stop & Shop Supermarket" & county == "Russell County"))
data <- subset(data, !(business == "Bracey Mini Mart" & county == "Mecklenburg County"))
data <- subset(data, !(business == "Daley & Son Grocery" & county == "Accomack County"))

write_rds(data, "./data/working/foodretail/final_food_10_coverage.rds")


#
# Read in: stray isochrones 15min --------------------------------------------------------------------------------
#

# Final geocoded data
data <- read_rds("./data/working/foodretail/final_food_15_coverage.rds")

data <- subset(data, !(business == "Springdale Water Gardens" & county == "Augusta County"))
data <- subset(data, !(business == "Super Save" & county == "Augusta County"))
data <- subset(data, !(business == "Virginia Travel Plaza" & county == "Bland County"))
data <- subset(data, !(business == "Sweet Frog" & county == "Danville city"))
data <- subset(data, !(business == "Walmart Supercenter" & county == "Lancaster County"))
data <- subset(data, !(business == "Bracey Slip-In" & county == "Mecklenburg County"))
data <- subset(data, !(business == "Whitby's Country Store" & county == "Mecklenburg County"))
data <- subset(data, !(business == "T M Williams & Son Seafood Inc" & county == "Middlesex County"))
data <- subset(data, !(business == "Stop & Shop Supermarket" & county == "Russell County"))
data <- subset(data, !(business == "Bracey Mini Mart" & county == "Mecklenburg County"))
data <- subset(data, !(business == "Daley & Son Grocery" & county == "Accomack County"))

write_rds(data, "./data/working/foodretail/final_food_15_coverage.rds")

