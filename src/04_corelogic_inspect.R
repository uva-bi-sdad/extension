library(sf)
library(ggplot2)
library(tidyverse)

corelogic <- read_rds("./data/working/corelogic/final_corelogic_forprocessing.rds")
# use geometry
# unique property: composite_property_linkage_key
borders <- read_rds("./data/working/acs/final_countyborders.rds")


#
# Corelogic Inspect  ------------------------------------------
#

# identify extent of problem/ play around with code  ----------
# rename for easy combining
corelogic <- rename(corelogic, c("GEOID" = "fips_code"))

# subset for one county
core_1 <- corelogic[which(corelogic$GEOID=="51167"),]
bord_1 <- borders[which(borders$GEOID=="51167"),]

# plot on top of each other
ggplot()+
  geom_sf(data=bord_1)+
  geom_sf(data=core_1)

# automate checking ------------------------------------------
county <- borders$GEOID
outside_loc <- list()
outside_loc_cont <- list()

for(i in 1:length(county)){
  # subset the corelogic data for the county its supposed to be in
  subset_core <- corelogic[which(corelogic$GEOID==county[i]),]
  subset_bord <- borders[which(borders$GEOID==county[i]),]
  # check if the observation is within the border of the county its supposed to be in
  subset_core$outside <- sapply(st_within(subset_core, subset_bord),function(x){length(x)==0})
  # subset those that are outside of the county
  outside <- subset_core[which(subset_core$outside == TRUE),]
  outside <- as.data.frame(outside)
  # save in a list
  outside_loc_cont[[i]] <- outside
}

outside_core <- do.call("rbind", outside_loc)
outside_core <- as.data.frame(outside_core)
outside_core <- st_as_sf(outside_core)

# change st_intersects to st_contains and outside_loc to outside_loc_cont to do this
outside_core_cont <- do.call("rbind", outside_loc_cont)
outside_core_cont <- as.data.frame(outside_core_cont)
outside_core_cont <- st_as_sf(outside_core_cont)

# 2,732 locations that are outside their county polygons with st_intersects

# assess how many counties we need to manually geocode -------
# I'm going to loop through each location and see what county they're actually in and then save
outside_new_cty <- list()
for(i in 1:length(county)){
  # subset the borders
  subset_bord_b <- borders[which(borders$GEOID==county[i]),]
  # from the unmatched data frame check if it fits in a different county
  outside_core$outside <- sapply(st_intersects(outside_core, subset_bord_b),function(x){length(x)==0})
  # whichever observations are matched with a new county are subsetted
  outside_loop <- outside_core[which(outside_core$outside == FALSE),]
  # turn this into a data frame
  outside_loop <- as.data.frame(outside_loop)
  if(nrow(outside_loop) > 0){
  # replace its GEOID with the newly matched one
  outside_loop$GEOID <- county[i]
  }
  # save in a list
  outside_new_cty[[i]] <- outside_loop
}

outside_core_new_cty <- do.call("rbind", outside_new_cty)
outside_core_new_cty <- as.data.frame(outside_core_new_cty)
outside_core_new_cty <- st_as_sf(outside_core_new_cty)

# check if there are any duplicates
# unique = 1382 and length is 1383 so only 1 duplicate
length(unique(outside_core_new_cty$apn__parcel_number_unformatted_))

# find out which locations weren't able to be matched in a new county
'%!in%' <- function(x,y)!('%in%'(x,y))
not_matched <- outside_core[which(outside_core$apn__parcel_number_unformatted_ %!in% outside_core_new_cty$apn__parcel_number_unformatted_),]

# we still have 1348 observations that weren't able to be placed in a new county
sum(is.na(not_matched$situs_street_name))
sum(is.na(not_matched$situs_city))
sum(is.na(not_matched$situs_zip_code))

# about half have street_names, cities, and zip codes to code in another way





