library(tidycensus)
library(tidyverse)
library(ggthemes)
library(ggmap)
library(ggrepel)
library(sf)
require(utils)
require(rgdal)
library(ggsflabel)

### acs graphics for teja

acsvars <- c(
  # age 65 +
  "B01001_020", "B01001_021", "B01001_022", "B01001_023", "B01001_024", "B01001_025",
  "B01001_044", "B01001_045", "B01001_046", "B01001_047", "B01001_048", "B01001_049",
  "B01001_001",
  # age <18
  "B01001_003", "B01001_004", "B01001_005", "B01001_006",
  "B01001_027", "B01001_028", "B01001_029", "B01001_030",
  # Hispanic
  "B03001_003", "B03001_001",
  # Black
  "B02001_003", "B02001_001",
  # Without BA
  "B15003_002", "B15003_003", "B15003_004", "B15003_005", "B15003_006", "B15003_007",
  "B15003_008", "B15003_009", "B15003_010", "B15003_011", "B15003_012", "B15003_013",
  "B15003_014", "B15003_015", "B15003_016", "B15003_017", "B15003_018", "B15003_019",
  "B15003_020", "B15003_021", "B15003_001",
  # Unemployed
  "B23025_005", "B23025_002",
  # In poverty
  "B17001_002", "B17001_001",
  # Without health insurance
  "B27001_005", "B27001_008", "B27001_011", "B27001_014", "B27001_017", "B27001_020", "B27001_023",
  "B27001_026", "B27001_029", "B27001_033", "B27001_036", "B27001_039", "B27001_042", "B27001_045",
  "B27001_048", "B27001_051", "B27001_054", "B27001_057", "B27001_001",
  # types of computers
  "B28001_001", "B28001_002", "B28001_003", "B28001_005","B28001_007", "B28001_011", "B28001_009",
  # presence and types of internet subscriptions
  "B28002_001", "B28002_013", "B28002_005", "B28002_009", "B28002_003", "B28002_007",
  # type of HI coverage by age, without HI
  "B27010_001", "B27010_017", "B27010_033", "B27010_050", "B27010_066",
  # public HI status by sex by age
  "B27003_001", "B27003_005", "B27003_008", "B27003_011", "B27003_014", "B27003_017", "B27003_020",
  "B27003_023", "B27003_026", "B27003_029", "B27003_033", "B27003_036", "B27003_039", "B27003_042",
  "B27003_045", "B27003_048", "B27003_051", "B27003_054", "B27003_057",
  # private HI status by sex by age
  "B27002_001", "B27002_005", "B27002_008", "B27002_011", "B27002_014", "B27002_017", "B27002_020",
  "B27002_023", "B27002_026", "B27002_029", "B27002_033", "B27002_036", "B27002_039", "B27002_042",
  "B27002_045", "B27002_048", "B27002_051", "B27002_054", "B27002_057",
  # public assistance or snap in past 12 months
  "B19058_001", "B19058_003"
)

census_key <- "ac2c12d089ea55018c59548c149fbe915463bebe"

data_county <- get_acs(geography = "county", state = 51, county = 141,
                      variables = acsvars,
                      year = 2019, survey = "acs5",
                      cache_table = TRUE, output = "wide", geometry = TRUE,
                      keep_geo_vars = TRUE)

data_tract <- get_acs(geography = "tract", state = 51, county = 141,
                      variables = acsvars,
                      year = 2019, survey = "acs5",
                      cache_table = TRUE, output = "wide", geometry = TRUE,
                      keep_geo_vars = TRUE)

data_bgrp <- get_acs(geography = "block group", state = 51, county = 141,
                     variables = acsvars,
                     year = 2019, survey = "acs5",
                     cache_table = TRUE, output = "wide", geometry = TRUE,
                     keep_geo_vars = TRUE)

acs_county <- data_county %>% transmute(
  STATEFP = STATEFP,
  COUNTYFP = COUNTYFP,
  GEOID = GEOID,
  NAME.x = NAME.x,
  NAME.y = NAME.y,
  ALAND = ALAND,
  AWATER = AWATER,
  geometry = geometry,
  age65 = (B01001_020E + B01001_021E + B01001_022E + B01001_023E + B01001_024E + B01001_025E +
             B01001_044E + B01001_045E + B01001_046E + B01001_047E + B01001_048E + B01001_049E) / B01001_001E * 100,
  under18 = (B01001_003E + B01001_004E + B01001_005E + B01001_006E +
               B01001_027E + B01001_028E + B01001_029E + B01001_030E) / B01001_001E * 100,
  hispanic = B03001_003E / B02001_001E * 100,
  black = B02001_003E / B02001_001E * 100,
  noba = (B15003_002E + B15003_003E + B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E +
            B15003_009E + B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E +
            B15003_016E + B15003_017E + B15003_018E + B15003_019E + B15003_020E + B15003_021E) / B15003_001E * 100,
  unempl = B23025_005E / B23025_002E * 100,
  inpov = B17001_002E / B17001_001E * 100,
  nohealthins = (B27001_005E + B27001_008E + B27001_011E + B27001_014E + B27001_017E + B27001_020E + B27001_023E + 
                   B27001_026E + B27001_029E + B27001_033E + B27001_036E + B27001_039E + B27001_042E + B27001_045E +
                   B27001_048E + B27001_051E + B27001_054E + B27001_057E) / B27001_001E * 100,
  nocomputer = B28001_011E/B28001_001E * 100,
  laptop = B28001_003E/B28001_001E * 100,
  smartphone = B28001_005E/B28001_001E * 100,
  tablet = B28001_007E/B28001_001E * 100,
  othercomputer = B28001_009E/B28001_001E * 100,
  nointernet = B28002_013E/B28002_001E * 100,
  satellite = B28002_009E/B28002_001E * 100,
  cellular = B28002_005E/B28002_001E * 100,
  dialup = B28002_003E/B28002_001E * 100,
  broadband = B28002_007E/B28002_001E * 100,
  nohealthins2 = (B27010_017E + B27010_033E + B27010_050E + B27010_066E)/B27010_001E * 100,
  publicins = (B27003_005E + B27003_008E + B27003_011E + B27003_014E + B27003_017E + B27003_020E + B27003_023E + 
                 B27003_026E + B27003_029E + B27003_033E + B27003_036E + B27003_039E + B27003_042E + B27003_045E +
                 B27003_048E + B27003_051E + B27003_054E + B27003_057E) / B27003_001E * 100,
  privateins = (B27002_005E + B27002_008E + B27002_011E + B27002_014E + B27002_017E + B27002_020E + B27002_023E + 
                  B27002_026E + B27002_029E + B27002_033E + B27002_036E + B27002_039E + B27002_042E + B27002_045E +
                  B27002_048E + B27002_051E + B27002_054E + B27002_057E) / B27002_001E * 100,
  snap = B19058_003E/B19058_001E * 100
)

acs_tract <- data_tract %>% transmute(
  STATEFP = STATEFP,
  COUNTYFP = COUNTYFP,
  TRACTCE = TRACTCE,
  GEOID = GEOID,
  NAME.x = NAME.x,
  NAME.y = NAME.y,
  ALAND = ALAND,
  AWATER = AWATER,
  geometry = geometry,
  age65 = (B01001_020E + B01001_021E + B01001_022E + B01001_023E + B01001_024E + B01001_025E +
             B01001_044E + B01001_045E + B01001_046E + B01001_047E + B01001_048E + B01001_049E) / B01001_001E * 100,
  under18 = (B01001_003E + B01001_004E + B01001_005E + B01001_006E +
               B01001_027E + B01001_028E + B01001_029E + B01001_030E) / B01001_001E * 100,
  hispanic = B03001_003E / B02001_001E * 100,
  black = B02001_003E / B02001_001E * 100,
  noba = (B15003_002E + B15003_003E + B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E +
            B15003_009E + B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E +
            B15003_016E + B15003_017E + B15003_018E + B15003_019E + B15003_020E + B15003_021E) / B15003_001E * 100,
  unempl = B23025_005E / B23025_002E * 100,
  inpov = B17001_002E / B17001_001E * 100,
  nohealthins = (B27001_005E + B27001_008E + B27001_011E + B27001_014E + B27001_017E + B27001_020E + B27001_023E + 
                   B27001_026E + B27001_029E + B27001_033E + B27001_036E + B27001_039E + B27001_042E + B27001_045E +
                   B27001_048E + B27001_051E + B27001_054E + B27001_057E) / B27001_001E * 100,
  nocomputer = B28001_011E/B28001_001E * 100,
  laptop = B28001_003E/B28001_001E * 100,
  smartphone = B28001_005E/B28001_001E * 100,
  tablet = B28001_007E/B28001_001E * 100,
  othercomputer = B28001_009E/B28001_001E * 100,
  nointernet = B28002_013E/B28002_001E * 100,
  satellite = B28002_009E/B28002_001E * 100,
  cellular = B28002_005E/B28002_001E * 100,
  dialup = B28002_003E/B28002_001E * 100,
  broadband = B28002_007E/B28002_001E * 100,
  nohealthins2 = (B27010_017E + B27010_033E + B27010_050E + B27010_066E)/B27010_001E * 100,
  publicins = (B27003_005E + B27003_008E + B27003_011E + B27003_014E + B27003_017E + B27003_020E + B27003_023E + 
                 B27003_026E + B27003_029E + B27003_033E + B27003_036E + B27003_039E + B27003_042E + B27003_045E +
                 B27003_048E + B27003_051E + B27003_054E + B27003_057E) / B27003_001E * 100,
  privateins = (B27002_005E + B27002_008E + B27002_011E + B27002_014E + B27002_017E + B27002_020E + B27002_023E + 
                  B27002_026E + B27002_029E + B27002_033E + B27002_036E + B27002_039E + B27002_042E + B27002_045E +
                  B27002_048E + B27002_051E + B27002_054E + B27002_057E) / B27002_001E * 100,
  snap = B19058_003E/B19058_001E * 100
)

# Block group (note: variables with estimate = 0 will have NAs in the final calculation. Disregard these
# for now and use tract-level values for plotting.)
acs_bgrp <- data_bgrp %>% transmute(
  STATEFP = STATEFP,
  COUNTYFP = COUNTYFP,
  TRACTCE = TRACTCE,
  BLKGRPCE = BLKGRPCE,
  GEOID = GEOID,
  NAME.x = NAME.x,
  NAME.y = NAME.y,
  ALAND = ALAND,
  AWATER = AWATER,
  geometry = geometry,
  age65 = (B01001_020E + B01001_021E + B01001_022E + B01001_023E + B01001_024E + B01001_025E +
             B01001_044E + B01001_045E + B01001_046E + B01001_047E + B01001_048E + B01001_049E) / B01001_001E * 100,
  under18 = (B01001_003E + B01001_004E + B01001_005E + B01001_006E +
               B01001_027E + B01001_028E + B01001_029E + B01001_030E) / B01001_001E * 100,
  hispanic = B03001_003E / B02001_001E * 100,
  black = B02001_003E / B02001_001E * 100,
  noba = (B15003_002E + B15003_003E + B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E +
            B15003_009E + B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E +
            B15003_016E + B15003_017E + B15003_018E + B15003_019E + B15003_020E + B15003_021E) / B15003_001E * 100,
  unempl = B23025_005E / B23025_002E * 100,
  inpov = B17001_002E / B17001_001E * 100,
  nohealthins = (B27001_005E + B27001_008E + B27001_011E + B27001_014E + B27001_017E + B27001_020E + B27001_023E + 
                   B27001_026E + B27001_029E + B27001_033E + B27001_036E + B27001_039E + B27001_042E + B27001_045E +
                   B27001_048E + B27001_051E + B27001_054E + B27001_057E) / B27001_001E * 100,
  nocomputer = B28001_011E/B28001_001E * 100,
  laptop = B28001_003E/B28001_001E * 100,
  smartphone = B28001_005E/B28001_001E * 100,
  tablet = B28001_007E/B28001_001E * 100,
  othercomputer = B28001_009E/B28001_001E * 100,
  nointernet = B28002_013E/B28002_001E * 100,
  satellite = B28002_009E/B28002_001E * 100,
  cellular = B28002_005E/B28002_001E * 100,
  dialup = B28002_003E/B28002_001E * 100,
  broadband = B28002_007E/B28002_001E * 100,
  nohealthins2 = (B27010_017E + B27010_033E + B27010_050E + B27010_066E)/B27010_001E * 100,
  publicins = (B27003_005E + B27003_008E + B27003_011E + B27003_014E + B27003_017E + B27003_020E + B27003_023E + 
                 B27003_026E + B27003_029E + B27003_033E + B27003_036E + B27003_039E + B27003_042E + B27003_045E +
                 B27003_048E + B27003_051E + B27003_054E + B27003_057E) / B27003_001E * 100,
  privateins = (B27002_005E + B27002_008E + B27002_011E + B27002_014E + B27002_017E + B27002_020E + B27002_023E + 
                  B27002_026E + B27002_029E + B27002_033E + B27002_036E + B27002_039E + B27002_042E + B27002_045E +
                  B27002_048E + B27002_051E + B27002_054E + B27002_057E) / B27002_001E * 100,
  snap = B19058_003E/B19058_001E * 100
)

# Unemployed in LF
min_unempl <- floor(min(acs_bgrp$unempl))
max_unempl <- ceiling(max(acs_bgrp$unempl))
register_google(key = "AIzaSyBCiyoZ4dEGYES5JLWdm9jODyJXU0clsoI", write = T)
gc <- geocode("patrick county, va, usa", source = "google")
center <- as.numeric(gc)

p <- get_stamenmap(bbox = c(left = -81, bottom = 36, 
                                  top = 37, right= -78),
                         zoom = 10,
                         maptype = "terrain")
ggmap(p)
acs_county <- st_transform(acs_county, 3857)
acs_tract <- st_transform(acs_tract, 3857)
acs_bgrp <- st_transform(acs_bgrp, 3857)

plot(st_transform(acs_county, crs = 3857)[1], bgMap = p,
     main = "County")
plot(st_transform(acs_county, crs = 3857)[13], bgMap = p,
     main = "Percent Population Unemployed by Count")
plot(st_transform(acs_tract, crs = 3857)[1], bgMap = p,
     main = "Tract")
plot(st_transform(acs_tract, crs = 3857)[15], bgMap = p,
     main = "Percent Population Unemployed by Census Tract")
plot(st_transform(acs_bgrp, crs = 3857)[1], bgMap = p,
     main = "Block Group")
plot(st_transform(acs_bgrp, crs = 3857)[16], bgMap = p,
     main = "Percent Population Unemployed by Census Block Group")

ggmap(p) +
  geom_sf(data = st_transform(acs_county, crs = 3857), size = 0.2, aes(fill = unempl), inherit.aes = FALSE) +
  labs(title = "Percent Population Unemployed\nby Census County, 2015/19\nPatrick County, Virginia",
       caption = "Source: American Community Survey 2015/19 (5-year) estimates.") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_unempl, max_unempl), 
                        breaks = seq(min_unempl, max_unempl, length.out = 5))
ggsave(path = "/Users/morgan/Documents/work/uva/non-dspg", device = "png", filename = "county_unempl.png", plot = last_plot())

ggplot() +
  geom_sf(data = acs_tract, size = 0.2, aes(fill = unempl)) +
  labs(title = "Percent Population Unemployed\nby Census Tract, 2015/19\nPatrick County, Virginia",
       caption = "Source: American Community Survey 2015/19 (5-year) estimates.") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_unempl, max_unempl), 
                        breaks = seq(min_unempl, max_unempl, length.out = 5))
ggsave(path = "/Users/morgan/Documents/work/uva/non-dspg/", device = "png", filename = "tract_unempl.png", plot = last_plot())

ggplot() +
  geom_sf(data = acs_bgrp, size = 0.2, aes(fill = unempl)) +
  labs(title = "Percent Population Unemployed\nby Census Block Group, 2015/19\nPatrick County, Virginia",
       caption = "Source: American Community Survey 2015/19 (5-year) estimates.") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_unempl, max_unempl), 
                        breaks = seq(min_unempl, max_unempl, length.out = 5))
ggsave(path = "/Users/morgan/Documents/work/uva/non-dspg/", device = "png", filename = "bgrp_unempl.png", plot = last_plot())

acs_county$NAME.x <- "Patrick County"
ggplot(acs_county) +
  geom_sf(data = acs_county, size = 0.2) +
  geom_sf_label(aes(label = NAME.x)) + 
  labs(title = "Census County\nPatrick County, Virginia") +
  theme_map() + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_unempl, max_unempl), 
                        breaks = seq(min_unempl, max_unempl, length.out = 5))
ggsave(path = "/Users/morgan/Documents/work/uva/non-dspg/", device = "png", filename = "county.png", plot = last_plot())

acs_tract$printname <- paste0("Census Tract", " ", acs_tract$NAME.x)
ggplot(acs_tract) +
  geom_sf(data = acs_tract, size = 0.2) +
  geom_sf_label(aes(label = printname)) + 
  labs(title = "Census Tract\nPatrick County, Virginia") +
  theme_map() + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_unempl, max_unempl), 
                        breaks = seq(min_unempl, max_unempl, length.out = 5))
ggsave(path = "/Users/morgan/Documents/work/uva/non-dspg/", device = "png", filename = "tract.png", plot = last_plot())

acs_bgrp$NAME.y <- str_replace_all(acs_bgrp$NAME.y, ", Patrick County, Virginia", "")
acs_bgrp$NAME.y <- str_replace_all(acs_bgrp$NAME.y, "Census ", "")
ggplot(acs_bgrp) +
  geom_sf(data = acs_bgrp, size = 0.2) +
  geom_sf_label_repel(
    aes(label = NAME.y),
    # additional parameters are passed to geom_label_repel()
    nudge_x = -5, nudge_y = -5, seed = 10
  ) + 
  labs(title = "Census Block Group\nPatrick County, Virginia") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "right") +
  scale_fill_continuous(name = "Percent", low = "#fee6ce", high = "#e6550d",
                        limits = c(min_unempl, max_unempl), 
                        breaks = seq(min_unempl, max_unempl, length.out = 5))
ggsave(path = "/Users/morgan/Documents/work/uva/non-dspg/", device = "png", filename = "bgrp.png", plot = last_plot())


