library(shiny)
library(shinycssloaders)
library(dashboardthemes)
library(shinydashboard)
library(shinydashboardPlus)
library(leaflet)
library(dplyr)
library(readr)
library(readxl)
library(sf)
library(DT)


#
# Read in data ------------------------------------------------
#

# Read in data
data_usda <- read_rds("data/final_usda.rds")
data_older <- read_rds("data/final_older.rds")
data_socdem <- read_rds("data/final_socdem.rds")
data_bband <- read_rds("data/final_internet.rds")
data_corelogic <- read_rds("data/final_corelogic.rds")

data_ems <- read_rds("data/final_ems_forapp.rds")
data_ems8_county <- read_rds("data/final_ems_8_countywide_coverage.rds")
data_ems10_county <- read_rds("data/final_ems_10_countywide_coverage.rds")
data_ems12_county <- read_rds("data/final_ems_12_countywide_coverage.rds")

data_wifi <- read_rds("data/final_wifi_forapp.rds")
data_wifi10_county <- read_rds("data/final_wifi_10_countywide_coverage.rds")
data_wifi15_county <- read_rds("data/final_wifi_15_countywide_coverage.rds")

data_food <- read_rds("data/final_foodretail_forapp.rds")
data_food10_county <- read_rds("data/final_food_10_countywide_coverage.rds")
data_food15_county <- read_rds("data/final_food_15_countywide_coverage.rds") 

data_borders <- read_rds("data/final_countyborders.rds")

data_measures <- read_excel("data/measures.xlsx")


#
# Get available data county vectors by dataset ------------------------------------------------
#

countylist_usda <- sort(unique(data_usda$county))
countylist_older <- sort(unique(data_older$county))
countylist_socdem <- sort(unique(data_socdem$county))
countylist_bband <- sort(unique(data_bband$county))

countylist_ems <- sort(unique(data_ems$county))
countylist_wifi <- sort(unique(data_wifi$county))
countylist_food <- sort(unique(data_food$county))


#
# Create variable choice vectors ------------------------------------------------
#

# USDA
choices_usda <- c("lapop1share", "lapop10share", "lakids1share", "lakids10share",
                   "laseniors1share", "laseniors10share", "lalowi1share", "lalowi10share")

names(choices_usda) <- c("Percent Population with Low Food Access at 1 Mile", "Percent Population with Low Food Access at 10 Miles",
                         "Percent Children with Low Food Access at 1 Mile", "Percent Children with Low Food Access at 10 Miles",
                         "Percent Older Adults with Low Food Access at 1 Mile", "Percent Older Adults with Low Food Access at 10 Miles",
                         "Percent Low Income Population with Low Food Access at 1 Mile", "Percent Low Income Population with Low Food Access at 10 Miles")

# Sociodemographics
choices_socdem <- c("totalpop_trct", "age65", "under18", "hispanic", "black", "noba", "unempl", "inpov", "nohealthins",
                   "publicins", "privateins", "snap")
  
names(choices_socdem) <- c("Total Tract Population", "Percent Population Age 65 and Older", "Percent Population Age 18 and Younger",
                          "Percent Population Hispanic", "Percent Population Black", "Percent Working-Age Population Without Bachelor's Degree",
                          "Percent Population In Labor Force Unemployed", "Percent Population in Poverty", 
                          "Percent Population Without Health Insurance", "Percent Population With Public Health Insurance",
                          "Percent Population With Private Health Insurance", "Percent Population Receiving SNAP Benefits or Public Assistance")


# Older adults
choices_older <- c("older", "nohealthins", "visdiff", "heardiff", "cogdiff", "ambdiff", "carediff", "ildiff", "disab",
                   "inpov", "snap", "labfor", "hhsixty_total", "hhsixty_marr", "hhsixty_single", "hhsixty_nonfam"
)

names(choices_older) <- c("Percent Population Age 65 or Older", "Percent Older Adults without Health Insurance",
                          "Percent Older Adults with Vision Difficulty", "Percent Older Adults with Hearing Difficulty", "Percent Older Adults with Cognitive Difficulty",
                          "Percent Older Adults with Ambulatory Difficulty", "Percent Older Adults with Self-Care Difficulty", "Percent Older Adults with Independent Living Difficulty",
                          "Percent Older Adults with Any Disability", "Percent Older Adults with Income Below Poverty Level", "Percent Households with Members Age 60 or Older Receiving SNAP",
                          "Percent Older Adults in Labor Force", "Percent Households with Members Age 60 or Older", "Percent Married Couple Households with Members Age 60 or Older",
                          "Percent Single Households with Householder Age 60 or Older", "Percent Non-Family Households with Members Age 60 or Older")


# Broadband
choices_bband <- c("nocomputer", "laptop", "smartphone", "tablet", "othercomputer",
                      "nointernet", "satellite", "cellular", "dialup", "broadband")

names(choices_bband) <- c("Percent Households without a Computer", "Percent Households with a Desktop or Laptop Computer",
                             "Percent Households with a Smartphone", "Percent Households with a Tablet Computer",
                             "Percent Households with Other Type of Computer",
                             "Percent Households without Internet Access", "Percent Households with Satellite Internet",
                             "Percent Households with Cellular Internet", "Percent Households with Dial-Up Internet",
                             "Percent Households with Broadband Internet")


#
# Other prep --------------------------------------------
#

# Fix leaflet legend NA issue
css_fix <- "div.info.legend.leaflet-control br {clear: both;}"
html_fix <- as.character(htmltools::tags$style(type = "text/css", css_fix))

# Spinner
options(spinner.color = "#f0f0f0", spinner.color.background = "#ffffff", spinner.size = 3, spinner.type = 7)


#
# User interface --------------------------------------------
#

ui <- dashboardPage(
  
  dashboardHeader(
    titleWidth = "40%",
    title = "Access Barriers to Health in Rural Virginia"
  ),
  
  dashboardSidebar(
    collapsed = FALSE,
    width = "300px",
    sidebarMenu(menuItem(text = "Home", 
                         tabName = "home",
                         icon = icon("info-circle")),
                hr(),
                menuItem(startExpanded = F,
                         text = "Community Context", 
                         icon = icon("info-circle"),
                         menuSubItem(text = "Population Characteristics", tabName = "population", icon = NULL),
                         menuSubItem(text = "Older Adult Characteristics", tabName = "olderadult", icon = NULL)),
                menuItem(startExpanded = F,
                         text = "Food Access", 
                         icon = icon("info-circle"),
                         menuSubItem(text = "Food Security", tabName = "foodsec", icon = NULL),
                         menuSubItem(text = "Food Retail", tabName = "foodretail", icon = NULL)),
                menuItem(startExpanded = F,
                         text = "Internet Access", 
                         icon = icon("info-circle"),
                         menuSubItem(text = "Broadband", tabName = "bband", icon = NULL),
                         menuSubItem(text = "Free Wi-Fi Hotspots", tabName = "wifi", icon = NULL)),
                menuItem(startExpanded = F,
                         text = "Health Access", 
                         icon = icon("info-circle"),
                         menuSubItem(text = "Emergency Medical Service Stations", tabName = "ems", icon = NULL)),
                hr(),
                menuItem(startExpanded = F,
                         text = "Data and Methods", 
                         icon = icon("info-circle"),
                         menuSubItem(text = "Measures", tabName = "measures", icon = NULL),
                         menuSubItem(text = "Data Sources", tabName = "data", icon = NULL)),
                menuItem(startExpanded = F,
                         text = "About", 
                         icon = icon("info-circle"),
                         menuSubItem(text = "This Project", tabName = "thisproject", icon = NULL),
                         menuSubItem(text = "Contact", tabName = "contact", icon = NULL))
    )),
  
  dashboardBody(
    
    shinyDashboardThemes(
      theme = "grey_light"
    ),
    
    tags$head(tags$style('.selectize-dropdown {z-index: 10000}')), # dropdown fix
    HTML(html_fix), # leaflet NA fix
    
    tabItems(
      
      #
      # MENU: Home ----------------------------------------------
      #
      
      tabItem(tabName = "home",
              fluidRow(style = "margin: 6px;",
                       h1(strong("Community Learning Through Data Driven Discovery: Barriers to Rural Health"), align = "center"),
                       br(),
                       p("Rural counties often face challenges in providing health care access to its residents given few health facilities available, lack of broadband infrastructure that limits providing telemedicine access or communicating health information, 
                          and individual-level inequalities that pose barriers to health care access and use. Identifying areas of high need or potential solutions may also be difficult for rural areas without adequate resources to acquire, analyze, and interpret 
                          relevant data. This project builds local capacity, leveraging social and data science to construct a rural county dashboard enhancing data-driven health access decision making in rural Virginia."),
                       p("This project is one of eight funded by the", a(href = "https://impact.extension.org/ntae/", "2020/21 NIFA NTAE grant", target = "_blank"), "through the University of Virginia, in collaboration with Virginia Cooperative Extension, Virginia Tech and implemented by the eXtension Foundation.")
              )
      ),
      
      #
      # SUBMENU: Community - sociodemographics ----------------------------------------------
      #
      
      tabItem(tabName = "population",
              fluidRow(style = "margin: 6px;",
                       h1(strong("Population Characteristics"), align = "center"),
                       br(),
                       selectInput("whichcounty_socdem", "Select County:", 
                                   selected = "Accomack County",
                                   multiple = F, width = "100%", choices = c(countylist_socdem)),
                       selectInput("whichvar_socdem", "Select Variable:", width = "100%", choices = choices_socdem),
                       br(),
                       withSpinner(leafletOutput("plot_socdem")),
                       p(tags$small("Data Source: American Community Survey 2014/18 5-Year Estimates."))
                       )
      ),
      
      #
      # SUBMENU: Community - older adult ----------------------------------------------
      #
      
      tabItem(tabName = "olderadult",
              fluidRow(style = "margin: 6px;",
                       h1(strong("Older Adult Characteristics"), align = "center"),
                       br(),
                       selectInput("whichcounty_older", "Select County:", 
                                   selected = "Accomack County",
                                   multiple = F, width = "100%", choices = c(countylist_older)),
                       selectInput("whichvar_older", "Select Variable:", width = "100%", choices = choices_older),
                       br(),
                       withSpinner(leafletOutput("plot_older")),
                       p(tags$small("Data Source: American Community Survey 2014/18 5-Year Estimates."))
              )
      ),
      
      #
      # SUBMENU: Access - food security--------------------------------------------------------------------------
      #
      
      tabItem(tabName = "foodsec",
              fluidRow(style = "margin: 6px;",
                       h1(strong("Food Security"), align = "center"),
                       br(),
                       selectInput("whichcounty_usda", "Select County:", 
                                   selected = "Accomack County",
                                   multiple = F, width = "100%", choices = c(countylist_usda)),
                       selectInput("whichvar_usda", "Select Variable:", width = "100%", choices = choices_usda),
                       br(),
                       withSpinner(leafletOutput("plot_usda")),
                       p(tags$small("Data Source: USDA Food Access Research Atlas, 2017."))
              )
      ),
      
      
      #
      # SUBMENU: Access - food retail --------------------------------------------------------------------------
      #
      
      tabItem(tabName = "foodretail",
              fluidRow(style = "margin: 6px;",
                       h1(strong("Food Retail Access"), align = "center"),
                       br(),
                       selectInput("whichcounty_food", "Select County:", 
                                   selected = "Accomack County",
                                   multiple = F, width = "100%", choices = c(countylist_food)),
                       br(),
                       column(width = 9, 
                              withSpinner(leafletOutput("plot_food_iso_county", height = "600px")),
                              p(tags$small("Data Sources: CoreLogic, 2019; MarketMaker, 2019; OpenStreetMap, 2021."))
                       ),
                       column(width = 3,
                              fluidRow(valueBoxOutput("box_food_countywide_10", width = "100%")),
                              fluidRow(valueBoxOutput("box_food_countywide_15", width = "100%")))
              )
      ),
      
      
      #
      # SUBMENU: Access - broadband --------------------------------------------------------------------------
      #
      
      tabItem(tabName = "bband",
              fluidRow(style = "margin: 6px;",
                       h1(strong("Internet and Computer Access"), align = "center"),
                       br(),
                       selectInput("whichcounty_bband", "Select County:", 
                                   selected = "Accomack County",
                                   multiple = F, width = "100%", choices = c(countylist_bband)),
                       selectInput("whichvar_bband", "Select Variable:", width = "100%", choices = choices_bband),
                       br(),
                       withSpinner(leafletOutput("plot_bband")),
                       p(tags$small("Data Source: American Community Survey 2014/18 5-Year Estimates."))
              )
      ),
      
      
      #
      # SUBMENU: Access - wifi --------------------------------------------------------------------------
      #
      
      tabItem(tabName = "wifi",
              fluidRow(style = "margin: 6px;",
                       h1(strong("Free Wi-Fi Hotspots"), align = "center"),
                       br(),
                       selectInput("whichcounty_wifi", "Select County:", 
                                   selected = "Accomack County",
                                   multiple = F, width = "100%", choices = c(countylist_wifi)),
                       br(),
                       column(width = 9, 
                              withSpinner(leafletOutput("plot_wifi_iso_county", height = "600px")),
                              p(tags$small("Data Sources: CoreLogic, 2019; CommonwealthConnect, 2020; OpenStreetMap, 2021."))
                       ),
                       column(width = 3,
                              fluidRow(valueBoxOutput("box_wifi_countywide_10", width = "100%")),
                              fluidRow(valueBoxOutput("box_wifi_countywide_15", width = "100%")))
              )
      ),
     
      
      #
      # SUBMENU: Access - EMS --------------------------------------------------------------------------
      #
      
      tabItem(tabName = "ems",
              fluidRow(style = "margin: 6px;",
                       h1(strong("Emergency Medical Services Station Access"), align = "center"),
                       br(),
                       selectInput("whichcounty_ems", "Select County:", 
                                   selected = "Accomack County",
                                   multiple = F, width = "100%", choices = c(countylist_ems)),
                       br(),
                       column(width = 9, 
                          withSpinner(leafletOutput("plot_ems_iso_county", height = "600px")),
                          p(tags$small("Data Sources: CoreLogic, 2019; Homeland Infrastructure Foundation-Level Data, 2019; OpenStreetMap, 2021."))
                          ),
                       column(width = 3,
                              fluidRow(valueBoxOutput("box_ems_countywide_8", width = "100%")),
                              fluidRow(valueBoxOutput("box_ems_countywide_10", width = "100%")),
                              fluidRow(valueBoxOutput("box_ems_countywide_12", width = "100%")))
              )
      ),
      
      
      #
      # SUBMENU: Data and methods - Measures --------------------------------------------------------------------------
      #
      
      tabItem(tabName = "measures",
              fluidRow(style = "margin: 6px;",
                       h1(strong("Measures"), align = "center"),
                       br(),
                       selectInput("measurestopic", "Select Topic:", width = "100%", 
                                   choices = c(
                                     "All Measures",
                                     "Sociodemographic Measures",
                                     "Older Adult Population Measures",
                                     "Connectivity Measures",
                                     "Food Access Measures",
                                     "Health Care Access Measures")
                       ),
                       DTOutput("measurestable")
              )
      ),
      
      
      #
      # SUBMENU: Data and methods - Data  --------------------------------------------------------------------------
      #
      
      tabItem(tabName = "data",
              fluidRow(style = "margin: 6px;",
                       h1(strong("Data Sources"), align = "center"),
                       br(),
                       column(12,
                              box(img(src = "data-hifld.png", style = "display: inline; float: left; margin-right:20px; margin-bottom: 20px;", width = "100px"),
                                  p(strong("Homeland Infrastructure Foundation-Level Data."), "Homeland Infrastructure Foundation-Level Data (HIFLD) is a collection of public
                                  source datasets at property level provided by the Department of Homeland Security. Since 2002, this HIFLD has provided quarterly
                                  updated layers on topics from education to energy, including on health care facilities. We used HIFLD emergency medical services
                                  station data at the latitude and longitude geographic level in our analyses."), width = 12
                              ),
                              box(img(src = "data-marketmaker.png", style = "display: inline; float: left; margin-right:20px; margin-bottom: 20px;", width = "200px"),
                                  p(strong("MarketMaker."), "MarketMaker is a networking site that connects producers across America. Its food retail dataset includes 
                                  locations of supermarkets, farmers' markets, and grocery stores. We use their 2019 data set to retrieve food retail location addresses
                                  in rural Virginia and subsequently geocode them to obtain longitude and latitude coordinates."), width = 12
                              ),
                              box(img(src = "data-acs.png", style = "display: inline; float: left; margin-right:20px; margin-bottom: 20px;", width = "200px"),
                                  p(strong("American Community Survey."), "The American Community Survey (ACS) is an ongoing yearly survey conducted by the U.S Census
                                  Bureau. ACS samples households to compile 1-year and 5-year datasets providing information on population sociodemographic and
                                  socioeconomic characteristics including employment, disability, and health insurance coverage. We used AC 2014/18 5-year
                                  estimates to obtain census tract and census block group-level to explore rural Virginia county resident characteristics."), width = 12
                              ),
                              box(img(src = "data-connect.png", style = "display: inline; float: left; margin-right:20px; margin-bottom: 20px;", width = "150px"),
                                  p(strong("CommonwealthConnect."), "The Virginia Tech CommonwealthConnect Wi-Fi Hotspot Map is an interactive map of free, publicly
                                  available wi-fi hotspots in Virginia. Its goal is to provide an easily accessible map of areas where individuals can connect to the
                                  internet for free, decreasing the constraints placed on families that do not have internet access at home. We used the 2020 wi-fi
                                  hotspot map data to retrieve hotspot locations in rural Virginia counties and subsequently employed the information in calculating hotspot
                                  coverage isochrones."), width = 12
                              ),
                              box(img(src = "data-corelogic.png", style = "display: inline; float: left; margin-right:20px; margin-bottom: 20px;", width = "120px"),
                                  p(strong("CoreLogic."), "CoreLogic is a supplier of proprietary US real estate and specialized business data at the property level.
                                  This company provides data spanning over 50 years at the latitude and longitude level. Information available in the dataset includes
                                  property characteristics, mortgage, foreclosures and performance. We used 2019 CoreLogic data to obtain the locations of all residential
                                  properties in rural Virginia counties."), width = 12
                              ),
                              box(img(src = "data-ers.png", style = "display: inline; float: left; margin-right:20px; margin-bottom: 20px;", width = "120px"),
                                  p(strong("Food Access Research Atlas."), "The United State Department of Agriculture Food Access Research Atlas is a data resource
                                  created by the Economic Research Service that provides information on food access indicators at census tract level. The data allows
                                  individuals to understand food access in communities based on factors like age and socioeconomic status. We used the 2017 Food Access
                                  Research Atlas to examine residents’ food access at multiple distance thresholds and by resident characteristics."), width = 12
                              )
                       )
              )
      ),
      
      
      #
      # SUBMENU: About - This project -----------------------------------------------------------------------
      #
      
      tabItem(tabName = "thisproject",
              fluidRow(style = "margin: 6px;",
                       h1(strong("Community Learning Through Data Driven Discovery: Barriers to Rural Health"), align = "center"),
                       br(),
                       p("Rural counties often face challenges in providing health care access to its residents given few health facilities available, lack of broadband infrastructure that limits providing telemedicine access or communicating health information, 
                          and individual-level inequalities that pose barriers to health care access and use. Identifying areas of high need or potential solutions may also be difficult for rural areas without adequate resources to acquire, analyze, and interpret 
                          relevant data. This project builds local capacity, leveraging social and data science to construct a rural county dashboard enhancing data-driven health access decision making in rural Virginia."),
                       p("This project is one of eight funded by the", a(href = "https://impact.extension.org/ntae/", "2020/21 NIFA NTAE grant", target = "_blank"), "through the University of Virginia, in collaboration with Virginia Cooperative Extension, Virginia Tech and implemented by the eXtension Foundation.")
                       )
      ),
      
      
      #
      # SUBMENU: About - Contact -----------------------------------------------------------------------
      #
      
      tabItem(tabName = "contact",
              fluidRow(style = "margin: 6px;",
                       h1(strong("Contact"), align = "center"),
                       br(),
                       p(a(href = "https://biocomplexity.virginia.edu/person/teja-pristavec", "Teja Pristavec", target = "_blank"), "(Research Assistant Professor, Biocomplexity Institute, University of Virginia)"),
                       p(a(href = "https://news.cals.vt.edu/experts/2015/06/02/mike-lambur/", "Mike Lambur", target = "_blank"), "(Associate Director of Program Development, Virginia Cooperative Extension, Virginia Tech)")
                       )
      )
    )
  )
)


#
# Server ------------------------------------------------------------------
#

server <- function(input, output){
  
  
  #
  # OPTIONS ------------------------------------------------------
  #
  
  map_colors <- c('#efe1c6', '#dccdb3', '#c9b8a0', '#b8a58d', '#a89179', '#987d65', '#896a52', '#7a573e', '#6b452b')
  
  
  #
  # FUNCTION: Map: Base maps ------------------------------------------
  #
  
  create_plot <- function(data, myvar, myvarlabel) {
    
    pal <- colorNumeric(map_colors, domain = myvar, na.color = "grey")
    
    labels <- lapply(
      paste("<strong>Area: </strong>",
            data$areaname,
            "<br />",
            "<strong>", myvarlabel, ": </strong>",
            round(myvar, 2)),
      htmltools::HTML
    )
    
    leaflet(data = data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(fillColor = ~pal(myvar), 
                  fillOpacity = 0.8, 
                  stroke = TRUE, smoothFactor = 0.8, weight = 0.5, color = "#202020",
                  label = labels,
                  labelOptions = labelOptions(direction = "bottom",
                                              style = list(
                                                "font-size" = "12px",
                                                "border-color" = "rgba(0,0,0,0.5)",
                                                direction = "auto"
                                              ))) %>%
      addLegend("bottomleft",
                pal = pal,
                values =  ~(myvar),
                title = "Value",
                opacity = 0.8,
                na.label = "Not Available")
  }
  
  
  #
  # FUNCTION: Map: Countywide isochrones ----------------------------------
  #

  # For 8, 10, 12
  create_plot_countywide3 <- function(data_labels, data_county_borders, data_county_points, data_county_residences, 
                                     data_county_8, data_county_10, data_county_12) {
    
    colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")
    
    m1 <- leaflet(options = leafletOptions()) %>% #minZoom = 10
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = data_county_borders,
                  stroke = T, weight = 2, color = "grey", fillOpacity = 0) %>%
      addCircles(data = data_county_residences, 
                 fillColor = colors[5],
                 fillOpacity = .8, 
                 stroke = FALSE, 
                 group = "Residential Properties") %>%
      addPolygons(data = data_county_8, 
                  fillColor = colors[1],
                  fillOpacity = .8, 
                  stroke = FALSE, 
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = data_county_10, 
                  fillColor = colors[1],
                  fillOpacity = .8, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = data_county_12, 
                  fillColor = colors[1],
                  fillOpacity = .8, 
                  stroke = FALSE, 
                  group = "12 Minute Isochrones") %>%
      addMarkers(data = data_county_points,
                 label = data_labels,
                 labelOptions = labelOptions(direction = "bottom",
                                             style = list(
                                               "font-size" = "12px",
                                               "border-color" = "rgba(0,0,0,0.5)",
                                               direction = "auto")))  %>%
      addLayersControl(
        position = "topright",
        overlayGroups = c("8 Minute Isochrones",
                          "10 Minute Isochrones",
                          "12 Minute Isochrones",
                          "Residential Properties"),
        options = layersControlOptions(collapsed = FALSE))
    
    m1 
  }
  
  # For 10, 15
  create_plot_countywide2 <- function(data_labels, data_county_borders, data_county_points, data_county_residences, 
                                      data_county_10, data_county_15) {
    
    colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")
    
    m1 <- leaflet(options = leafletOptions()) %>% #minZoom = 10
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = data_county_borders,
                  stroke = T, weight = 2, color = "grey", fillOpacity = 0) %>%
      addCircles(data = data_county_residences, 
                 fillColor = colors[5],
                 fillOpacity = .8, 
                 stroke = FALSE, 
                 group = "Residential Properties") %>%
      addPolygons(data = data_county_10, 
                  fillColor = colors[1],
                  fillOpacity = .8, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = data_county_15, 
                  fillColor = colors[1],
                  fillOpacity = .8, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addMarkers(data = data_county_points,
                 label = data_labels,
                 labelOptions = labelOptions(direction = "bottom",
                                             style = list(
                                               "font-size" = "12px",
                                               "border-color" = "rgba(0,0,0,0.5)",
                                               direction = "auto")))  %>%
      addLayersControl(
        position = "topright",
        overlayGroups = c("10 Minute Isochrones",
                          "15 Minute Isochrones",
                          "Residential Properties"),
        options = layersControlOptions(collapsed = FALSE))
    
    m1 
  }
  
  
  #
  # FUNCTION: Value box: Countywide isochrones ----------------------------------
  #
  
  create_countywide_coverage <- function(data, coveragelabel, iconname) {
    
  valueBox(
    paste0(round(data, 2), "%"), coveragelabel, icon = icon(iconname),
    color = "olive", width = "100%"
  )
    
  }
  
  
  #
  # OUTPUT: Countywide Coverage Box - EMS ----------------------------------------
  #
  
  box_ems_8 <- reactive({data_ems8_county %>% filter(county == input$whichcounty_ems) %>% pull(ems_countywide_coverage_8)})
  box_ems_10 <- reactive({data_ems10_county %>% filter(county == input$whichcounty_ems) %>% pull(ems_countywide_coverage_10)})
  box_ems_12 <- reactive({data_ems12_county %>% filter(county == input$whichcounty_ems) %>% pull(ems_countywide_coverage_12)})
   
  output$box_ems_countywide_8 <- renderValueBox({
    create_countywide_coverage(box_ems_8(), "Coverage at 8 Minute Drive", "fas fa-ambulance")
  })
  output$box_ems_countywide_10 <- renderValueBox({
    create_countywide_coverage(box_ems_10(), "Coverage at 10 Minute Drive", "fas fa-ambulance")
  })
  output$box_ems_countywide_12 <- renderValueBox({
    create_countywide_coverage(box_ems_12(), "Coverage at 12 Minute Drive", "fas fa-ambulance")
  })
  
  
  #
  # OUTPUT: Countywide Coverage Box - WIFI ----------------------------------------
  #
  
  box_wifi_10 <- reactive({data_wifi10_county %>% filter(county == input$whichcounty_wifi) %>% pull(wifi_countywide_coverage_10)})
  box_wifi_15 <- reactive({data_wifi15_county %>% filter(county == input$whichcounty_wifi) %>% pull(wifi_countywide_coverage_15)})
  
  output$box_wifi_countywide_10 <- renderValueBox({
    create_countywide_coverage(box_wifi_10(), "Coverage at 10 Minute Drive", "fas fa-wifi")
  })
  output$box_wifi_countywide_15 <- renderValueBox({
    create_countywide_coverage(box_wifi_15(), "Coverage at 15 Minute Drive", "fas fa-wifi")
  })
  
  
  #
  # OUTPUT: Countywide Coverage Box - FOOD RETAIL ----------------------------------------
  #
  
  box_food_10 <- reactive({data_food10_county %>% filter(county == input$whichcounty_food) %>% pull(food_countywide_coverage_10)})
  box_food_15 <- reactive({data_food15_county %>% filter(county == input$whichcounty_food) %>% pull(food_countywide_coverage_15)})
  
  output$box_food_countywide_10 <- renderValueBox({
    create_countywide_coverage(box_food_10(), "Coverage at 10 Minute Drive", "fas fa-bread-slice")
  })
  output$box_food_countywide_15 <- renderValueBox({
    create_countywide_coverage(box_food_15(), "Coverage at 15 Minute Drive", "fas fa-bread-slice")
  })
  
  
  #
  # OUTPUT: Plot - Countywide isochrones - EMS ------------------------------------------
  #
  
  plot_ems_borders <- reactive({data_borders %>% filter(county == input$whichcounty_ems)})
  plot_ems_points <- reactive({data_ems %>% filter(county == input$whichcounty_ems)})
  plot_ems_residences <- reactive({data_corelogic %>% filter(county == input$whichcounty_ems)})
  plot_ems_8 <- reactive({data_ems8_county %>% filter(county == input$whichcounty_ems)})
  plot_ems_10 <- reactive({data_ems10_county %>% filter(county == input$whichcounty_ems)})
  plot_ems_12 <- reactive({data_ems12_county %>% filter(county == input$whichcounty_ems)})
  
  labels_ems <- reactive({
    lapply(
    paste("<strong>Name: </strong>",
          plot_ems_points()$name,
          "<br />",
          "<strong>Service Type: </strong>",
          plot_ems_points()$naicsdescr,
          "<br />",
          "<strong>Address:</strong>",
          paste0(plot_ems_points()$address, ", ", plot_ems_points()$city, ", VA ", plot_ems_points()$zip)
    ),
    htmltools::HTML
  )
  })
  
  output$plot_ems_iso_county <- renderLeaflet({
    create_plot_countywide3(labels_ems(), plot_ems_borders(), plot_ems_points(), plot_ems_residences(), plot_ems_8(), plot_ems_10(), plot_ems_12())
  })
  
  
  #
  # OUTPUT: Plot - Countywide isochrones - Wifi ------------------------------------------
  #
  
  plot_wifi_borders <- reactive({data_borders %>% filter(county == input$whichcounty_wifi)})
  plot_wifi_points <- reactive({data_wifi %>% filter(county == input$whichcounty_wifi)})
  plot_wifi_residences <- reactive({data_corelogic %>% filter(county == input$whichcounty_wifi)})
  plot_wifi_10 <- reactive({data_wifi10_county %>% filter(county == input$whichcounty_wifi)})
  plot_wifi_15 <- reactive({data_wifi15_county %>% filter(county == input$whichcounty_wifi)})
  
  labels_wifi <- reactive({
    lapply(
    paste("<strong>Name: </strong>",
          plot_wifi_points()$name,
          "<br />",
          "<strong>County: </strong>",
          plot_wifi_points()$county,
          "<br />",
          "<strong>GEOID: </strong>",
          plot_wifi_points()$GEOID,
          "<br />",
          "<strong>Address:</strong>",
          paste0(plot_wifi_points()$address, ", ", plot_wifi_points()$city_1, ", VA ", plot_wifi_points()$zip_code)
    ),
    htmltools::HTML
  )
  })
  
  output$plot_wifi_iso_county <- renderLeaflet({
    create_plot_countywide2(labels_wifi(), plot_wifi_borders(), plot_wifi_points(), plot_wifi_residences(), plot_wifi_10(), plot_wifi_15())
  })
  
  
  #
  # OUTPUT: Plot - Countywide isochrones - Food retail ------------------------------------------
  #
  
  plot_food_borders <- reactive({data_borders %>% filter(county == input$whichcounty_food)})
  plot_food_points <- reactive({data_food %>% filter(county == input$whichcounty_food)})
  plot_food_residences <- reactive({data_corelogic %>% filter(county == input$whichcounty_food)})
  plot_food_10 <- reactive({data_food10_county %>% filter(county == input$whichcounty_food)})
  plot_food_15 <- reactive({data_food15_county %>% filter(county == input$whichcounty_food)})
  
  labels_food <- reactive({
    lapply(
    paste("<strong>Name: </strong>",
          plot_food_points()$business,
          "<br />",
          "<strong>Type: </strong>",
          plot_food_points()$profiles,
          "<br />",
          "<strong>FIPS: </strong>",
          plot_food_points()$FIPS,
          "<br />",
          "<strong>County: </strong>",
          plot_food_points()$county,
          "<br />",
          "<strong>Address: </strong>",
          paste0(plot_food_points()$address1, ", ", plot_food_points()$city, ", VA ", plot_food_points()$zip)
    ),
    htmltools::HTML
  )
  })
  
  output$plot_food_iso_county <- renderLeaflet({
    create_plot_countywide2(labels_food(), plot_food_borders(), plot_food_points(), plot_food_residences(), plot_food_10(), plot_food_15())
  })
  
  
  #
  # OUTPUT: Plot - USDA ------------------------------------------
  #
  
  
  plot_usda_data <- reactive({data_usda %>% filter(county == input$whichcounty_usda)})
  plot_usda_var <- reactive({plot_usda_data()[[input$whichvar_usda]]})
    
  output$plot_usda <- renderLeaflet({

    var_label <- names(choices_usda)[choices_usda == input$whichvar_usda]
    
    create_plot(plot_usda_data(), plot_usda_var(), var_label)
  })

  
  #
  # OUTPUT: Plot - Older adults ------------------------------------------
  #
  
  plot_older_data <- reactive({data_older %>% filter(county == input$whichcounty_older)})
  plot_older_var <- reactive({plot_older_data()[[input$whichvar_older]]})
  
  output$plot_older <- renderLeaflet({
    
    var_label <- names(choices_older)[choices_older == input$whichvar_older]
    
    create_plot(plot_older_data(), plot_older_var(), var_label)
  })
  
  
  #
  # OUTPUT: Plot - Sociodemographics ------------------------------------------
  #
  
  plot_socdem_data <- reactive({data_socdem %>% filter(county == input$whichcounty_socdem)})
  plot_socdem_var <- reactive({plot_socdem_data()[[input$whichvar_socdem]]})
  
  output$plot_socdem <- renderLeaflet({
    
    var_label <- names(choices_socdem)[choices_socdem == input$whichvar_socdem]
    
    create_plot(plot_socdem_data(), plot_socdem_var(), var_label)
  })
  
  
  #
  # OUTPUT: Plot - Broadband ------------------------------------------
  #
  
  plot_bband_data <- reactive({data_bband %>% filter(county == input$whichcounty_bband)})
  plot_bband_var <- reactive({plot_bband_data()[[input$whichvar_bband]]})
  
  output$plot_bband <- renderLeaflet({
    
    var_label <- names(choices_older)[choices_bband == input$whichvar_bband]
    
    create_plot(plot_bband_data(), plot_bband_var(), var_label)
  })
  
  
  #
  # OUTPUT: Measures table ------------------------------------------
  #
  
  var_topic <- reactive({
    input$measurestopic
  })
  
  output$measurestable <- renderDataTable({
    if(var_topic() == "All Measures"){
      table <- as.data.frame(data_measures)
      datatable(table, rownames = FALSE, options = list(pageLength = 15)) %>% 
        formatStyle(0, target = "row", lineHeight = "80%")
    }
    else{
      data <- switch(input$measurestopic,
                     "Connectivity Measures" = "connectivity",
                     "Sociodemographic Measures" = "demographics",
                     "Food Access Measures" = "food access",
                     "Health Care Access Measures" = "health",
                     "Older Adult Population Measures" = "older adults")
      table <- subset(data_measures, Topic == data)
      table <- as.data.frame(table)
      datatable(table, rownames = FALSE, options = list(pageLength = 15)) %>% 
        formatStyle(0, target = "row", lineHeight = "80%")
    }
  })
  
}


#
# App ------------------------------------------------------------------
#

shinyApp(ui = ui, server = server)



