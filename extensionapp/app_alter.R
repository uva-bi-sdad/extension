library(shiny)
library(dashboardthemes)
library(shinydashboard)
library(shinydashboardPlus)
library(leaflet)
library(dplyr)
library(readr)
library(readxl)
library(DT)


#
# Prepare ------------------------------------------------
#

# Read in data
data_usda <- read_rds("data/final_usda.rds")
data_measures <- read_excel("data/measures.xlsx")

# Get county vectors by dataset
countylist_usda <- unique(data_usda$County)


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
                         tabName = "home"),
                menuItem(startExpanded = F,
                         text = "Explore Community Context", 
                         icon = icon("info-circle"),
                         menuSubItem(text = "Population Characteristics", tabName = "population", icon = NULL),
                         menuSubItem(text = "Older Adult Characteristics", tabName = "olderadult", icon = NULL)),
                menuItem(startExpanded = F,
                         text = "Explore Access", 
                         icon = icon("info-circle"),
                         menuSubItem(text = "Food Retail", tabName = "food", icon = NULL),
                         menuSubItem(text = "Free Wi-Fi Hotspots", tabName = "wifi", icon = NULL),
                         menuSubItem(text = "Emergency Medical Service Stations", tabName = "ems", icon = NULL)),
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
    
    tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
    
    tabItems(
      
      #
      # MENU: Home ----------------------------------------------
      #
      
      tabItem(tabName = "home",
              fluidRow(style = "margin: 6px;",
                       "This is the splash page.")
      ),
      
      #
      # SUBMENU: Community - population ----------------------------------------------
      #
      
      tabItem(tabName = "population",
              fluidRow(style = "margin: 6px;")
      ),
      
      #
      # SUBMENU: Community - older adult ----------------------------------------------
      #
      
      tabItem(tabName = "olderadult",
              fluidRow(style = "margin: 6px;")
      ),
      
      #
      # SUBMENU: Access - food --------------------------------------------------------------------------
      #
      
      tabItem(tabName = "food",
              fluidRow(style = "margin: 6px;",
                       h1(strong("Food Security"), align = "center"),
                       br(),
                       selectInput("whichcounty_usda", "Select County:", 
                                   selected = "Accomack",
                                   multiple = F, width = "100%", choices = c(countylist_usda)),
                       selectInput("whichvar_usda", "Select Variable:", width = "100%", choices = c(
                         "Percent Population with Low Food Access at 1 Mile" = "lapop1share",
                         "Percent Population with Low Food Access at 10 Miles" = "lapop10share",
                         "Percent Children with Low Food Access at 1 Mile" = "lakids1share",
                         "Percent Children with Low Food Access at 10 Miles" = "lakids10share",
                         "Percent Older Adults with Low Food Access at 1 Mile" = "laseniors1share",
                         "Percent Older Adults with Low Food Access at 10 Miles" = "laseniors10share",
                         "Percent Low Income Population with Low Food Access at 1 Mile" = "lalowi1share",
                         "Percent Low Income Population with Low Food Access at 10 Miles" = "lalowi10share")),
                       br(),
                       leafletOutput("plot_usda"))
      ),
      
      #
      # SUBMENU: Access - wifi --------------------------------------------------------------------------
      #
      
      tabItem(tabName = "wifi",
              fluidRow(style = "margin: 6px;")
      ),
      
      #
      # SUBMENU: Access - EMS --------------------------------------------------------------------------
      #
      
      tabItem(tabName = "ems",
              fluidRow(style = "margin: 6px;")
      ),
      
      #
      # SUBMENU: Data and methods - Measures --------------------------------------------------------------------------
      #
      
      tabItem(tabName = "measures",
              fluidRow(style = "margin: 6px;",
                       title = "Measures",
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
                              box(img(src = "data-hifld.png", style = "display: inline; float: left;", width = "100px"),
                                  p(strong("Homeland Infrastructure Foundation-Level Data."), "Homeland Infrastructure Foundation-Level Data (HIFLD) is a collection of public
                                  source datasets at property level provided by the Department of Homeland Security. Since 2002, this HIFLD has provided quarterly
                                  updated layers on topics from education to energy, including on health care facilities. We used HIFLD emergency medical services
                                  station data at the latitude and longitude geographic level in our analyses."), width = 12
                              ),
                              box(img(src = "data-marketmaker.png", style = "display: inline; float: left;", width = "200px"),
                                  p(strong("MarketMaker."), "MarketMaker is a networking site that connects producers across America. Its food retail dataset includes 
                                  locations of supermarkets, farmers' markets, and grocery stores. We use their 2019 data set to retrieve food retail location addresses
                                  in rural Virginia and subsequently geocode them to obtain longitude and latitude coordinates."), width = 12
                              ),
                              box(img(src = "data-acs.png", style = "display: inline; float: left;", width = "200px"),
                                  p(strong("American Community Survey."), "The American Community Survey (ACS) is an ongoing yearly survey conducted by the U.S Census
                                  Bureau. ACS samples households to compile 1-year and 5-year datasets providing information on population sociodemographic and
                                  socioeconomic characteristics including employment, disability, and health insurance coverage. We used AC 2014/18 5-year
                                  estimates to obtain census tract and census block group-level to explore rural Virginia county resident characteristics."), width = 12
                              ),
                              box(img(src = "data-connect.png", style = "display: inline; float: left;", width = "150px"),
                                  p(strong("CommonwealthConnect."), "The Virginia Tech CommonwealthConnect Wi-Fi Hotspot Map is an interactive map of free, publicly
                                  available wi-fi hotspots in Virginia. Its goal is to provide an easily accessible map of areas where individuals can connect to the
                                  internet for free, decreasing the constraints placed on families that do not have internet access at home. We used the 2020 wi-fi
                                  hotspot map data to retrieve hotspot locations in rural Virginia counties and subsequently employed the information in calculating hotspot
                                  coverage isochrones."), width = 12
                              ),
                              box(img(src = "data-corelogic.png", style = "display: inline; float: left;", width = "120px"),
                                  p(strong("CoreLogic."), "CoreLogic is a supplier of proprietary US real estate and specialized business data at the property level.
                                  This company provides data spanning over 50 years at the latitude and longitude level. Information available in the dataset includes
                                  property characteristics, mortgage, foreclosures and performance. We used 2019 CoreLogic data to obtain the locations of all residential
                                  properties in rural Virginia counties."), width = 12
                              ),
                              box(img(src = "data-ers.png", style = "display: inline; float: left;", width = "120px"),
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
              fluidRow(style = "margin: 6px;")
      ),
      
      #
      # SUBMENU: About - Contact -----------------------------------------------------------------------
      #
      
      tabItem(tabName = "contact",
              fluidRow(style = "margin: 6px;")
      )
      
    )
  )
)


#
# Server ------------------------------------------------------------------
#

server <- function(input, output){
  
  #
  # Options ------------------------------------------------------
  #
  
  navBarBlue <- '#427EDC'
  colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")
  
  #
  # Map function ------------------------------------------
  #
  

  
  
  #
  # OUTPUT: Plot - USDA ------------------------------------------
  #
  

  output$plot_usda <- renderLeaflet({
    
    leaflet(data = data_usda[data_usda$County == input$whichcounty_usda, ]) %>%
      addTiles() %>%
      addPolygons()
  })
  
  observeEvent(input$whichcounty_usda, {
    leafletProxy("plot_usda") %>%
      addPolygons(data = data_usda[data_usda$County == input$whichcounty_usda, ])
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



