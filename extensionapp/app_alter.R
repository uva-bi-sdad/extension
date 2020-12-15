library(shiny)
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
data_internet <- read_rds("data/final_internet.rds")

data_measures <- read_excel("data/measures.xlsx")


#
# Get available data county vectors by dataset ------------------------------------------------
#

countylist_usda <- sort(unique(data_usda$county))
countylist_older <- sort(unique(data_older$county))
countylist_internet <- sort(unique(data_internet$county))

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

# Older adult
choices_older <- c("older", "nohealthins", "visdiff", "heardiff", "cogdiff", "ambdiff", "carediff", "ildiff", "disab",
                   "inpov", "snap", "labfor", "hhsixty_total", "hhsixty_marr", "hhsixty_single", "hhsixty_nonfam"
)
  
names(choices_older) <- c("Percent Population Age 65 or Older", "Percent Older Adults without Health Insurance",
  "Percent Older Adults with Vision Difficulty", "Percent Older Adults with Hearing Difficulty", "Percent Older Adults with Cognitive Difficulty",
  "Percent Older Adults with Ambulatory Difficulty", "Percent Older Adults with Self-Care Difficulty", "Percent Older Adults with Independent Living Difficulty",
  "Percent Older Adults with Any Disability", "Percent Older Adults with Income Below Poverty Level", "Percent Households with Members Age 60 or Older Receiving SNAP",
  "Percent Older Adults in Labor Force", "Percent Households with Members Age 60 or Older", "Percent Married Couple Households with Members Age 60 or Older",
  "Percent Single Households with Householder Age 60 or Older", "Percent Non-Family Households with Members Age 60 or Older")

# Internet
choices_internet <- c("nocomputer", "laptop", "smartphone", "tablet", "othercomputer",
                      "nointernet", "satellite", "cellular", "dialup", "broadband")

names(choices_internet) <- c("Percent Households without a Computer", "Percent Households with a Desktop or Laptop Computer",
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
    
    tags$head(tags$style('.selectize-dropdown {z-index: 10000}')), # dropdown fix
    HTML(html_fix), # leaflet NA fix
    
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
              fluidRow(style = "margin: 6px;",
                       h1(strong("Older Adult Characteristics"), align = "center"),
                       br(),
                       selectInput("whichcounty_older", "Select County:", 
                                   selected = "Accomack County",
                                   multiple = F, width = "100%", choices = c(countylist_older)),
                       selectInput("whichvar_older", "Select Variable:", width = "100%", choices = choices_older),
                       br(),
                       leafletOutput("plot_older"))
      ),
      
      #
      # SUBMENU: Access - food --------------------------------------------------------------------------
      #
      
      tabItem(tabName = "food",
              fluidRow(style = "margin: 6px;",
                       h1(strong("Food Security"), align = "center"),
                       br(),
                       selectInput("whichcounty_usda", "Select County:", 
                                   selected = "Accomack County",
                                   multiple = F, width = "100%", choices = c(countylist_usda)),
                       selectInput("whichvar_usda", "Select Variable:", width = "100%", choices = choices_usda),
                       br(),
                       leafletOutput("plot_usda"))
      ),
      
      #
      # SUBMENU: Access - wifi --------------------------------------------------------------------------
      #
      
      tabItem(tabName = "wifi",
              fluidRow(style = "margin: 6px;",
                       h1(strong("Internet and Computer Access"), align = "center"),
                       br(),
                       selectInput("whichcounty_internet", "Select County:", 
                                   selected = "Accomack County",
                                   multiple = F, width = "100%", choices = c(countylist_internet)),
                       selectInput("whichvar_internet", "Select Variable:", width = "100%", choices = choices_internet),
                       br(),
                       leafletOutput("plot_internet"))
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
  
  map_colors <- c('#efe1c6', '#dccdb3', '#c9b8a0', '#b8a58d', '#a89179', '#987d65', '#896a52', '#7a573e', '#6b452b')
  
  
  #
  # Map function ------------------------------------------
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
  # OUTPUT: Plot - Internet ------------------------------------------
  #
  
  plot_internet_data <- reactive({data_internet %>% filter(county == input$whichcounty_internet)})
  plot_internet_var <- reactive({plot_internet_data()[[input$whichvar_internet]]})
  
  output$plot_internet <- renderLeaflet({
    
    var_label <- names(choices_older)[choices_internet == input$whichvar_internet]
    
    create_plot(plot_internet_data(), plot_internet_var(), var_label)
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



