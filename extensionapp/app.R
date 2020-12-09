library(shiny)
library(shinydashboard)
library(leaflet)
library(tidyverse)
library(DT)
library(readxl)


#
# Read in dta ------------------------------------------------
#


datausda <- read_rds("data/final_usda.rds")
allcountynames <- datausda$County

measures_table <- read_excel("data/Measures.xlsx")


#
# Options --------------------------------------------
#

ui <- dashboardPage(
  
  dashboardHeader(
    titleWidth = '100%',
    title = "Title"
  ),
  
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
      menuItem("Home", tabName = "home"),
      menuItem("Proof of Concept", tabName = "usda"),
      menuItem(text="Explore Access", tabName="explore",
               menuSubItem(text = "Food Retail", tabName ="food"),
               menuSubItem(text = "Free Wifi", tabName = "wifi"),
               menuSubItem(text = "EMS Stations", tabName = "ems")),
      menuItem(text = "Data and Methods", tabName = "data", icon = icon("info-circle"),
               menuSubItem(text = "Measures Table", tabName = "methods"),
               menuSubItem(text = "Data Descriptions", tabName = "descriptions"))
    )),
  
  dashboardBody(
  tabItems(
    
  # Main -----------------------------------------------------------
  tabItem(tabName = "home",
           fluidRow(style = "margin: 6px;",
                    align = "center",
                    br("", style = "padding-top:10px;"),
                    h2(strong("Addressing Barriers to Health in Rural Virginia Counties"),
                       br(""),
                       h4("University of Virginia"),
                       h4("Biocomplexity Insititute"))
           )
  ),
  
  # Proof of concept with usda data -----------------------------------------------------------
  tabItem(tabName = "usda",
           fluidRow(style = "margin: 6px;",
                    h1(strong("Proof of Concept"), align = "center"),
                    br(),
                    selectInput("usdadrop", "Select County:", multiple = F, width = "100%", choices = c(allcountynames)),
                    selectInput("usdadrop_1", "Select Variable:", width = "100%", choices = c(
                      "Percent Population with Low Food Access at 1 Mile" = "lapop1share",
                      "Percent Population with Low Food Access at 10 Miles" = "lapop10share",
                      "Percent Children with Low Food Access at 1 Mile" = "lakids1share",
                      "Percent Children with Low Food Access at 10 Miles" = "lakids10share",
                      "Percent Low Income Population with Low Food Access at 1 Mile" = "lalowi1share",
                      "Percent Low Income Population with Low Food Access at 10 Miles" = "lalowi10share",
                      "Percent Older Adults with Low Food Access at 1 Mile" = "laseniors1share",
                      "Percent Older Adults with Low Food Access at 10 Miles" = "laseniors10share")),
                    br(),
                    leafletOutput("usdaplot")
           )
  ),
  # Food retail ---------------------------------------------------
  tabItem(tabName = "food",
          fluidRow(style = "margin: 6px;",
                   h1(strong("Food Retail"), align = "center"),
                   br()
          )
  ),
  
  # Free wfi ------------------------------------------------------
  tabItem(tabName = "eifi",
          fluidRow(style = "margin: 6px;",
                   h1(strong("Proof of Concept"), align = "center"),
                   br()
          )
  ),
  
  # Ems stations --------------------------------------------------
  tabItem(tabName = "ems",
          fluidRow(style = "margin: 6px;",
                   h1(strong("Proof of Concept"), align = "center"),
                   br()
          )
  ),

  # Data and measures  -----------------------------------------------------------
  tabItem(tabName = "descriptions",
           fluidRow(style = "margin: 6px;",
                    h1(strong("Data and Measures"), align = "center"),
                    br(),
                      column(12,
                             box(img(src = "data-hifld.png", style = "display: inline; float: left;", width = "100px"),
                             p(strong("Homeland Infrastructure Foundation-Level Data."), "Homeland Infrastructure Foundation-Level Data (HIFLD) is a collection of public
                               source datasets at property level provided by the Department of Homeland Security. Since 2002, this HIFLD has provided quarterly
                               updated layers on topics from education to energy, including on health care facilities. We used HIFLD emergency medical services
                               station data at the latitude and longitude geographic level in our analyses."), width = 12),
                             # br(""),
                             shinydashboard::box(img(src = "data-marketmaker.png", style = "display: inline; float: left;", width = "200px"),
                             p(strong("MarketMaker."), "MarketMaker is a data source that includes locations of supermarkets, farmers' markets, and grocery stores.
                               It is a networking site that connects producers across America, and we utilize their 2019 data set to parse out locations of food
                               for communities. These locations are at the longitude and latitude geographic level."), width = 12
                             ),
                      box(img(src = "data-acs.png", style = "display: inline; float: left;", width = "200px"),
                             p(strong("American Community Survey."), "The American Community Survey (ACS) is an ongoing yearly survey conducted by the U.S Census
                               Bureau. ACS samples households to compile 1-year and 5-year datasets providing information on population sociodemographic and
                               socioeconomic characteristics including employment, disability, and health insurance coverage. We used AC 2014/18 5-year
                               estimates to obtain census tract and census block group-level to explore Patrick County resident characteristics."), width = 12),
                             # br(""),
                      box(img(src = "data-connect.png", style = "display: inline; float: left;", width = "150px"),
                             p(strong("CommonwealthConnect."), "The Virginia Tech CommonwealthConnect Wi-Fi Hotspot Map is an interactive map of free, publicly
                               available wi-fi hotspots in Virginia. Its goal is to provide an easily accessible map of areas where individuals can connect to the
                               internet for free, decreasing the constraints placed on families that do not have internet access at home. We used the 2020 wi-fi
                               hotspot map data to retrieve hotspot locations in Patrick County and subsequently employed the information in calculating hotspot
                               coverage isochrones."), width = 12),
                             # br(""),
                      box(img(src = "data-corelogic.png", style = "display: inline; float: left;", width = "120px"),
                             p(strong("CoreLogic."), "CoreLogic is a supplier of proprietary US real estate and specialized business data at the property level.
                               This company provides data spamming over 50 years at the latitude and longitude level. Information available in the dataset includes
                               property characteristics, mortgage, foreclosures and performance. We used 2019 CoreLogic data to obtain the locations of all residential
                               properties in Patrick County."), width = 12
                             ),
                     box(img(src = "data-traveltime.png", style = "display: inline; float: left;", width = "140px"),
                             p(strong("TravelTime."), "TravelTime Application Programming Interface (API) aggregates data from OpenStreetMap, transport timetables and
                               speed profiles to generate isochrones. An isochrone is a shape covering all locations that can be reached within the same timeframe
                               given a start location, departure time, and a mode of transportation. We used the TravelTime API to produce isochrones of 10- and
                               15-minute drive time interval from supermarkets, farmers' markets, and free wi-fi hotspots, and of 8-, 10-, and 12-minute drive
                               time intervals from all emergency medical service stations in Patrick County."), width = 12
                         ),
                     box(img(src = "data-ers.png", style = "display: inline; float: left;", width = "120px"),
                             p(strong("Food Access Research Atlas."), "The United State Department of Agriculture Food Access Research Atlas is a data resource
                               created by the Economic Research Service that provides information on food access indicators at census tract level. The data allows
                               individuals to understand food access in communities based on factors like age and socioeconomic status. We used the 2017 Food Access
                               Research Atlas to examine Patrick County residents’ food access at multiple distance thresholds and by resident characteristics."), width = 12
                             )))
                             ),
             tabItem(tabName = "measures",
                      fluidRow(
                      box(width = 12,
                            title = "Measures and Data Sources",
                      selectInput("topic", "Select Topic:", width = "100%", choices = c(
                        "All Measures",
                        "Sociodemographic Measures",
                        "Older Adult Population Measures",
                        "Connectivity Measures",
                        "Food Access Measures",
                        "Health Care Access Measures")),
                      DTOutput("datatable")))
                    )
                  )
      )
    )

server <- function(input, output){
  
  # Options ------------------------------------------------------
  navBarBlue <- '#427EDC'
  colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")
  
  
  # Proof of concept panel - USDA ------------------------------------------------------
  usdadata <- reactive({datausda %>% filter(County == input$usdadrop)})
  
  output$usdaplot <- renderLeaflet({
    
    pal <- colorQuantile("Blues",domain = usdadata()$lakids1share, probs = seq(0, 1, length = 5), right = TRUE)
    
    labels <- lapply(
      paste("<strong>Area: </strong>",
            usdadata()$NAME,
            "<br />",
            "<strong>% Population with",
            #usda_spec,
            "low food access for children at 1 mile",
            round(usdadata()$lakids1share, 2)),
      htmltools::HTML
    )
    
    leaflet(data = usdadata(), options = leafletOptions(minZoom = 10))%>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(fillColor = ~pal(usdadata()$lakids1share), 
                  fillOpacity = 0.6, 
                  stroke = FALSE,
                  label = labels,
                  labelOptions = labelOptions(direction = "bottom",
                                              style = list(
                                                "font-size" = "12px",
                                                "border-color" = "rgba(0,0,0,0.5)",
                                                direction = "auto"
                                              ))) %>%
      addLegend("bottomleft",
                pal = pal,
                values =  ~(usdadata()$lakids1share),
                title = "Percent by<br>Quartile Group",
                opacity = 0.6,
                labFormat = function(type, cuts, p) {
                  n = length(cuts)
                  paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                })
  })
  
  # Data and measures table ------------------------------------------
  
  var_topic <- reactive({
    input$topic
  })
  
  output$datatable <- renderDataTable({
    if(var_topic() == "All Measures"){
      table <- as.data.frame(measures_table)
      datatable(table, rownames = FALSE, options = list(pageLength = 15)) %>% formatStyle(0, target = 'row', lineHeight = '80%')
    }
    else{
      data <- switch(input$topic,
                     "Connectivity Measures" = "connectivity",
                     "Sociodemographic Measures" = "demographics",
                     "Food Access Measures" = "food access",
                     "Health Care Access Measures" = "health",
                     "Older Adult Population Measures" = "older adults")
      table <- subset(measures_table, Topic == data)
      table <- as.data.frame(table)
      datatable(table, rownames = FALSE, options = list(pageLength = 15)) %>% formatStyle(0, target = 'row', lineHeight = '80%')
    }
  })
  
}

shinyApp(ui = ui, server = server)

