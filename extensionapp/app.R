library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(mapview)
library(ggthemes)
library(RColorBrewer)
library(sjmisc)
library(shinythemes)
library(DT)
library(data.table)
library(rsconnect)
library(shinycssloaders)
library(readxl)
library(readr)
library(stringr)
#
# Data ------------------------------------------------
#

measures_table <- read_excel("data/Measures.xlsx")


#
# Options --------------------------------------------
#

prettyblue <- "#232D4B"
navBarBlue <- '#427EDC'
options(spinner.color = prettyblue, spinner.color.background = '#ffffff', spinner.size = 3, spinner.type = 7)

colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")

#
# UI ------------------------------------------------
#

ui <- fluidPage(
  shinyDashboardThemes(
    theme = "grey_light"
  ),
   
   titlePanel("Extension Project"),
   
  # DATA PANEL -----------------------------------------------------------
  tabPanel("Data and Measures", value = "data",
           fluidRow(style = "margin: 6px;",
                    h1(strong("Data and Measures"), align = "center"),
                    br()
           ),
           tabsetPanel(
             tabPanel("Data Sources",
                      h3("", align = "center"),
                     br(""),
                      column(12, 
                             box(img(src = "data-hifld.png", style = "display: inline; float: left;", width = "100px"),
                             p(strong("Homeland Infrastructure Foundation-Level Data."), "Homeland Infrastructure Foundation-Level Data (HIFLD) is a collection of public 
                               source datasets at property level provided by the Department of Homeland Security. Since 2002, this HIFLD has provided quarterly 
                               updated layers on topics from education to energy, including on health care facilities. We used HIFLD emergency medical services 
                               station data at the latitude and longitude geographic level in our analyses.")),
                             # br(""),
                             box(img(src = "data-marketmaker.png", style = "display: inline; float: left;", width = "200px"),
                             p(strong("MarketMaker."), "MarketMaker is a data source that includes locations of supermarkets, farmers' markets, and grocery stores.
                               It is a networking site that connects producers across America, and we utilize their 2019 data set to parse out locations of food
                               for communities. These locations are at the longitude and latitude geographic level.")
                             )
                      # )
                      ,
                      #column(4,
                             box(img(src = "data-acs.png", style = "display: inline; float: left;", width = "200px"),
                             p(strong("American Community Survey."), "The American Community Survey (ACS) is an ongoing yearly survey conducted by the U.S Census 
                               Bureau. ACS samples households to compile 1-year and 5-year datasets providing information on population sociodemographic and 
                               socioeconomic characteristics including employment, disability, and health insurance coverage. We used AC 2014/18 5-year 
                               estimates to obtain census tract and census block group-level to explore Patrick County resident characteristics.")),
                             # br(""),
                             box(img(src = "data-connect.png", style = "display: inline; float: left;", width = "150px"),
                             p(strong("CommonwealthConnect."), "The Virginia Tech CommonwealthConnect Wi-Fi Hotspot Map is an interactive map of free, publicly 
                               available wi-fi hotspots in Virginia. Its goal is to provide an easily accessible map of areas where individuals can connect to the 
                               internet for free, decreasing the constraints placed on families that do not have internet access at home. We used the 2020 wi-fi 
                               hotspot map data to retrieve hotspot locations in Patrick County and subsequently employed the information in calculating hotspot 
                               coverage isochrones.")),
                             # br(""),
                             box(img(src = "data-corelogic.png", style = "display: inline; float: left;", width = "120px"),
                             p(strong("CoreLogic."), "CoreLogic is a supplier of proprietary US real estate and specialized business data at the property level. 
                               This company provides data spamming over 50 years at the latitude and longitude level. Information available in the dataset includes 
                               property characteristics, mortgage, foreclosures and performance. We used 2019 CoreLogic data to obtain the locations of all residential
                               properties in Patrick County.")
                             ),
                      # ),
                      #column(4,
                             box(img(src = "data-traveltime.png", style = "display: inline; float: left;", width = "140px"),
                             p(strong("TravelTime."), "TravelTime Application Programming Interface (API) aggregates data from OpenStreetMap, transport timetables and
                               speed profiles to generate isochrones. An isochrone is a shape covering all locations that can be reached within the same timeframe 
                               given a start location, departure time, and a mode of transportation. We used the TravelTime API to produce isochrones of 10- and 
                               15-minute drive time interval from supermarkets, farmers' markets, and free wi-fi hotspots, and of 8-, 10-, and 12-minute drive 
                               time intervals from all emergency medical service stations in Patrick County.")),
                            #  br(""),
                             box(img(src = "data-ers.png", style = "display: inline; float: left;", width = "120px"),
                             p(strong("Food Access Research Atlas."), "The United State Department of Agriculture Food Access Research Atlas is a data resource 
                               created by the Economic Research Service that provides information on food access indicators at census tract level. The data allows 
                               individuals to understand food access in communities based on factors like age and socioeconomic status. We used the 2017 Food Access
                               Research Atlas to examine Patrick County residents’ food access at multiple distance thresholds and by resident characteristics.")
                             ))
                             ),
             tabPanel("Measures",  
                      h3(strong(""), align = "center"),
                      selectInput("topic", "Select Topic:", width = "100%", choices = c(
                        "All Measures",
                        "Sociodemographic Measures",
                        "Older Adult Population Measures",
                        "Connectivity Measures",
                        "Food Access Measures",
                        "Health Care Access Measures")
                      ),
                      withSpinner(DTOutput("datatable"))
             )
                             )
                             )
)

server <- function(input, output) {
  
  
  # data and measures table: done ----------------------------------------
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

