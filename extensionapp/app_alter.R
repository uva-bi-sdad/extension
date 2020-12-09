library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(leaflet)
library(tidyverse)
library(DT)
library(readxl)


#
# Read in data ------------------------------------------------
#


datausda <- read_rds("data/final_usda.rds")
allcountynames <- datausda$County

measures_table <- read_excel("data/Measures.xlsx")


#
# Options --------------------------------------------
#

ui <- dashboardPage(title = "Economic Mobility Data Infrastructure",
                    
                    dashboardHeader(
                      titleWidth = '100%',
                      title = "Title"
                    ),
                    
                    dashboardSidebar(
                      sidebarMenu(
                        hr(),
                        menuItem(text = "Home", tabName = "home", icon = icon("list-ol")),
                        menuItem(text = "Proof of Concept", tabName = "usda", icon = icon("money-check-alt")),
                        menuItem(text = "Explore Access", tabName = "explore", icon = icon("child"),
                             menuSubItem(text = "Food Retail", tabName ="food"),
                             menuSubItem(text = "Free Wifi", tabName = "wifi"),
                             menuSubItem(text = "EMS Stations", tabName = "ems")),
                        hr(),
                        menuItem(text = "Data and Methods", tabName = "data", icon = icon("info-circle"),
                                 menuSubItem(text = "Measures Table", tabName = "methods"),
                                 menuSubItem(text = "Data Descriptions", tabName = "descriptions"))
                      )
                    ),
                    
                    dashboardBody()
)

server <- function(input, output){
  
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

