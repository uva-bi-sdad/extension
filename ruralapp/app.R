library(tidyverse)
library(leaflet)
library(shiny)
library(shinythemes)
library(sf)
library(dplyr)
library(tidycensus)
library(readr)
library(DT)


#
# Load data and prepare ------------------------------------------------------------------------
#

data <- read_rds("ruralapp.rds")
data <- st_transform(data, 4269)


#
# App --------------------------------
#


# UI
ui <- fluidPage(theme = shinytheme("cosmo"),
                hr(),
                h1(strong("Maps")), 
                tabsetPanel(
                  tabPanel("VDH ORH",
                           fluidRow(
                             style = "margin: 17px",
                             h4(strong('County-Level Rurality: VDH Office of Rural Health Definition')),
                             p(),
                             leafletOutput('plot_srhp', height = "500px")
                           )
                  ),
                  
                  tabPanel("OMB",
                           fluidRow(
                             style = "margin: 17px",
                             h4(strong('County-Level Rurality: US Office of Management and Budget Definition')),
                             p(),
                             leafletOutput('plot_omb', height = "500px")
                           )
                  ),
                  
                  tabPanel("Purdue IRR",
                           fluidRow(
                             style = "margin: 17px",
                             h4(strong('County-Level Rurality: Purdue Index of Relative Rurality Definition')),
                             p(),
                             leafletOutput('plot_irr', height = "500px")
                           )
                  ),
                  
                  tabPanel("USDA RUCC",
                           fluidRow(
                             style = "margin: 17px",
                             h4(strong('County-Level Rurality: USDA Rural-Urban Continuum Code Definition')),
                             p(),
                             leafletOutput('plot_rucc', height = "500px")
                           )
                  ),
                  
                  tabPanel("USDA Urban Influence",
                           fluidRow(
                             style = "margin: 17px",
                             h4(strong('County-Level Rurality: USDA Urban Influence Codes Definition')),
                             p(),
                             leafletOutput('plot_urbinf', height = "500px")
                           )
                  ),
                  
                  tabPanel("Isserman",
                           fluidRow(
                             style = "margin: 17px",
                             h4(strong('County-Level Rurality: Isserman Definition')),
                             p(),
                             leafletOutput('plot_isser', height = "500px")
                           )
                  )
                ),
                
                hr(),
                h1(strong("Data")),
                
                column(3,
                       wellPanel(
                   br("Show counties that are rural according to this number of definitions:"),
                   br(),
                tags$div(numericInput(inputId = "numdef_min",
                                      label = "Minimum:",
                                      value = 0,
                                      min = 0,
                                      max = 6,
                                      step = 1, 
                                      width = "100px"),  
                         style = "display:inline-block"),
                tags$div(numericInput(inputId = "numdef_max",
                                      label = "Maximum:",
                                      value = 0,
                                      min = 0,
                                      max = 6,
                                      step = 1, 
                                      width = "100px"),  
                         style = "display:inline-block"),
                br(),
                textOutput("selected_range")
                       )
                ),
                column(9,
                   DTOutput("filtered_table")
                )
)

# Server
server <- function(input, output, session) {
  
  # Table
  ruraltable <- reactive({
        
        data %>% st_set_geometry(NULL) %>%
        select(county_name.y, rucc_2013, uic_2013, ombrural, srhprurality, irr2010, isserman, 
               score_rucc, score_urbinf, score_omb, score_srhp, score_irr, score_isserman, score) %>%
        mutate(irr2010 = round(irr2010, 2)) %>%
        filter(score >= input$numdef_min & score <= input$numdef_max) %>%
        arrange(desc(score))
  })
  
  output$filtered_table <- renderDataTable({
    datatable(ruraltable(), rownames = FALSE, options = list(pageLength = 15),
              colnames = c("County", "RUCC", "UIC", "OMB", "ORH", "IRR", "Isserman",  
                           "RUCC rural?", "UIC rural?", "OMB rural?", "ORH rural?", "IRR rural?", "Isserman rural?", "Total")) 
  })
  
  output$selected_range <- renderText({ 
    paste("Virginia has", nrow(ruraltable()), "counties that are rural according to equal to or more than", input$numdef_min, 
          "and equal to or less than", input$numdef_max, "definitions.")
  })
  
  # Labels 
  labels_rural <- lapply(
    paste("<strong>Area: </strong>",
          data$county_name.y,
          "<br />",
          "<strong>USDA RUCC 2013: </strong>",
          data$rucc_2013,
          "<br />",
          "<strong>USDA RUCC 2013 description: </strong>",
          "<br />",
          data$description_rucc,
          "<br />",
          "<strong>USDA Urban influence 2013: </strong>",
          data$uic_2013,
          "<br />",
          "<strong>USDA Urban influence 2013 description: </strong>",
          "<br />",
          data$description_urbinf,
          "<br />",
          "<strong>IRR 2010: </strong>",
          round(data$irr2010, 2),
          "<br />",
          "<strong>Isserman 2013: </strong>",
          data$isserman,
          "<br />",
          "<strong>VDH ORH 2020: </strong>",
          data$srhprurality,
          "<br />",
          "<strong>OMB 2013: </strong>",
          data$ombrural
    ),
    htmltools::HTML
  )
  
  # Plot function with factor palette
  create_plot_factor <- function(myvar, myvarlabel) {
    
    pal <- colorFactor("YlGn", domain = myvar)
    
    leaflet(data = data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(fillColor = ~pal(myvar), 
                  fillOpacity = 0.7, 
                  stroke = TRUE, weight = 0.5, color = "#202020",
                  label = labels_rural,
                  labelOptions = labelOptions(direction = "bottom",
                                              style = list(
                                                "font-size" = "12px",
                                                "border-color" = "rgba(0,0,0,0.5)",
                                                direction = "auto"
                                              )))  %>%
      addLegend("bottomleft",
                pal = pal,
                values = ~(myvar),
                title = myvarlabel,
                opacity = 0.7)
  }
  
  # Plot function with continuous palette
  create_plot_cont <- function(myvar, myvarlabel) {
    
    pal <- colorBin("YlGn", domain = myvar, bins = 6)
    
    leaflet(data = data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(fillColor = ~pal(myvar), 
                  fillOpacity = 0.7, 
                  stroke = TRUE, weight = 0.5, color = "#202020",
                  label = labels_rural,
                  labelOptions = labelOptions(direction = "bottom",
                                              style = list(
                                                "font-size" = "12px",
                                                "border-color" = "rgba(0,0,0,0.5)",
                                                direction = "auto"
                                              )))  %>%
      addLegend("bottomleft",
                pal = pal,
                values = ~(myvar),
                title = myvarlabel,
                opacity = 0.7)
  }
  
  # Create plots 
  output$plot_srhp <- renderLeaflet({
    create_plot_factor(data$srhprurality, "VDH ORH Classification")
  })
  
  output$plot_omb <- renderLeaflet({
    create_plot_factor(data$ombrural, "OMB Classification")
  })
  
  output$plot_isser <- renderLeaflet({
    create_plot_factor(data$isserman, "Isserman Classification")
  })
  
  output$plot_rucc <- renderLeaflet({
    create_plot_factor(data$rucc_2013, "RUCC Classification")
  })
  
  output$plot_irr <- renderLeaflet({
    create_plot_cont(data$irr201, "IRR Classification")
  })
  
  output$plot_urbinf <- renderLeaflet({
    create_plot_factor(data$uic_2013, "USDA Urban Influence<br>Classification")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

