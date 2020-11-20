library(leaflet)
library(shiny)
library(shinythemes)
library(sf)
library(dplyr)
library(readr)
library(DT)


#
# Load data and prepare ------------------------------------------------------------------------
#

data <- read_rds("ruralapp.rds")


#
# App --------------------------------
#


# UI
ui <- fluidPage(theme = shinytheme("cosmo"),
                
                headerPanel(title = h1(strong("Virginia County Rurality Explorer"), align = "center"),
                            windowTitle = "Virginia County Rurality Explorer"),
                
                fluidPage(
                  fluidRow(
                    hr(),
                    h2(strong("Maps")),
                    
                    tabsetPanel(
                      tabPanel("Rural-Urban Type",
                               fluidRow(
                                 style = "margin: 17px",
                                 h4(strong('County-Level Rurality: Virginia Department of Health Office of Rural-Urban Definition')),
                                 p(),
                                 leafletOutput('plot_srhp', height = "500px")
                               )
                      ),
                      
                      tabPanel("Metropolitan/Nonmetropolitan Delineation",
                               fluidRow(
                                 style = "margin: 17px",
                                 h4(strong('County-Level Rurality: US Office of Management and Budget Metropolitan/Nonmetropolitan Delineation')),
                                 p(),
                                 leafletOutput('plot_omb', height = "500px")
                               )
                      ),
                      
                      tabPanel("Index of Relative Rurality",
                               fluidRow(
                                 style = "margin: 17px",
                                 h4(strong('County-Level Rurality: Purdue Index of Relative Rurality Definition')),
                                 p(),
                                 leafletOutput('plot_irr', height = "500px")
                               )
                      ),
                      
                      tabPanel("Rural-Urban Continuum Code",
                               fluidRow(
                                 style = "margin: 17px",
                                 h4(strong('County-Level Rurality: USDA Rural-Urban Continuum Code Definition')),
                                 p(),
                                 leafletOutput('plot_rucc', height = "500px")
                               )
                      ),
                      
                      tabPanel("Urban Influence Code",
                               fluidRow(
                                 style = "margin: 17px",
                                 h4(strong('County-Level Rurality: USDA Urban Influence Codes Definition')),
                                 p(),
                                 leafletOutput('plot_urbinf', height = "500px")
                               )
                      ),
                      
                      tabPanel("Rural-Urban Density Typology",
                               fluidRow(
                                 style = "margin: 17px",
                                 h4(strong('County-Level Rurality: Isserman Rural-Urban Density Typology Definition')),
                                 p(),
                                 leafletOutput('plot_isser', height = "500px")
                               )
                      ),
                      
                      tabPanel("Urban-Rural Classification Scheme",
                               fluidRow(
                                 style = "margin: 17px",
                                 h4(strong('County-Level Rurality: National Center for Health Statistics Urban-Rural Classification Scheme Definition')),
                                 p(),
                                 leafletOutput('plot_nchs', height = "500px")
                               )
                      )
                    )
                  ),
                  
                  fluidRow(
                    hr(),
                    h2(strong("Data")),
                    
                    column(12,
                           DTOutput("filtered_table")
                    )
                  ),
                  
                  fluidRow(
                    hr(),
                    h2(strong("Sources")),
                    
                    column(12,
                           p(strong("The Index of Relative Rurality"), "was developed by researchers at Purdue University. The index is calculated
                             based on area population size, density, remoteness, and built-up area. Counties are assigned a relative rurality value 
                             on a continuous scale ranging from 0 to 1, where 0 represents the most urban area and 1 represents
                             the most rural area. The latest available data was released in 2018.",
                             a(href = "https://purr.purdue.edu/publications/2960/about?v=1", target = "blank", "Read more here.")), 
                            p(strong("The Metropolitan/Nonmetropolitan Delineation"), "was developed by the US Office of Management and Budget.
                              It classifies US counties as metropolitan, micropolitan, or neither. It distinguishes between metropolitan and micropolitan
                              areas based on the size of their urban core. The latest available delineation was released in 2020.",
                              a(href = "https://www.census.gov/programs-surveys/metro-micro.html", target = "blank", "Read more here.")), 
                             p(strong("Urban Influence Codes"), "were developed by the US Department of Agriculture Economic Research Service.
                               Counties are assigned metropolitan or nonmetropolitan status according to the Office of Management and Budget's definitions.
                               Metropolitan counties are further divided into two subcategories by population size, and nonmetropolitan counties are subdivided into ten categories by major city size, metro area proximity, and micropolitan area proximity.
                               Each US county is assigned one of the resulting twelve urban influence codes. The latest available data was released in 2013.", 
                               a(href = "https://www.ers.usda.gov/data-products/urban-influence-codes.aspx", target = "blank", "Read more here.")), 
                             p(strong("Urban-Rural Continuum Codes"), "were developed by the US Department of Agriculture Economic Research Service.
                              Counties are assigned metropolitan or nonmetropolitan status according to the Office of Management and Budget's definitions. 
                              Metropolitan areas further divided into three categories depending on population size, and
                              nonmetropolitan areas subdivided into six categories depending on urbanization and metropolitan area adjecency.
                              Each US county is assigned one of the resulting nine rural-urban continuum codes. The latest available data was released in 2013.",
                               a(href = "https://www.ers.usda.gov/data-products/rural-urban-continuum-codes/", target = "blank", "Read more here.")), 
                             p(strong("The Rural-Urban Density Typology"), "was developed by a researcher at the University of Illinois, Urbana. 
                              The typology is based the share of urban county residents, population density,
                              and the population size of the county's largest urban area. 
                              Each US county is assigned either urban, mixed urban, mixed rural, or rural status. Virginia Department of Health's interpretation
                               of the index was released in 2013 and is available",
                               a(href = "https://www.vdh.virginia.gov/content/uploads/sites/76/2016/06/2013VSRHP-final.pdf", target = "blank", "here."),
                              a(href = "https://journals.sagepub.com/doi/10.1177/0160017605279000", target = "blank", "Read more here.")),
                           p(strong("The Urban-Rural Classification Scheme"), "was developed by the US Centers for Disease Control National Center for Health Statistics. 
                             The scheme is based on the Office of Management and Budget's metropolitan/nonmetropolitan delineation, dividing
                             metropolitan counties into four categories based on population and metropolitan statistical area size, and nonmetropolitan counties into two 
                             categories based on the Office of Management and Budget's micropolitan versus nonmetro delineation. The latest available data were 
                             released for 2013.", 
                             a(href = "https://www.cdc.gov/nchs/data_access/urban_rural.htm", target = "blank", "Read more here.")),
                            p(strong("The Virginia Department of Health Rural-Urban Type"), "was developed by its Office of Rural Health. The classification
                              builds on the Office of Management and Budget's metropolitan/nonmetropolitan delineation and considers nonmetro and micropolitan 
                              statistical areas as rural, and metropolitan statistical areas as urban."
                            )
                    )
                    )
                  )
)

# Server
server <- function(input, output, session) {
  
  # Table
  ruraltable <- reactive({
    
    data %>% st_set_geometry(NULL) %>%
      select(county_name.y, rucc_2013, uic_2013, ombrural, srhprurality, irr2010, isserman, nchs_2013) %>%
      mutate(irr2010 = round(irr2010, 2)) %>%
      arrange((county_name.y))
  })
  
  output$filtered_table <- renderDataTable({
    datatable(ruraltable(), rownames = FALSE, options = list(pageLength = 15),
              colnames = c("County", "Rural-Urban Continuum Code", "Urban Influence Code", "Metropolitan/Nonmetropolitan Delineation", "Rural-Urban Type", "Index of Relative Rurality", "Rural-Urban Density Typology", "Urban-Rural Classification Scheme"))
  })
  
  # Labels
  labels_rural <- lapply(
    paste("<strong>Area: </strong>",
          data$county_name.y,
          "<br />",
          "<strong>Rural-Urban Continuum Code: </strong>",
          data$rucc_2013,
          "<br />",
          "<strong>Rural-Urban Continuum Code description: </strong>",
          data$description_rucc,
          "<br />",
          "<strong>Urban influence Code: </strong>",
          data$uic_2013,
          "<br />",
          "<strong>Urban influence Code description: </strong>",
          data$description_urbinf,
          "<br />",
          "<strong>Index of Relative Rurality: </strong>",
          round(data$irr2010, 2),
          "<br />",
          "<strong>Rural-Urban Density Typology: </strong>",
          data$isserman,
          "<br />",
          "<strong>Rural-Urban Type: </strong>",
          data$srhprurality,
          "<br />",
          "<strong>Metropolitan/Nonmetropolitan Delineation: </strong>",
          data$ombrural,
          "<br />",
          "<strong>Urban-Rural Classification Scheme: </strong>",
          data$nchs_2013,
          "<br />",
          "<strong>Urban-Rural Classification Scheme description: </strong>",
          data$nchs_2013_desc
    ),
    htmltools::HTML
  )
  
  # Plot function with factor palette
  create_plot_factor <- function(myvar, myvarlabel) {
    
    pal <- colorFactor("YlGn", domain = myvar, ordered = TRUE)
    
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
  output$plot_nchs <- renderLeaflet({
    create_plot_factor(data$nchs_2013, "Urban-Rural Classification<br>Scheme")
  })
  
  output$plot_srhp <- renderLeaflet({
    create_plot_factor(data$srhprurality, "VDH ORH Classification")
  })
  
  output$plot_omb <- renderLeaflet({
    create_plot_factor(data$ombrural, "Metropolitan/Nonmetropolitan<br>Delineation")
  })
  
  output$plot_isser <- renderLeaflet({
    create_plot_factor(data$isserman, "Isserman Classification")
  })
  
  output$plot_rucc <- renderLeaflet({
    create_plot_factor(data$rucc_2013, "Rural-Urban<br>Continuum Code")
  })
  
  output$plot_irr <- renderLeaflet({
    create_plot_cont(data$irr201, "Index of Relative<br>Rurality")
  })
  
  output$plot_urbinf <- renderLeaflet({
    create_plot_factor(data$uic_2013, "Urban Influence<br>Code")
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)

