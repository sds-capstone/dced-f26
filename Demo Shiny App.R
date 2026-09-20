library(shiny)
library(tidyverse)
library(tidycensus)
library(leaflet)
library(sf)

census_api_key("34df405496a792f31bd4cf10741ff37e850c3ab0")

data <- get_acs(geography = "tract", variables = c("Median Household Income" = "S1901_C01_012", "Median Rent" = "DP04_0134", "Poverty Rate" = "S1701_C03_001", "Percent White Only" = "DP05_0037P", "Percent Foreign-Born" = "DP02_0094P"), year = 2024, state = "DC", geometry = TRUE, survey = "acs5") |>
  mutate(short_name = substr(NAME, 1, nchar(NAME) - 44)) |>
  st_transform('+proj=longlat +datum=WGS84')

ui <- fluidPage(
  titlePanel("Washington, D.C. Census Tract Demographics"),
    wellPanel(style = "margin-left: 20px; margin-right: 20px",
              selectInput(inputId = "var_select",
                label = "Variable to Display",
                choices = unique(data$variable))),
    leafletOutput("map")
  )

server <- function(input, output) {
  
  filtered_data <- reactive({
    data |>
      filter(variable == input$var_select)
  })
  
  filtered_data_pal <- reactive({
    colorNumeric(palette = "BuPu", domain = filtered_data()$estimate)
  })
  
  output$map <- renderLeaflet({
    pal <- filtered_data_pal()
    
    leaflet(filtered_data()) |>
      addTiles() |>
      addPolygons(color = "black", weight = 1, fillColor = ~pal(estimate), fillOpacity = 0.7, label = ~paste0(short_name, " | ", format(estimate, big.mark = ",", scientific = FALSE))) |>
      addLegend(pal = pal, values = ~filtered_data()$estimate, title = "Estimate")
  })
}

shinyApp(ui = ui, server = server)