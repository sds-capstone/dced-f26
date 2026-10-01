library(shiny)
library(tidyverse)
library(tidycensus)
library(leaflet)
library(sf)
library(tigris)

census_api_key("34df405496a792f31bd4cf10741ff37e850c3ab0")

data <- get_acs(geography = "tract", variables = c("Median Household Income" = "S1901_C01_012", "Median Rent" = "DP04_0134", "Poverty Rate" = "S1701_C03_001", "Percent White Only" = "DP05_0037P", "Percent Foreign-Born" = "DP02_0094P"), year = 2024, state = "DC", geometry = TRUE, survey = "acs5") |>
  mutate(short_name = substr(NAME, 1, nchar(NAME) - 44)) |>
  st_transform(crs = "WGS84") |>
  select(c(variable, estimate, short_name, geometry))

tracts <- tracts(state = "DC") |>
  select(c(TRACTCE, NAMELSAD, geometry))

crimes <- read.csv("https://hub.arcgis.com/api/v3/datasets/74d924ddc3374e3b977e6f002478cb9b_7/downloads/data?format=csv&spatialRefId=26985&where=1%3D1") |>
  filter(!is.na(CENSUS_TRACT)) |>
  mutate(TRACTCE = sprintf("%06d", CENSUS_TRACT)) |>
  group_by(TRACTCE) |>
  summarize(estimate = n())

tracts <- left_join(tracts, crimes, by = join_by(TRACTCE)) |>
  rename(short_name = NAMELSAD) |>
  select(!TRACTCE) |>
  mutate(variable = "Number of Crimes in 2025")

tracts <- st_transform(tracts, crs = "WGS84")
  
data <- rbind(data, tracts)
  

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
    
    if (unique(sf::st_geometry_type(filtered_data()) == "POINT")) {
      leaflet(filtered_data()) |>
        addTiles() |>
        addCircles(color = ~pal(estimate), fillOpacity = 0.9, label = ~paste0(short_name, " | ", format(estimate, big.mark = ",", scientific = FALSE))) |>
        addLegend(pal = pal, values = ~filtered_data()$estimate, title = "Estimate")
    }
    if (unique(sf::st_geometry_type(filtered_data()) == "POLYGON")) {
    leaflet(filtered_data()) |>
      addTiles() |>
      addPolygons(color = "black", weight = 1, fillColor = ~pal(estimate), fillOpacity = 0.7, label = ~paste0(short_name, " | ", format(estimate, big.mark = ",", scientific = FALSE))) |>
      addLegend(pal = pal, values = ~filtered_data()$estimate, title = "Estimate")
    }
  })
}

shinyApp(ui = ui, server = server)