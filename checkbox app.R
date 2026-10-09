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

tracts <- st_transform(tracts, crs = "WGS84")

crimes <- read.csv("https://hub.arcgis.com/api/v3/datasets/74d924ddc3374e3b977e6f002478cb9b_7/downloads/data?format=csv&spatialRefId=26985&where=1%3D1") |>
  filter(!is.na(CENSUS_TRACT)) |>
  mutate(TRACTCE = sprintf("%06d", CENSUS_TRACT)) |>
  group_by(TRACTCE) |>
  summarize(estimate = n())

crimes <- left_join(tracts, crimes, by = join_by(TRACTCE)) |>
  rename(short_name = NAMELSAD) |>
  select(!TRACTCE) |>
  mutate(variable = "Number of Crimes in 2025")

# Loading in the public schools absentee and coordinates shapefile
schools_shapefile <- st_read("school_geospatial")

# Loading in the absenteeism by school data
absentee <- read.csv("absenteeism_ds.csv")

## Cleaning absenteeism data
# Change column names (col 4 to match shapefile name)
names(absentee)[1] <- "year"
names(absentee)[4] <- "SCHOOL_ID"
names(absentee)[6] <- "percent_abs"
# Filter school year to 2024-25
# Remove percent sign from percent_abs and make it a numeric variable
abs_clean <-
  absentee |>
  filter(year == "2024-25") |>
  mutate(percent_abs = as.numeric(sub("%", "", percent_abs)))

# Joining shapefile and csv! Going to be honest I copied some of Una's code for this part! 
geo_abs_joined <- left_join(schools_shapefile, abs_clean, by = "SCHOOL_ID") |>
  filter(!is.na(percent_abs)) |>
  st_transform(crs = "WGS84") |>
  mutate(variable = "% of Students Chronically Absent, 24-25",
         estimate = percent_abs,
         short_name = SCHOOL_NAM) |>
  select(variable, estimate, short_name, geometry)

# loading in built environment indicators and selecting relevant columns
built_environment <- st_read("built_environment_indicators") |>
  select(c(TRACTCE, m1_1_schoo:m9_5_HIN))

# changing column names so they're informative
colnames(built_environment)[2:42] <- c("Percent Tract Within 15-Minute Walk to School", "Percent Tract Within 15-Minute Walk to Modernized School", "Percent Tract Within 15-Minute Walk to Playground", "Percent Tract Within 2-Minute Walk of School Crossing Guard", "Percent Tract Along Safe Route to School", "Percent Tract Within 15-Minute Walk of Library", "Percent Tract Within 15-Minute Walk of Wireless Hotspot", "Percent Households With Broadband Internet", "Percent Tract Within 15-Minute Walk of Recreation Center", "Percent Households With Work Commute Under 45 Mins", "Percent Tract Within 15-Minute Walk of Banking Institution", "Percent Tract Within 15-Minute Walk of Cash Checking Institution", "Percent Buildings of Good Quality", "Percent Homes Built Since 1970", "Percent Housing Units Affordable", "Percent Tract Within 2-Minute Walk of Vacant or Blighted House", "Percent Tract Within 2-Minute Walk of Bus Stop", "Percent Tract Within 15-Minute Walk of Metro Station", "Percent Tract Within 5-Minute Walk of Capital Bikeshare Location", "Percent Street Area With Bike Lanes", "Percent 311 Calls Made for Sidewalk Repair", "Percent Tract Alleys and Parking Lots", "Percent Tract Within 15-Minute Walk of Grocery Store", "Percent Tract in Low Food Access Area", "Percent Tract Within 15-Minute Walk of Farmers Market", "Percent Tract Within 15-Minute Walk of Healthy Corner Store", "Percent Tract Within 5-Minute Walk of Restaurant", "Percent Tract Within 15-Minute Walk of Liquor Store", "Percent Tract Within 15-Minute Walk of Health Care Facility", "Percent Tract Within 15-Minute Walk of Mental Health Provider", "Percent Tract With Tree Canopy", "Percent Tract Within 10-Minute Walk of Park", "Percent Tract Within 1/4 Mile of Trail", "Land Use Diversity Score (0-1)", "Percent Positive Land Uses", "Percent Tract Within Floodplain", "Percent Tract Within 2-Minute Walk of Vacant Lot", "Percent Sidewalks Within 30 Feet of Streetlight", "Percent Tract Within 15-Minute Walk of Police Department", "Percent Tract Within 15-Minute Walk of Fire Station", "Percent Tract Within 250 Feet of High Injury Network Corridor")

# pivoting to longer format and reformatting percentage variables to percent, and rounding each number to 2 decimal places
built_environment <- built_environment |>
  pivot_longer('Percent Tract Within 15-Minute Walk to School':'Percent Tract Within 250 Feet of High Injury Network Corridor', names_to = "variable", values_to = "estimate") |>
  mutate(estimate = case_when(variable == "Land Use Diversity Score (0-1)" ~ round(estimate, 2),
                              .default = round(estimate*100, 2)))

# transforming CRS to match other data
built_environment <- st_transform(built_environment, crs = "WGS84")

# getting tract names and GEOIDs to merge with built_environment
tracts_names <- as.data.frame(cbind(tracts$NAMELSAD, tracts$TRACTCE))
colnames(tracts_names) <- c("short_name", "TRACTCE")

# merging tract names with built_environment
built_environment <- right_join(built_environment, tracts_names, by = join_by(TRACTCE)) |>
  select(!TRACTCE)

# Putting all dfs into one thing for the shiny app
data <- rbind(data, crimes, geo_abs_joined, built_environment)

# Ok so in this version I also included a checkbox to overlay chronic absent
ui <- fluidPage(
  titlePanel("Washington, D.C. Census Tract Demographics"),
  wellPanel(style = "margin-left: 20px; margin-right: 20px",
            selectInput(inputId = "var_select",
                        label = "Variable to Display",
                        choices = unique(data$variable)),
            checkboxInput(inputId = "show_schools",
                          label = "% of Students Chronically Absent by School",
                          value = FALSE)),
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
# No longer need the if/else point polygon logic    
    m <- leaflet(filtered_data()) |>
      addTiles() |>
      addPolygons(color = "black", weight = 1, fillColor = ~pal(estimate), fillOpacity = 0.7, label = ~paste0(short_name, " | ", format(estimate, big.mark = ",", scientific = FALSE))) |>
      addLegend(pal = pal, values = ~estimate, title = "Estimate")
# Added if statement for whether or not to show school points
    if (input$show_schools) {
      school_pal <- colorNumeric(palette = "YlOrRd", domain = geo_abs_joined$estimate)
      m <- m |>
        addCircleMarkers(data = geo_abs_joined, radius = 6, color = "black", weight = 1,
                         fillColor = ~school_pal(estimate), fillOpacity = 0.9,
                         label = ~paste0(short_name, " | ", estimate, "% chronically absent")) |>
        addLegend(data = geo_abs_joined, pal = school_pal, values = ~estimate,
                  title = "% Chronically Absent", position = "bottomleft")
    }
    m
  })
}

shinyApp(ui = ui, server = server)
