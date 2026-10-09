library(shiny)
library(tidyverse)
library(tidycensus)
library(leaflet)
library(sf)
library(tigris)

# Setting API key for census data. Pulls data from census package
census_api_key("34df405496a792f31bd4cf10741ff37e850c3ab0")

# Names of variables that your pulling
# Geometry- shapefile instead of dataset
# variable called shortname- shortens name of census tract like tract 1 etc
# st transform from sf package, transforming everything to default coordinates
data <- get_acs(geography = "tract", variables = c("Median Household Income" = "S1901_C01_012", "Median Rent" = "DP04_0134", "Poverty Rate" = "S1701_C03_001", "Percent White Only" = "DP05_0037P", "Percent Foreign-Born" = "DP02_0094P"), year = 2024, state = "DC", geometry = TRUE, survey = "acs5") |>
  mutate(short_name = substr(NAME, 1, nchar(NAME) - 44)) |>
  st_transform(crs = "WGS84") |>
  select(c(variable, estimate, short_name, geometry))

# tigris tract shapefile
# selecting the variables- variable (names of variables), estimate (coorspondering num like income, geometry is shapefiles of census tracts)
tracts <- tracts(state = "DC") |>
  select(c(TRACTCE, NAMELSAD, geometry))

# crime data read in, this is points but just moved it into tract. Turns track ID into character var
crimes <- read.csv("https://hub.arcgis.com/api/v3/datasets/74d924ddc3374e3b977e6f002478cb9b_7/downloads/data?format=csv&spatialRefId=26985&where=1%3D1") |>
  filter(!is.na(CENSUS_TRACT)) |>
  mutate(TRACTCE = sprintf("%06d", CENSUS_TRACT)) |>
  group_by(TRACTCE) |>
  summarize(estimate = n())

# Join track shapefile with crimes dataset
tracts <- left_join(tracts, crimes, by = join_by(TRACTCE)) |>
  rename(short_name = NAMELSAD) |>
  select(!TRACTCE) |>
  mutate(variable = "Number of Crimes in 2025")

# transforming to WGS84 is a coordinate reference system... way its projected on a map. If shapefile is not in WGS84, transforms into
tracts <- st_transform(tracts, crs = "WGS84")

# Loading in the public schools absentee and coordinates shapefile
schools_shapefile <- st_read("school_geospatial")

# Loading in the absenteeism by school data
absentee <- read.csv("data-files/absenteeism_ds.csv")

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

#Loading in air quality data
air_quality <- read.csv("data-files/air-quality-history.csv")

#Cleaning data
air_quality <- air_quality |>
  mutate(DATETIME_LOCAL = lubridate::ymd_hms(DATETIME_LOCAL)) |>
  mutate(year = lubridate::year(DATETIME_LOCAL))

air_quality_2024 <- air_quality |>
  filter(
    year == 2024,
    PARAMETER_NAME %in% c("Nitrogen dioxide (NO2)", "PM2.5 - Local Conditions", "Ozone")
  ) |>
  mutate(
    month = lubridate::month(DATETIME_LOCAL)
  )
  
#Getting monthly and annual average AQIs by site
air_quality_monthly_aqi <- air_quality_2024 |>
  group_by(LOCAL_SITE_NAME, PARAMETER_NAME, month) |>
  summarize(
    monthly_aqi = mean(AQI, na.rm = TRUE),
    .groups = "drop"
  )

air_quality_monthly_aqi_overall <- air_quality_monthly_aqi |>
  group_by(LOCAL_SITE_NAME, month) |>
  summarize(
    monthly_aqi = max(monthly_aqi, na.rm = TRUE),
    .groups = "drop"
  )

air_quality_monthly_aqi_wide <- air_quality_monthly_aqi_overall |>
  mutate(
    month = month.name[month]
  ) |>
  pivot_wider(
    names_from = month,
    values_from = monthly_aqi
  )

air_quality_annual_aqi <- air_quality_2024 |>
  group_by(LOCAL_SITE_NAME) |>
  summarize(
    annual_aqi = mean(AQI, na.rm = TRUE),
    .groups = "drop"
  )

air_quality_annual_aqi <- air_quality_monthly_aqi_overall |>
  group_by(LOCAL_SITE_NAME) |>
  summarize(
    annual_aqi = mean(monthly_aqi, na.rm = TRUE),
    .groups = "drop"
  )

air_quality_by_sites <- air_quality_monthly_aqi_wide |>
  left_join(
    air_quality_annual_aqi,
    by = "LOCAL_SITE_NAME"
  ) |>
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 1)
    )
  )

#getting site locations from original dataset
air_quality_locations <- air_quality_2024 |>
  select(
    LOCAL_SITE_NAME,
    LATITUDE,
    LONGITUDE
  ) |>
  distinct()

air_quality_by_sites <- air_quality_by_sites |>
  left_join(
    air_quality_locations,
    by = "LOCAL_SITE_NAME"
  )

#creating spatial object from the lat and long coordinates given in dataset 4326 is the ESPG identifier for WSG84
air_quality_by_sites_sf <- st_as_sf(
  air_quality_by_sites,
  coords = c("LONGITUDE", "LATITUDE"),
  crs = 4326
)

names(air_quality_by_sites_sf)


# Putting all dfs into one thing for the shiny app
data <- rbind(data, tracts, geo_abs_joined)

# For each shiny app, need to make a ui and a server. 
# The ui tells you how to set up the page. Title panel, wellpanel puts in middle
# leaflet map is the type of map
ui <- fluidPage(
  titlePanel("Washington, D.C. Census Tract Demographics"),
  wellPanel(style = "margin-left: 20px; margin-right: 20px",
            selectInput(inputId = "var_select",
                        label = "Variable to Display",
                        choices = unique(data$variable)),
            # Ok so in this version I also included a checkbox to overlay chronic absent
            checkboxInput(inputId = "show_schools",
                          label = "% of Students Chronically Absent by School",
                          value = FALSE),
            checkboxInput(
              inputId = "show_air_quality",
              label = "2024 Average Monthly AQI by Monitoring Site",
              value = FALSE
            )
            ), #seeing default checkbox input as false, not checked
  
  leafletOutput("map")
)

# reactive function, will continue changes
# since its reactive, will call back
# runs filter function, updates dataset
server <- function(input, output) {
  
  filtered_data <- reactive({
    data |>
      filter(variable == input$var_select) #where var select id comes in. filters var to what selected in dropdown.
  })
  
  filtered_data_pal <- reactive({ # pal we’re using (color, domain changes based on values of variable for estimate, basically rescaling based on diff variable ranges for diff variables 
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
    if (input$show_schools) { # if they check the school checkbox
      school_pal <- colorNumeric(palette = "YlOrRd", domain = geo_abs_joined$estimate) # yellow color to contrast w purple
      m <- m |>
        addCircleMarkers(data = geo_abs_joined, radius = 6, color = "black", weight = 1, # circle markers instead of circle
                         fillColor = ~school_pal(estimate), fillOpacity = 0.9,
                         label = ~paste0(short_name, " | ", estimate, "% chronically absent")) |>
        addLegend(data = geo_abs_joined, pal = school_pal, values = ~estimate,
                  title = "% Chronically Absent", position = "bottomleft")
    }
  # added if statement for air quality points, label for monthly and annual aqi, colored by annual aqi
    if (input$show_air_quality) {
      air_quality_pal <- colorNumeric(
        palette = c("#D9F0A3", "#78C679", "#238443", "#004529"),
        domain = air_quality_by_sites_sf$annual_aqi
      )
      m <- m |>
        addCircleMarkers(
          data = air_quality_by_sites_sf,
          radius = 7,
          color = "black",
          weight = 1,
          fillColor = ~air_quality_pal(annual_aqi),
          fillOpacity = 0.9,
          label = ~lapply(
            paste0(
              "<strong>", LOCAL_SITE_NAME, "</strong><br>",
              "Annual AQI: ", round(annual_aqi, 1), "<br>",
              "January: ", ifelse(is.na(January), "No data", round(January, 1)), "<br>",
              "February: ", ifelse(is.na(February), "No data", round(February, 1)), "<br>",
              "March: ", ifelse(is.na(March), "No data", round(March, 1)), "<br>",
              "April: ", ifelse(is.na(April), "No data", round(April, 1)), "<br>",
              "May: ", ifelse(is.na(May), "No data", round(May, 1)), "<br>",
              "June: ", ifelse(is.na(June), "No data", round(June, 1)), "<br>",
              "July: ", ifelse(is.na(July), "No data", round(July, 1)), "<br>",
              "August: ", ifelse(is.na(August), "No data", round(August, 1)), "<br>",
              "September: ", ifelse(is.na(September), "No data", round(September, 1)), "<br>",
              "October: ", ifelse(is.na(October), "No data", round(October, 1)), "<br>",
              "November: ", ifelse(is.na(November), "No data", round(November, 1)), "<br>",
              "December: ", ifelse(is.na(December), "No data", round(December, 1))
            ),
            htmltools::HTML
          )
        ) |>
        addLegend(
          data = air_quality_by_sites_sf,
          pal = air_quality_pal,
          values = ~annual_aqi,
          title = "Annual AQI",
          position = "bottomright"
        )
    }
    m
  })
}

shinyApp(ui = ui, server = server)
