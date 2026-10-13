library(shiny)
library(tidyverse)
library(tidycensus)
library(leaflet)
library(sf)
library(tigris)
library(arcgislayers)

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

# transforming to WGS84 is a coordinate reference system... way its projected on a map. If shapefile is not in WGS84, transforms into
tracts <- st_transform(tracts, crs = "WGS84")

# crime data read in, this is points but just moved it into tract. Turns tract ID into character var
crimes <- read.csv("https://hub.arcgis.com/api/v3/datasets/74d924ddc3374e3b977e6f002478cb9b_7/downloads/data?format=csv&spatialRefId=26985&where=1%3D1") |>
  filter(!is.na(CENSUS_TRACT)) |>
  mutate(TRACTCE = sprintf("%06d", CENSUS_TRACT)) |>
  group_by(TRACTCE) |>
  summarize(estimate = n())

# Join track shapefile with crimes dataset
crimes <- left_join(tracts, crimes, by = join_by(TRACTCE)) |>
  rename(short_name = NAMELSAD) |>
  select(!TRACTCE) |>
  mutate(variable = "Number of Crimes in 2025")

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

# loading child development centers data into R
child_development <- arc_select(arc_open("https://maps2.dcgis.dc.gov/dcgis/rest/services/DCGIS_DATA/Public_Service_WebMercator/MapServer/33"), fields = "")

child_development <- st_transform(child_development, crs = "WGS84")

# assigning each center to a Census tract
centers_intersect <- st_intersection(child_development, tracts)

# counting number of centers per Census tract
centers_intersect <- as.data.frame(centers_intersect)[c("NAMELSAD")] |>
  group_by(NAMELSAD) |>
  mutate(estimate = n()) |>
  ungroup() |>
  distinct()

# joining tracts with centers
centers_intersect <- left_join(tracts, centers_intersect, by = join_by("NAMELSAD")) |>
  rename(short_name = NAMELSAD) |>
  select(!TRACTCE) |>
  mutate(estimate = case_when(is.na(estimate) ~ 0,
                              .default = estimate),
         variable = "Number of Child Development Centers")

# loading affordable housing data into R and filtering to completed projects only
affordable_housing <- arc_select(arc_open("https://maps2.dcgis.dc.gov/dcgis/rest/services/DCGIS_DATA/Property_and_Land_WebMercator/FeatureServer/62"), fields = c("TOTAL_AFFORDABLE_UNITS", "STATUS_PUBLIC")) |>
  filter(STATUS_PUBLIC == "Completed 2015 to Date") |>
  select(!STATUS_PUBLIC)

affordable_housing <- st_transform(affordable_housing, crs = "WGS84")

# assigning each point to a Census tract
housing_intersect <- st_intersection(affordable_housing, tracts)

# counting number of affordable units per Census tract
housing_intersect <- as.data.frame(housing_intersect)[c("NAMELSAD", "TOTAL_AFFORDABLE_UNITS")] |>
  group_by(NAMELSAD) |>
  mutate(estimate = sum(TOTAL_AFFORDABLE_UNITS)) |>
  ungroup() |>
  select(!TOTAL_AFFORDABLE_UNITS) |>
  distinct()

# joining tracts with housing units
housing_intersect <- left_join(tracts, housing_intersect, by = join_by("NAMELSAD")) |>
  rename(short_name = NAMELSAD) |>
  select(!TRACTCE) |>
  mutate(estimate = case_when(is.na(estimate) ~ 0,
                              .default = estimate),
         variable = "Affordable Housing Units Constructed Since 2015")

# loading transit stops data into R
# it doesn't load in unless I do stop_type_text as a field
transit_stops <- arc_select(arc_open("https://services.arcgis.com/xOi1kZaI0eWDREZv/ArcGIS/rest/services/NTAD_National_Transit_Map_Stops/FeatureServer/0"), fields = c("stop_type_text"))

transit_stops <- st_transform(transit_stops, crs = "WGS84")

# assigning each stop to a Census tract
transit_intersect <- st_intersects(transit_stops, tracts)

# pulling row numbers representing stops located in DC tracts
rows <- c()
for (i in 1:nrow(transit_stops)) {
  if (length(transit_intersect[[i]]) != 0) {
    rows <- c(rows, transit_intersect[[i]])
  }
}

# matching row numbers to Census tracts
transit_final <- data.frame(short_name = character(), geometry = list())
transit_final <- st_as_sf(transit_final, wkt = "geometry", crs = "WGS84")

transit_tracts <- tracts |>
  rename(short_name = NAMELSAD) |>
  select(!TRACTCE)

for (i in rows) {
  transit_final <- rbind(transit_tracts[i,], transit_final)
}

# counting number of transit stops in each Census tract
transit_final <- transit_final |>
  group_by(short_name) |>
  mutate(estimate = n()) |>
  ungroup() |>
  distinct() |>
  mutate(estimate = case_when(is.na(estimate) ~ 0,
                              .default = estimate),
         variable = "Number of Transit Stops")

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
data <- rbind(data, crimes, geo_abs_joined, built_environment, centers_intersect, housing_intersect, transit_final)

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
      leaflet::addProviderTiles("Stadia.StamenTonerLite") |>
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
