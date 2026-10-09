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

## START AYLA PART

# tigris tract shapefile
# selecting the variables- variable (names of variables), estimate (coorspondering num like income, geometry is shapefiles of census tracts)
tracts <- tracts(state = "DC") |>
  st_transform(crs = "WGS84") |>
  select(c(TRACTCE, NAMELSAD, geometry))

# reading in the neighborhoods data set
neighborhoods <- read.csv("Neighborhood_Labels.csv")

# convert neighborhood data into sf spatial object
# I used 3857 because the coords are in web mercator
neighborhood_points <- st_as_sf(
  neighborhoods,
  coords = c("X", "Y"),
  crs = 3857,
  remove = FALSE
  
)

# transform neighborhood and tracts into the same coord system
neighborhood_points <- st_transform(
  neighborhood_points,
  st_crs(tracts)
)

# change them into a coord for distance calculations
tracts_projected <- st_transform(tracts, 26985)
points_projected <- st_transform(neighborhood_points, 26985)

# create one point inside each census tract so that i can find neares neighborhood point
tract_points <- st_point_on_surface(tracts_projected)



# finding nearest neighborhoods points
nearest_neighborhoods <- st_nearest_feature(
  tract_points,
  points_projected
)

# assigning census tracts the name of the nearest neighborhood
tracts_projected$NEIGHBORHOOD <- neighborhoods$NAME[nearest_neighborhoods]



# dissolve the census tract polygons that share the same neighborhood
neighborhood_polygons <- tracts_projected |>
  group_by(NEIGHBORHOOD) |>
  summarise(.groups = "drop")
# re transform back for leaflet
neighborhood_polygons <- neighborhood_polygons |>
  st_transform(4326)

tract_neighborhood <- tracts_projected |>
  st_drop_geometry() |>
  select(TRACTCE, NEIGHBORHOOD) |>
  distinct()

# adding the ward shapefile
ward_shapefile <- st_read("ward_2022")   
              
# same set of things as other shapefiles
wards <- ward_shapefile |>
  mutate(ward = paste("Ward", WARD)) |>
  select(ward)                           

# Here I followed the logic of aylas neighborhood code for wards

# ward coordinate crs to points
wards_projected <- st_transform(wards, st_crs(tract_points))

# st_within to see what ward contains that point
tract_ward <- st_join(tract_points, wards_projected, join = st_within) |>
  st_drop_geometry() |>
  select(TRACTCE, ward) |>
  distinct()

# joining ward and neighborhood by tract variable 
tract_lookup <- tract_neighborhood |>
  rename(neighborhood = NEIGHBORHOOD) |>
  left_join(tract_ward, by = "TRACTCE")

# Names of variables that your pulling
# Geometry- shapefile instead of dataset
# variable called shortname- shortens name of census tract like tract 1 etc
# st transform from sf package, transforming everything to default coordinates
data <- get_acs(geography = "tract", variables = c("Median Household Income" = "S1901_C01_012", "Median Rent" = "DP04_0134", "Poverty Rate" = "S1701_C03_001", "Percent White Only" = "DP05_0037P", "Percent Foreign-Born" = "DP02_0094P"), year = 2024, state = "DC", geometry = TRUE, survey = "acs5") |>
  mutate(TRACTCE = substr(GEOID, nchar(GEOID) - 5, nchar(GEOID))) |>
  mutate(short_name = substr(NAME, 1, nchar(NAME) - 44)) |>
  st_transform(crs = "WGS84") |>
  select(c(variable, estimate, short_name, TRACTCE, geometry))

data <- data |>
  left_join(tract_lookup, by = "TRACTCE") |>
  filter(!is.na(neighborhood)) |>
  select(variable, estimate, short_name, neighborhood, ward, geometry)

# Loading in the public schools absentee and coordinates shapefile
schools_shapefile <- st_read("school_geospatial")

  
# crime data read in, this is points but just moved it into tract. Turns track ID into character var
crimes <- read.csv("https://hub.arcgis.com/api/v3/datasets/74d924ddc3374e3b977e6f002478cb9b_7/downloads/data?format=csv&spatialRefId=26985&where=1%3D1") |>
  filter(!is.na(CENSUS_TRACT)) |>
  mutate(TRACTCE = sprintf("%06d", CENSUS_TRACT)) |>
  group_by(TRACTCE) |>
  summarize(estimate = n())

# Join track shapefile with crimes dataset
# Changed second line fyi -alice
tracts <- left_join(tracts, crimes, by = join_by(TRACTCE)) |>
  left_join(tract_lookup, by = "TRACTCE") |>
  rename(short_name = NAMELSAD) |>
  mutate(variable = "Number of Crimes in 2025") |>
  select(variable, estimate, short_name, neighborhood, ward, geometry)


# transforming to WGS84 is a coordinate reference system... way its projected on a map. If shapefile is not in WGS84, transforms into
tracts <- st_transform(tracts, crs = "WGS84")

## END AYLA PART

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

# same thing as b4, just doing it for schools 
# adding coordinates for each school
neighborhood_join1  <- neighborhood_polygons |>
  rename(neighborhood = NEIGHBORHOOD) |>
  st_transform(st_crs(geo_abs_joined))
ward_join1 <- st_transform(wards, st_crs(geo_abs_joined))

# adding neighborhoods and wards cols
geo_abs_joined <- geo_abs_joined |>
  st_join(neighborhood_join1,  join = st_within) |> 
  st_join(ward_join1, join = st_within) |>
  select(variable, estimate, short_name, neighborhood, ward, geometry)

# Putting all dfs into one thing for the shiny app
data <- rbind(data, tracts, geo_abs_joined)

# dropdown shows list of area type (but grouped)
# filters to only that area type
area_choices <- list(
  "Citywide"      = "Citywide",
  "Wards"         = sort(unique(na.omit(data$ward))),
  "Neighborhoods" = sort(unique(na.omit(data$neighborhood)))
)

# For each shiny app, need to make a ui and a server. 
# The ui tells you how to set up the page. divided in half by map1 and map2
# *This is Alice*- I think that you can do the side by side in server but I did it in the ui part
ui <- fluidPage(
  titlePanel("Washington, D.C. Census Tract Demographics"),
  fluidRow(
    # i put 6 for width cuz half the page but we could change - alice
    column(width = 6, 
           selectInput("area1", "Area 1", choices = area_choices),
           selectInput("var1", "Variable 1", choices = unique(data$variable)),
           leafletOutput("map1", height = "500px")
    ),
    column(width = 6,
           selectInput("area2", "Area 2", choices = area_choices),
           selectInput("var2", "Variable 2", choices = unique(data$variable)),
           leafletOutput("map2", height = "500px")
    )
  )
)

# reactive function, will continue changes
# since its reactive, will call back
# runs filter function, updates dataset
server <- function(input, output) {
  
  # reactive filter for map1
  filtered1 <- reactive({
    d <- data |> filter(variable == input$var1)
    if (input$area1 != "Citywide") {
      # only rows where column name matches for ward
      d <- d |> filter(neighborhood == input$area1 | ward == input$area1)
    }
    d

  })
  # same thing for filter 2
  filtered2 <- reactive({
    d <- data |> filter(variable == input$var2)
    if (input$area2 != "Citywide") {
      d <- d |> filter(neighborhood == input$area2 | ward == input$area2)
    }
    d
  })
  
  make_map <- function(map_data) {
    pal <- colorNumeric("BuPu", domain = map_data$estimate)
    
    # Una, this is Alice! I realized the problem was that the second statement was supposed to be an "if else"
    # If is a point shapefile, will show circles 
    if (all(st_geometry_type(map_data) == "POINT")) {
      # Una, this is Alice! I realized the problem was that the second statement was supposed to be an "if else"
      # If is a point shapefile, will show circles 
      leaflet(map_data) |>
        addTiles() |>
        addCircles(color = ~pal(estimate), radius = 250, fillOpacity = 0.9,
                   label = ~paste0(short_name, " | ",
                                   format(estimate, big.mark = ",", scientific = FALSE))) |>
        addLegend(pal = pal, values = ~estimate, title = unique(map_data$variable))
    } else {
      # If it is a polygod shapefile, will show polygons
      leaflet(map_data) |>
        addTiles() |>
        addPolygons(color = "black", weight = 1, fillColor = ~pal(estimate), fillOpacity = 0.7,
                    label = ~paste0(short_name, " | ",
                                    format(estimate, big.mark = ",", scientific = FALSE))) |>
        addLegend(pal = pal, values = ~estimate, title = unique(map_data$variable))
    }
  }
  
  
  
  
  output$map1 <- renderLeaflet({ make_map(filtered1()) })
  output$map2 <- renderLeaflet({ make_map(filtered2()) })
}



#this is what runs shiny app
shinyApp(ui = ui, server = server)