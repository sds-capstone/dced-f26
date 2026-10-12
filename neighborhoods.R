library(shiny)
library(tidyverse)
library(tidycensus)
library(leaflet)
library(sf)
library(tigris)

# Setting API key for census data. Pulls data from census package
census_api_key("34df405496a792f31bd4cf10741ff37e850c3ab0")

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
  left_join(tract_neighborhood, by = "TRACTCE") |>
  filter(!is.na(NEIGHBORHOOD)) |>
  mutate(short_name = NEIGHBORHOOD) |>
  select(variable, estimate, short_name, geometry)


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
                        choices = unique(data$variable))),
  leafletOutput("map")
)

# reactive function, will continue changes
# since its reactive, will call back
# runs filter function, updates dataset
server <- function(input, output) {
  
  filtered_data <- reactive({ #where var select id comes in. filters var to what selected in dropdown.
    data |>
      filter(variable == input$var_select)
  })
  
  filtered_data_pal <- reactive({ # pal we’re using (color, domain changes based on values of variable for estimate, basically rescaling based on diff variable ranges for diff variables 
    colorNumeric(palette = "BuPu", domain = filtered_data()$estimate)
  })
  
  output$map <- renderLeaflet({
    pal <- filtered_data_pal()
    # No longer need the if/else point polygon logic    
    
    # Una, this is Alice! I realized the problem was that the second statement was supposed to be an "if else"
    # If is a point shapefile, will show circles 
    if (unique(sf::st_geometry_type(filtered_data()) == "POINT")) {
      leaflet(filtered_data()) |>
        addTiles() |>
        addCircles(color = ~pal(estimate), radius = 250, fillOpacity = 0.9, label = ~paste0(short_name, " | ", format(estimate, big.mark = ",", scientific = FALSE))) |>
        addLegend(pal = pal, values = ~filtered_data()$estimate, title = "Estimate")
    }
    
    
    # If it is a polygod shapefile, will show polygons
    else if (unique(sf::st_geometry_type(filtered_data()) == "POLYGON")) {
      leaflet(filtered_data()) |>
        addTiles() |>
        addPolygons(color = "black", weight = 1, fillColor = ~pal(estimate), fillOpacity = 0.7, label = ~paste0(short_name, " | ", format(estimate, big.mark = ",", scientific = FALSE))) |>
        addLegend(pal = pal, values = ~filtered_data()$estimate, title = "Estimate")
    }
  })
}



#this is what runs shiny app
shinyApp(ui = ui, server = server)