library(shiny)
library(tidyverse)
library(tidycensus)
library(leaflet)
library(sf)
library(tigris)

# Setting API key for census data. Pulls data from census package
census_api_key("6fdd3d02e0762b982b0c2188bfd0d01b406d7c60")

# tigris tract shapefile
# selecting the variables- variable (names of variables), estimate (coorspondering num like income, geometry is shapefiles of census tracts)
tracts <- tracts(state = "DC") |>
  st_transform(crs = 4326) |>
  select(c(TRACTCE, NAMELSAD, geometry))

# after Simran reorganized the folders I could not for the life of me get the code to run because
# it kept saying no existing file or directory
# and i checked the file path like a billion times and still did not work
# so i did the read package thing that prof cook showed 
# reading in the neighborhoods data set
neighborhoods <- read.csv(here::here("data-files", "Neighborhood_Labels.csv"))

# convert neighborhood data into sf spatial object
# I used 3857 because the coords are in web mercator
neighborhood_points <- st_as_sf(
  neighborhoods,
  coords = c("X", "Y"),
  crs = 3857,
  remove = FALSE
)

# (from Ayla) transform neighborhood and tracts into the same coord system
neighborhood_points <- st_transform(
  neighborhood_points,
  st_crs(tracts)
)

# change them into a coord for distance calculations
tracts_projected <- st_transform(tracts, 4326)
points_projected <- st_transform(neighborhood_points, 4326)

# adding the ward shapefile
ward_shapefile <- st_read(here::here("data-files", "ward_2022"))   

# same set of things as other shapefiles
wards <- ward_shapefile |>
  mutate(ward = paste("Ward", WARD)) |>
  select(ward)                           

###

# From Rory: "Weights: 2020 Census blocks with population (POP20)"
dc_blocks <- blocks(state = "DC", year = 2020) |>
  # except i did st_transform instead of crs bc we alr have that
  st_transform(4326)

# Get a point from each block. Got st point on surface postgis documentation
# https://postgis.net/docs/ST_PointOnSurface.html
block_points <- st_point_on_surface(dc_blocks)

# (From Ayla) nearest neighborhoods points... but added block points too
nearest_neighborhoods <- st_nearest_feature(block_points, points_projected)
block_points$NEIGHBORHOOD <- neighborhoods$NAME[nearest_neighborhoods]

# changed the ward stuff to block points
# ward transform to point
# see what ward contains that point w the st_within join
# st_transform(4326) instead of WGS84 like b4
wards_projected <- st_transform(wards, 4326)
block_points <- st_join(block_points, wards_projected, join = st_within)

# sorting the tracts into neighborhoods and wards by weight 
tract_weighted <- block_points |>
  st_drop_geometry() |>
  # group tracts by by neighborhood and ward
  group_by(TRACTCE = TRACTCE20, neighborhood = NEIGHBORHOOD, ward) |>
  # weight by sum population
  summarize(pop = sum(POP20), .groups = "drop") |>
  # group tract var
  group_by(TRACTCE) |>
  slice_max(pop, n = 1, with_ties = FALSE) |>
  select(-pop)

# join tracts points with weighted by tract var
tracts_projected <- left_join(tracts_projected, tract_weighted, by = "TRACTCE")

# change neighborhood var name
tracts_projected <- tracts_projected |>
  rename(NEIGHBORHOOD = neighborhood)

# new tracts into polygons by neighborhood
neighborhood_polygons <- tracts_projected |>
  group_by(NEIGHBORHOOD) |>
  summarise(.groups = "drop") |>
  st_transform(4326)

# Names of variables that your pulling
# Geometry- shapefile instead of dataset
# variable called shortname- shortens name of census tract like tract 1 etc
# st transform from sf package, transforming everything to default coordinates
data <- get_acs(geography = "tract", variables = c("Median Household Income" = "S1901_C01_012", "Median Rent" = "DP04_0134", "Poverty Rate" = "S1701_C03_001", "Percent White Only" = "DP05_0037P", "Percent Foreign-Born" = "DP02_0094P"), year = 2024, state = "DC", geometry = TRUE, survey = "acs5") |>
  mutate(TRACTCE = substr(GEOID, nchar(GEOID) - 5, nchar(GEOID))) |>
  mutate(short_name = substr(NAME, 1, nchar(NAME) - 44)) |>
  st_transform(crs = 4326) |>
  select(c(variable, estimate, short_name, TRACTCE, geometry))

data <- data |>
  left_join(tract_weighted, by = "TRACTCE") |>
  filter(!is.na(neighborhood)) |>
  select(variable, estimate, short_name, neighborhood, ward, geometry)

# Loading in the public schools absentee and coordinates shapefile
schools_shapefile <- st_read(here::here("data-files", "school_geospatial"))

schools_shapefile <- st_transform(
  schools_shapefile,
  st_crs(tracts)
)
# crime data read in, this is points but just moved it into tract. Turns track ID into character var
crimes <- read.csv("https://hub.arcgis.com/api/v3/datasets/74d924ddc3374e3b977e6f002478cb9b_7/downloads/data?format=csv&spatialRefId=4326&where=1%3D1") |>
  filter(!is.na(CENSUS_TRACT)) |>
  mutate(TRACTCE = sprintf("%06d", CENSUS_TRACT)) |>
  group_by(TRACTCE) |>
  summarize(estimate = n())

# Join track shapefile with crimes dataset
# Changed second line fyi -alice
tracts <- left_join(tracts, crimes, by = join_by(TRACTCE)) |>
  left_join(tract_weighted, by = "TRACTCE") |>
  rename(short_name = NAMELSAD) |>
  mutate(variable = "Number of Crimes in 2025") |>
  select(variable, estimate, short_name, neighborhood, ward, geometry)

# transforming to 4326 is a coordinate reference system... way its projected on a map
tracts <- st_transform(tracts, crs = 4326)

# END tract/ward/neighborhood part!

# Loading in the absenteeism by school data
absentee <- read.csv(here::here("data-files", "absenteeism_ds.csv"))

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
  mutate(percent_abs = as.numeric(sub("%", "", percent_abs)),
         # school id as integer bc was str in other one
         SCHOOL_ID = as.integer(SCHOOL_ID))

# parks and rec
dc_parks_sf <- st_read(
  "https://opendata.dc.gov/api/download/v1/items/287eaa2ecbff4d699762bbc6795ffdca/geojson?layers=9")

dc_parks_sf <- st_transform(
  dc_parks_sf,
  st_crs(tracts)
)

dc_parks_sf <- dc_parks_sf |>
  mutate(park_id = row_number())

# counting each park in tract
park_counts <- st_join(
  tracts |> select(short_name),
  dc_parks_sf |> select(park_id),
  join = st_intersects,
  left = TRUE
) |>
  st_drop_geometry() |>
  group_by(short_name) |>
  summarise(
    park_estimate = n_distinct(park_id, na.rm = TRUE),
    .groups = "drop"
  )

# adding counts to geometry
park_tracts <- tracts |>
  left_join(park_counts, by = "short_name") |>
  mutate(
    estimate = coalesce(park_estimate, 0L),
    variable = "Number of Parks"
  ) |>
  select(variable, estimate, short_name, neighborhood, ward, geometry)

# ayla trying to transform each spatial object into same crs

tracts <- st_transform(tracts, 4326)

dc_parks_sf <- st_transform(dc_parks_sf, 4326)

schools_shapefile <- st_transform(
  schools_shapefile, 4326
)

wards <- st_transform(wards, 4326)

neighborhood_points <- st_transform(
  neighborhood_points, 4326
)

block_points <- st_transform(block_points, 4326)
# Joining shapefile and csv! Going to be honest I copied some of Una's code for this part! 
geo_abs_joined <- left_join(schools_shapefile, abs_clean, by = "SCHOOL_ID") |>
  filter(!is.na(percent_abs)) |>
  st_transform(crs = 4326) |>
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
data <- rbind(data, tracts, geo_abs_joined, park_tracts)

# dropdown shows list of area type (but grouped)
# filters to only that area type
area_choices <- list(
  "Citywide"      = "Citywide",
  "Wards"         = sort(unique(na.omit(data$ward))),
  "Neighborhoods" = sort(unique(na.omit(data$neighborhood)))
)

# rory repeat
blocks_sf <- blocks(state = "DC", year = 2020) |>
  st_transform(4326)

# To see where I figured out the variable names and broke this down:
# see blockview.qmd in our repo
block_data <- get_decennial(
  geography = "block", state = "DC", year = 2020, sumfile = "dhc",
  variables = c(
    pop         = "P1_001N",
    race_total  = "P3_001N",
    white       = "P3_002N",
    black       = "P3_003N",
    aian        = "P3_004N",
    asian       = "P3_005N",
    nhpi        = "P3_006N",
    other_race  = "P3_007N",
    two_or_more = "P3_008N",
    occ_total   = "H4_001N",
    owned_mort  = "H4_002N",
    owned_free  = "H4_003N",
    renter      = "H4_004N"
  ),
  output = "wide"
) |>
  mutate(
    owner      = owned_mort + owned_free,
    pct_white  = 100 * white / race_total,
    pct_black  = 100 * black / race_total,
    pct_asian  = 100 * asian / race_total,
    pct_renter = 100 * renter / occ_total,
    pct_owner  = 100 * owner / occ_total
  )

# add neighborhood and ward to each block (alr have b4 in block points)
block_lookup <- block_points |>
  st_drop_geometry() |>
  select(GEOID20, neighborhood = NEIGHBORHOOD, ward)

# join block vars estimates with blocks_sf shapefile
block_long <- block_data |>
  select(GEOID, pop, starts_with("pct_")) |>
  pivot_longer(-GEOID, names_to = "variable", values_to = "estimate") |>
  mutate(
    # renaming all the variables- no hispanic yet, will add next time
    variable = recode(variable,
      pop        = "Total Population (2020)",
      pct_white  = "Percent White Alone (2020)",
      pct_black  = "Percent Black Alone (2020)",
      pct_asian  = "Percent Asian Alone (2020)",
      pct_renter = "Percent Renter-Occupied (2020)",
      pct_owner  = "Percent Owner-Occupied (2020)"),
      estimate   = if_else(is.finite(estimate), estimate, NA_real_),
      short_name = paste("Block", GEOID)) |>
  left_join(block_lookup, by = c("GEOID" = "GEOID20")) |>
  left_join(select(blocks_sf, GEOID20, geometry),
            by = c("GEOID" = "GEOID20")) |>
  st_as_sf() |>
  select(variable, estimate, short_name, neighborhood, ward, geometry)



# For each shiny app, need to make a ui and a server. 
# The ui tells you how to set up the page. divided in half by map1 and map2
# *This is Alice*- I think that you can do the side by side in server but I did it in the ui part
ui <- fluidPage(
  titlePanel("Washington, D.C. Census Tract Demographics"),
  # list of tabs on top
  tabsetPanel(
    id = "main_tabs",
    # 1- side by side maps and histograms and sum stats 
    tabPanel("Compare Maps and Visualizations",
             fluidRow(
               # i put 6 for width cuz half the page but we could change - alice
               column(width = 6, 
                      selectInput("area1", "Area 1", choices = area_choices),
                      selectInput("var1", "Variable 1", choices = unique(data$variable)),
                      leafletOutput("map1", height = "500px"),
                      # added summary stats and histogram to output
                      tableOutput("stats1"),
                      plotOutput("hist1", height = "250px")
               ),
               column(width = 6,
                      selectInput("area2", "Area 2", choices = area_choices),
                      selectInput("var2", "Variable 2", choices = unique(data$variable)),
                      leafletOutput("map2", height = "500px"),
                      # added summary stats and histogram to output (2nd side)
                      tableOutput("stats2"),
                      plotOutput("hist2", height = "250px")
               )
             )
    ),
    # 2- MESSY! messing around with block / school sf bullshit
    tabPanel("Block/Neighborhood/School Breakdown"),
    
    # 3- initial mockup regression with few variables 
    tabPanel("Initial Mockup Regression- Few Variables"),
    
  # 2- MESSY! messing around with block / school sf bullshit
    tabPanel("Block/Neighborhood/School Breakdown",
    fluidRow(
      column(width = 12,
             selectInput("area_block", "Area", choices = area_choices),
             selectInput("var_block", "Variable", choices = unique(block_long$variable)),
             leafletOutput("map_block", height="600px"),
             tableOutput("stats_block"),
             plotOutput("hist_block", height="250px")
  )
  )
  ),
  
  # 3- initial mockup regression with few variables 
  tabPanel("Initial Mockup Regression- Few Variables",
    fluidPage(
      "We do not have the block regression stuff yet, I'll put it in tomorrow night-Alice"
  )
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
  
  # MAP FUNCTION!!! - function to put data in polygons and points on map
  make_map <- function(map_data) {
    pal <- colorNumeric("BuPu", domain = map_data$estimate)
    
    # Una, this is Alice! I realized the problem was that the second statement was supposed to be an "if else"
    # If is a point shapefile, will show circles 
    if (all(st_geometry_type(map_data) == "POINT")) {
      # Una, this is Alice! I realized the problem was that the second statement was supposed to be an "if else"
      # If is a point shapefile, will show circles 
      leaflet(map_data) |>
        leaflet::addProviderTiles("Stadia.StamenTonerLite") |>
        addCircles(color = ~pal(estimate), radius = 250, fillOpacity = 0.9,
                   label = ~paste0(short_name, " | ",
                                   format(estimate, big.mark = ",", scientific = FALSE))) |>
        addLegend(pal = pal, values = ~estimate, title = unique(map_data$variable))
    } else {
      # If it is a polygod shapefile, will show polygons
      leaflet(map_data) |>
        leaflet::addProviderTiles("Stadia.StamenTonerLite") |>
        addPolygons(color = "black", weight = 1, fillColor = ~pal(estimate), fillOpacity = 0.7,
                    label = ~paste0(short_name, " | ",
                                    format(estimate, big.mark = ",", scientific = FALSE))) |>
        addLegend(pal = pal, values = ~estimate, title = unique(map_data$variable))
    }
  }
  
  # SUMMARY STATS FUNCTION
  # takes the selected variable and calculates the n, median, mean, sd
  make_stats <- function(map_data) {
    vals <- map_data |> st_drop_geometry() |> filter(!is.na(estimate)) #drop geometry bc dont need map shapes
    data.frame(
      n      = nrow(vals),
      median = median(vals$estimate),
      mean   = mean(vals$estimate),
      sd     = sd(vals$estimate)
    )
  }
  
  # HISTOGRAM FUNCTION
  # takes the selected variable and makes histogram
  make_hist <- function(map_data) {
    ggplot(st_drop_geometry(map_data), aes(x = estimate)) +
      # we can change color / theme / bin width if you guys want
      geom_histogram(bins = 15, fill = "#8856a7", color = "white") +
      labs(x = unique(map_data$variable), y = "Count") +
      theme_minimal()
  }
  
  make_hist <- function(map_data) {
    ggplot(st_drop_geometry(map_data), aes(x = estimate)) +
      geom_histogram(bins = 15, fill = "#8856a7", color = "white") +
      labs(x = unique(map_data$variable), y = "Count") +
      theme_minimal()
  }
  
  # two maps output
  output$map1 <- renderLeaflet({ make_map(filtered1()) })
  output$map2 <- renderLeaflet({ make_map(filtered2()) })
  
  # two summary stats output
  output$stats1 <- renderTable({ make_stats(filtered1()) })
  output$stats2 <- renderTable({ make_stats(filtered2()) })
  
  # two histogram output
  output$hist1 <- renderPlot({ make_hist(filtered1()) })
  output$hist2 <- renderPlot({ make_hist(filtered2()) })
  
# function block by block for tab 2
  filtered3 <- reactive({
    d <- block_long |> filter(variable == input$var_block)
    if (input$area_block != "Citywide") {
      d <- d |> filter(neighborhood == input$area_block | ward == input$area_block)
    }
    d
  })
  
  # block-by-block-app maps stats hist!
  output$map_block   <- renderLeaflet({ make_map(filtered3()) })
  output$stats_block <- renderTable({ make_stats(filtered3()) })
  output$hist_block  <- renderPlot({ make_hist(filtered3()) })
}



# this is what runs shiny app
shinyApp(ui = ui, server = server)