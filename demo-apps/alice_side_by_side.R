library(shiny)
library(tidyverse)
library(tidycensus)
library(leaflet)
library(sf)
library(tigris)
library(arcgislayers)

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

# change neigh var name
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

# END tract/ward/neighborhood part!

#Crime Data

# crime data read in, this is points but just moved it into tract. Turns track ID into character var
crimes <- read.csv("https://hub.arcgis.com/api/v3/datasets/74d924ddc3374e3b977e6f002478cb9b_7/downloads/data?format=csv&spatialRefId=4326&where=1%3D1") |>
  filter(!is.na(CENSUS_TRACT)) |>
  mutate(TRACTCE = sprintf("%06d", CENSUS_TRACT)) |>
  group_by(TRACTCE) |>
  summarize(estimate = n())

# transforming to 4326 is a coordinate reference system... way its projected on a map
crime_tracts <- st_transform(tracts, crs = 4326)

# Join track shapefile with crimes dataset
# Changed second line fyi -alice
crime_tracts <- left_join(tracts, crimes, by = join_by(TRACTCE)) |>
  left_join(tract_weighted, by = "TRACTCE") |>
  rename(short_name = "NAMELSAD") |>
  mutate(variable = "Number of Crimes in 2025") |>
  select(variable, estimate, short_name, neighborhood, ward, geometry)

#School Absenteeism - point data for checkbox

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
         SCHOOL_ID = as.integer(SCHOOL_ID))

# Joining shapefile and csv! Going to be honest I copied some of Una's code for this part! 
geo_abs_joined <- left_join(schools_shapefile, abs_clean, by = "SCHOOL_ID") |>
  filter(!is.na(percent_abs)) |>
  st_transform(crs = "WGS84") |>
  mutate(variable = "% of Students Chronically Absent, 24-25",
         estimate = percent_abs,
         short_name = SCHOOL_NAM) |>
  select(variable, estimate, short_name, geometry)

# joining with neighborhood and ward 
neighborhood_join_abs <- neighborhood_polygons |>
  rename(neighborhood = NEIGHBORHOOD) |>
  st_transform(st_crs(geo_abs_joined))

ward_join_abs <- st_transform(wards, st_crs(geo_abs_joined))

geo_abs_joined <- geo_abs_joined |>
  st_join(neighborhood_join_abs, join = st_within) |>
  st_join(ward_join_abs, join = st_within) |>
  select(variable, estimate, short_name, neighborhood, ward, geometry)

# Air Quality - point for checkbox

#Loading in air quality data
air_quality <- read.csv(here::here("data-files", "air-quality-history.csv"))

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

#Built Environment

# Loading in built environment indicators and selecting relevant columns
built_environment <- st_read(here::here("data-files", "built_environment_indicators")) |>
  select(c(TRACTCE, m1_1_schoo:m9_5_HIN))

# Changing column names so they're informative
colnames(built_environment)[2:42] <- c(
  "Percent Tract Within 15-Minute Walk to School", "Percent Tract Within 15-Minute Walk to Modernized School", 
  "Percent Tract Within 15-Minute Walk to Playground", "Percent Tract Within 2-Minute Walk of School Crossing Guard", 
  "Percent Tract Along Safe Route to School", "Percent Tract Within 15-Minute Walk of Library", 
  "Percent Tract Within 15-Minute Walk of Wireless Hotspot", "Percent Households With Broadband Internet", 
  "Percent Tract Within 15-Minute Walk of Recreation Center", "Percent Households With Work Commute Under 45 Mins", 
  "Percent Tract Within 15-Minute Walk of Banking Institution", "Percent Tract Within 15-Minute Walk of Cash Checking Institution", 
  "Percent Buildings of Good Quality", "Percent Homes Built Since 1970", "Percent Housing Units Affordable", 
  "Percent Tract Within 2-Minute Walk of Vacant or Blighted House", "Percent Tract Within 2-Minute Walk of Bus Stop", 
  "Percent Tract Within 15-Minute Walk of Metro Station", "Percent Tract Within 5-Minute Walk of Capital Bikeshare Location", 
  "Percent Street Area With Bike Lanes", "Percent 311 Calls Made for Sidewalk Repair", "Percent Tract Alleys and Parking Lots", 
  "Percent Tract Within 15-Minute Walk of Grocery Store", "Percent Tract in Low Food Access Area", 
  "Percent Tract Within 15-Minute Walk of Farmers Market", "Percent Tract Within 15-Minute Walk of Healthy Corner Store", 
  "Percent Tract Within 5-Minute Walk of Restaurant", "Percent Tract Within 15-Minute Walk of Liquor Store", 
  "Percent Tract Within 15-Minute Walk of Health Care Facility", "Percent Tract Within 15-Minute Walk of Mental Health Provider", 
  "Percent Tract With Tree Canopy", "Percent Tract Within 10-Minute Walk of Park", "Percent Tract Within 1/4 Mile of Trail", 
  "Land Use Diversity Score (0-1)", "Percent Positive Land Uses", "Percent Tract Within Floodplain", 
  "Percent Tract Within 2-Minute Walk of Vacant Lot", "Percent Sidewalks Within 30 Feet of Streetlight", 
  "Percent Tract Within 15-Minute Walk of Police Department", "Percent Tract Within 15-Minute Walk of Fire Station", 
  "Percent Tract Within 250 Feet of High Injury Network Corridor"
)

# pivoting to longer format and reformatting percentage variables to percent, and rounding each number to 2 decimal places
built_environment <- built_environment |>
  pivot_longer('Percent Tract Within 15-Minute Walk to School':'Percent Tract Within 250 Feet of High Injury Network Corridor', names_to = "variable", values_to = "estimate") |>
  mutate(estimate = case_when(variable == "Land Use Diversity Score (0-1)" ~ round(estimate, 2),
                              .default = round(estimate*100, 2)))

# transforming CRS to match other data
built_environment <- st_transform(built_environment, crs = "WGS84")

#tract names and join
tract_base <- tracts(state = "DC") |>
  st_transform(crs = 4326) |>
  select(TRACTCE, short_name = NAMELSAD, geometry) |>
  st_drop_geometry()

built_environment_tracts <- built_environment |>
  left_join(tract_base, by = "TRACTCE") |>
  left_join(tract_weighted, by = "TRACTCE") |>
  st_as_sf() |>
  select(variable, estimate, short_name, neighborhood, ward, geometry)

# Child Development Centers

child_development <- arc_select(arc_open("https://maps2.dcgis.dc.gov/dcgis/rest/services/DCGIS_DATA/Public_Service_WebMercator/MapServer/33"), fields = "")

child_development <- st_transform(child_development, crs = 4326)

# assigning each center to a Census tract
centers_intersect <- st_intersection(child_development, tracts)

# counting number of centers per Census tract
centers_count <- as.data.frame(centers_intersect)[c("TRACTCE", "NAMELSAD")] |>
  group_by(TRACTCE, NAMELSAD) |>
  summarise(estimate = n(), .groups = "drop")

# joining tracts, weights, and formatting columns to match side-by-side structure
child_development_tracts <- tracts |>
  select(TRACTCE, NAMELSAD, geometry) |>
  left_join(centers_count, by = c("TRACTCE", "NAMELSAD")) |>
  left_join(tract_weighted, by = "TRACTCE") |>
  mutate(
    estimate = case_when(is.na(estimate) ~ 0, .default = estimate),
    variable = "Number of Child Development Centers",
    short_name = NAMELSAD
  ) |>
  select(variable, estimate, short_name, neighborhood, ward, geometry)

# Affordable Housing

# loading affordable housing data into R and filtering to completed projects only
affordable_housing <- arc_select(arc_open("https://maps2.dcgis.dc.gov/dcgis/rest/services/DCGIS_DATA/Property_and_Land_WebMercator/FeatureServer/62"), fields = c("TOTAL_AFFORDABLE_UNITS", "STATUS_PUBLIC")) |>
  filter(STATUS_PUBLIC == "Completed 2015 to Date") |>
  select(!STATUS_PUBLIC)

affordable_housing <- st_transform(affordable_housing, crs = "WGS84")

# assigning each point to a Census tract
housing_intersect <- st_intersection(affordable_housing, tracts)

# counting number of affordable units per Census tract
housing_counts <- as.data.frame(housing_intersect)[c("NAMELSAD", "TOTAL_AFFORDABLE_UNITS")] |>
  group_by(NAMELSAD) |>
  mutate(estimate = sum(TOTAL_AFFORDABLE_UNITS)) |>
  ungroup() |>
  select(!TOTAL_AFFORDABLE_UNITS) |>
  distinct()

# joining tracts with housing units
affordable_housing_tracts <- tracts |>
  left_join(housing_counts, by = join_by("NAMELSAD")) |>
  left_join(tract_weighted, by = join_by("TRACTCE")) |>
  rename(short_name = NAMELSAD) |>
  mutate(estimate = case_when(is.na(estimate) ~ 0,
                              .default = estimate),
         variable = "Affordable Housing Units Constructed Since 2015") |>
  select(variable, estimate, short_name, neighborhood, ward, geometry)

# Transit Stops

# loading transit stops data into R
# it doesn't load in unless I do stop_type_text as a field
transit_stops <- arc_select(arc_open("https://services.arcgis.com/xOi1kZaI0eWDREZv/ArcGIS/rest/services/NTAD_National_Transit_Map_Stops/FeatureServer/0"), fields = c("stop_type_text"))

transit_stops <- st_transform(transit_stops, crs = "WGS84") |>
  mutate(stop_id = row_number())

transit_counts <- st_join(
  tracts |> select(TRACTCE),
  transit_stops |> select(stop_id),
  join = st_intersects,
  left = TRUE
) |>
  st_drop_geometry() |>
  group_by(TRACTCE) |>
  mutate(
    estimate = n_distinct(stop_id, na.rm = TRUE),
    .groups = "drop"
  )

transit_tracts <- tracts |>
  select(TRACTCE, NAMELSAD, geometry) |>
  left_join(transit_counts, by = "TRACTCE") |>
  left_join(tract_weighted, by = "TRACTCE") |>
  mutate(
    variable = "Number of Transit Stops",
    short_name = NAMELSAD
  ) |>
  select(variable, estimate, short_name, neighborhood, ward, geometry)

# Parks & Rec

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
  tracts |> select(TRACTCE, short_name = NAMELSAD),
  dc_parks_sf |> select(park_id),
  join = st_intersects,
  left = TRUE
) |>
  st_drop_geometry() |>
  group_by(TRACTCE, short_name) |>
  summarise(
    park_estimate = n_distinct(park_id, na.rm = TRUE),
    .groups = "drop"
  )

# adding counts to geometry
park_tracts <- tracts |>
  select(TRACTCE, geometry) |>
  left_join(park_counts, by = "TRACTCE") |>
  left_join(tract_weighted, by = "TRACTCE") |>
  mutate(
    estimate = coalesce(park_estimate, 0L),
    variable = "Number of Parks"
  ) |>
  select(variable, estimate, short_name, neighborhood, ward, geometry)

# Putting all dfs into one thing for the shiny app
data <- rbind(data, crime_tracts, geo_abs_joined, built_environment_tracts, child_development_tracts, affordable_housing_tracts, transit_tracts, park_tracts)

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

abs_var <- unique(geo_abs_joined$variable)

# this part is iffy but i did mean absenteeism of the schools in each tract for NOW
tract_abs <- geo_abs_joined |>
  st_join(select(tracts, tract = NAMELSAD), join = st_within) |>
  st_drop_geometry() |>
  filter(!is.na(tract)) |>
  group_by(tract) |>
  summarize(absentee_rate = mean(estimate, na.rm = TRUE),
            n_schools     = n(),
            .groups = "drop")

# tract vars -> one value per tract (average of its tracts)
tract_vars <- data |>
  st_drop_geometry() |>
  filter(variable != abs_var, !is.na(short_name)) |>
  group_by(tract = short_name, variable) |>
  summarize(estimate = mean(estimate, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = variable, values_from = estimate)

# theres only one row for tract- join by tract
tract_data <- left_join(tract_abs, tract_vars, by = "tract")

# remove absenteeism from b4 dropdown
reg_vars <- setdiff(unique(data$variable), abs_var)

## not relevant to map. renaming variables for the regression tab, do you guys like the 10-point thing?
reg_steps <- c("Median Household Income"  = 10000,
               "Median Rent"              = 100,
               "Poverty Rate"             = 10,
               "Percent White Only"       = 10,
               "Percent Foreign-Born"     = 10,
               "Number of Crimes in 2025" = 10)
reg_step_text <- c("Median Household Income"  = "$10,000 increase",
                   "Median Rent"              = "$100 increase",
                   "Poverty Rate"             = "10-point increase",
                   "Percent White Only"       = "10-point increase",
                   "Percent Foreign-Born"     = "10-point increase",
                   "Number of Crimes in 2025" = "10-point increase")

# bdd691cddd8e503d7d82f0f37497bd5dd58a44f1

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
                      checkboxInput("show_schools1", "% of Students Chronically Absent by School", value = FALSE),
                      checkboxInput("show_air_quality1", "2024 Air Quality Monitoring Sites", value = FALSE),
                      leafletOutput("map1", height = "500px"),
                      # added summary stats and histogram to output
                      tableOutput("stats1"),
                      plotOutput("hist1", height = "250px")
               ),
               column(width = 6,
                      selectInput("area2", "Area 2", choices = area_choices),
                      selectInput("var2", "Variable 2", choices = unique(data$variable)),
                      checkboxInput("show_schools2", "% of Students Chronically Absent by School", value = FALSE),
                      checkboxInput("show_air_quality2", "2024 Air Quality Monitoring Sites", value = FALSE),
                      leafletOutput("map2", height = "500px"),
                      # added summary stats and histogram to output (2nd side)
                      tableOutput("stats2"),
                      plotOutput("hist2", height = "250px")
               )
    ),
    
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
  # title explanation
  tabPanel("Initial OLS Regression"),
  p("What is the impact of Variable 1 on School Chronic absenteeism rate, while controlling for Variable 2?"),
    fluidRow(
      # drop down to select var1
      # set default to perc white
      column(width = 6,
             selectInput("var_reg1", "Variable 1", choices = reg_vars,
                         selected = "Percent White Only")
             ),
      # dropdonw to select var2
      # se default to income
      column(width = 6,
             selectInput("var_reg2", "Variable 2", choices = reg_vars,
                         selected = "Median Household Income")
      )
      ),
  # super quick label- need to change
  h4("Simple Linear Regression Between Variables"),
  # text description
  textOutput("reg_interpretation"),
  # coef table
  h4("Coefficient table"),
  # regression table
  tableOutput("reg_table"),
  # scatterplot with var 1 and chronic abs, fitted value lined is 
  plotOutput("reg_scatter", height = "400px"),
  # residual plot
  plotOutput("reg_resid", height = "300px")
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
  make_map <- function(map_data, show_schools = FALSE, show_air_quality = FALSE) {
    pal <- colorNumeric("BuPu", domain = map_data$estimate)
    
    # Una, this is Alice! I realized the problem was that the second statement was supposed to be an "if else"
    # If is a point shapefile, will show circles 
    if (all(st_geometry_type(map_data) == "POINT")) {
      # Una, this is Alice! I realized the problem was that the second statement was supposed to be an "if else"
      # If is a point shapefile, will show circles 
      m <- leaflet(map_data) |>
        leaflet::addProviderTiles("Stadia.StamenTonerLite") |>
        addCircles(color = ~pal(estimate), radius = 250, fillOpacity = 0.9,
                   label = ~paste0(short_name, " | ",
                                   format(estimate, big.mark = ",", scientific = FALSE))) |>
        addLegend(pal = pal, values = ~estimate, title = unique(map_data$variable))
    } else {
      # If it is a polygod shapefile, will show polygons
      m <- leaflet(map_data) |>
        leaflet::addProviderTiles("Stadia.StamenTonerLite") |>
        addPolygons(color = "black", weight = 1, fillColor = ~pal(estimate), fillOpacity = 0.7,
                    label = ~paste0(short_name, " | ",
                                    format(estimate, big.mark = ",", scientific = FALSE))) |>
        addLegend(pal = pal, values = ~estimate, title = unique(map_data$variable))
    }
    
    if (show_schools) { # if they check the school checkbox
      school_pal <- colorNumeric(palette = "YlOrRd", domain = geo_abs_joined$estimate) # yellow color to contrast w purple
      m <- m |>
        addCircleMarkers(data = geo_abs_joined, radius = 6, color = "black", weight = 1, # circle markers instead of circle
                         fillColor = ~school_pal(estimate), fillOpacity = 0.9,
                         label = ~paste0(short_name, " | ", estimate, "% chronically absent")) |>
        addLegend(data = geo_abs_joined, pal = school_pal, values = ~estimate,
                  title = "% Chronically Absent", position = "bottomleft")
    }
    
    if (show_air_quality){
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
  output$map1 <- renderLeaflet({ make_map(filtered1(), show_air_quality = input$show_air_quality1, show_schools = input$show_schools1) })
  output$map2 <- renderLeaflet({ make_map(filtered2(), show_air_quality = input$show_air_quality2, show_schools = input$show_schools2) })
  
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

# tab 3 (regression) 
# one row per tract (y = absenteeism, x1 = var 1, x2 = var 2) 
reg_data <- reactive({
  validate(need(input$var_reg1 != input$var_reg2, "Pick different variables."))
  d <- tract_data |>                                              
    transmute(tract,                                                
              y  = absentee_rate,
              x1 = .data[[input$var_reg1]],
              x2 = .data[[input$var_reg2]]) |>
    drop_na()
  validate(need(nrow(d) > 3, "Not enough tracts with data for this pair of variables."))   # SWAP: neighborhoods
  d
})

# multiple linear regression, fit by ordinary least squares (OLS)
reg_model <- reactive({ lm(y ~ x1 + x2, data = reg_data()) })

# coefficient table
output$reg_table <- renderTable({
  broom::tidy(reg_model()) |>
    mutate(term = recode(term, "(Intercept)" = "Intercept",
                         x1 = input$var_reg1, x2 = input$var_reg2)) |>
    select(Term = term, Estimate = estimate, `Std. Error` = std.error, `p-value` = p.value)
}, digits = 3)

# scatterplot: points are tracts (SWAP: neighborhoods), lines are the model's fit at low / median / high var 2
output$reg_scatter <- renderPlot({
  d <- reg_data()
  m <- reg_model()
  lvls    <- c("Low (25th percentile)", "Median", "High (75th percentile)")
  x2_vals <- unname(quantile(d$x2, c(0.25, 0.5, 0.75)))
  lines_df <- expand_grid(x1 = seq(min(d$x1), max(d$x1), length.out = 50),
                          level = lvls) |>
    mutate(x2 = x2_vals[match(level, lvls)])
  lines_df$fit   <- predict(m, newdata = lines_df)
  lines_df$level <- factor(lines_df$level, levels = lvls)
  
  ggplot() +
    geom_point(data = d, aes(x = x1, y = y), color = "#8856a7", size = 2.5, alpha = 0.8) +
    geom_line(data = lines_df, aes(x = x1, y = fit, color = level), linewidth = 1) +
    scale_color_brewer(palette = "Dark2") +
    labs(x = input$var_reg1, y = abs_var, color = paste0(input$var_reg2, ":")) +
    theme_minimal()
})

# residuals vs. fitted: actual minus predicted, against predicted
output$reg_resid <- renderPlot({
  m <- reg_model()
  ggplot(data.frame(fitted = fitted(m), resid = resid(m)), aes(x = fitted, y = resid)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point(color = "#8856a7", size = 2.5, alpha = 0.8) +
    labs(x = "Fitted values (predicted absenteeism rate)",
         y = "Residual (actual - predicted)") +
    theme_minimal()
})

# interpretation of the coefficient table (shown right under the dropdowns in the ui)
reg_interpretation <- reactive({
  b      <- coef(reg_model())[["x1"]]
  change <- b * reg_steps[[input$var_reg1]]
  paste0("For every ", reg_step_text[[input$var_reg1]], " in a tract's ",
         input$var_reg1, ", the absentee rate of schools in that tract is (hypothetically) predicted to ",
         if (change >= 0) "increase" else "decrease"," by ", round(abs(change), 2), 
         " percentage points, compared to tracts with the same level of ",   
         input$var_reg2, ".")
})
output$reg_interpretation <- renderText({ reg_interpretation() })
}

# this is what runs shiny app
shinyApp(ui = ui, server = server)