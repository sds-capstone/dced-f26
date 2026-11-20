# DC 2024 general election turnout by census tract ------------------------
# Estimates registered voters and ballots cast in the November 2024 general
# election for each of DC's 206 census tracts.
#
# DC reports election results by voting precinct, and precinct boundaries
# don't line up with tract boundaries. Moving the counts onto tracts lets them
# be joined to ACS and other census data.
#
# Steps:
# 1. Download the 144 voting precinct boundaries from DC GIS.
# 2. Download tract boundaries and total population from the ACS.
# 3. Place each 2020 census block in a precinct, using a point inside the
#    block. Every block sits inside one tract, so this links each precinct to
#    the tracts it overlaps.
# 4. Add up block voting-age population (18 and over) by precinct and tract to
#    build a crosswalk that records what share of each precinct's adults live
#    in each tract.
# 5. Read the certified precinct results from the DC Board of Elections and
#    split each precinct's registered voters and ballots cast across tracts
#    using those shares.
#
# The method assumes registered voters are spread across a precinct the same
# way its adults are. Tract figures are estimates with decimals, and they add
# up to the citywide totals (464,502 registered, 328,871 ballots cast).
#
# Main outputs, also written to outputs/:
# - precinct_tract_xwalk: adult population shares for each precinct and tract
#   pair (dc-precinct-tract-crosswalk.csv)
# - tract_election_geo: tract geometry with population, overlapping
#   precincts, registered voters, ballots cast and turnout
#   (dc-turnout-by-tract-2024.gpkg)

# Setup -------------------------------------------------------------------

library(tidyverse)
library(tidycensus)
library(sf)
library(httr2)
library(conflicted)

conflicts_prefer(dplyr::filter)

# NAD83 / Maryland State Plane in meters, the CRS DC GIS uses
dc_crs <- 26985

# Voting precincts --------------------------------------------------------

precinct_req <- request(
  "https://maps2.dcgis.dc.gov/dcgis/rest/services/DCGIS_DATA/Administrative_Other_Boundaries_WebMercator/MapServer/0/query"
) |>
  req_url_query(
    where = "1=1",
    outFields = "NAME",
    outSR = dc_crs,
    f = "geojson"
  )

precinct_geo <- precinct_req |>
  req_perform() |>
  resp_body_string() |>
  read_sf() |>
  select(precinct = NAME)

# Census tracts -----------------------------------------------------------

# The 2025 ACS 5-year data comes out in December 2026; switch the year then
tract_pop <- get_acs(
  geography = "tract",
  variables = c(total_pop = "B01001_001"),
  state = "DC",
  year = 2024,
  survey = "acs5",
  geometry = TRUE,
  cb = FALSE
) |>
  select(GEOID, NAME, total_pop = estimate, total_pop_moe = moe) |>
  st_transform(dc_crs)

# Census blocks (population weights) --------------------------------------

# Only adults can register, so blocks are weighted by voting-age population
# from the 2020 redistricting file rather than by total population
dc_blocks <- get_decennial(
  geography = "block",
  variables = c(vap = "P3_001N"),
  state = "DC",
  year = 2020,
  sumfile = "pl",
  geometry = TRUE,
  output = "wide"
) |>
  select(block_geoid = GEOID, vap) |>
  st_transform(dc_crs)

# Blocks nest within tracts (the tract GEOID is the first 11 digits of the
# block GEOID), so blocks only need to be matched to precincts
block_precinct <- dc_blocks |>
  st_set_agr("constant") |>
  st_point_on_surface() |>
  st_join(precinct_geo, join = st_within) |>
  st_drop_geometry() |>
  mutate(tract_geoid = str_sub(block_geoid, 1, 11))

stopifnot(
  "Some blocks don't fall inside any precinct" = !anyNA(block_precinct$precinct)
)

# Precinct-to-tract crosswalk ---------------------------------------------

# precinct_share is the share of a precinct's adults who live in the tract.
# Multiplying precinct counts by it allocates them to tracts.
# tract_share is the share of a tract's adults who live in the precinct.
precinct_tract_xwalk <- block_precinct |>
  summarise(vap = sum(vap), .by = c(precinct, tract_geoid)) |>
  mutate(precinct_share = vap / sum(vap), .by = precinct) |>
  mutate(tract_share = vap / sum(vap), .by = tract_geoid) |>
  arrange(tract_geoid, desc(tract_share))

# Tract geometry with precinct membership ---------------------------------

tract_precincts <- precinct_tract_xwalk |>
  filter(vap > 0) |>
  summarise(
    n_precincts = n(),
    primary_precinct = precinct[which.max(tract_share)],
    primary_precinct_share = max(tract_share),
    precincts = str_flatten(precinct, ", "),
    .by = tract_geoid
  )

tract_precinct_geo <- tract_pop |>
  left_join(
    tract_precincts,
    by = join_by(GEOID == tract_geoid),
    relationship = "one-to-one"
  )

# Aggregating precinct data to tracts -------------------------------------

# `precinct_data` needs one row per precinct, a `precinct` column that matches
# precinct_geo$precinct ("Precinct 21") and the count columns named in `cols`.
# The allocated counts have decimals but add up to the precinct totals.
aggregate_precincts_to_tracts <- function(precinct_data, xwalk, cols) {
  xwalk |>
    inner_join(
      precinct_data,
      by = join_by(precinct),
      relationship = "many-to-one"
    ) |>
    summarise(
      across({{ cols }}, \(x) sum(x * precinct_share, na.rm = TRUE)),
      .by = tract_geoid
    )
}

# 2024 general election ---------------------------------------------------

election_url <- "https://electionresults.dcboe.org/Downloads/Reports/November_5_2024_General_Election_Certified_Results.csv"

# col_select errors if a column is missing, and stop_for_problems() errors if
# any value doesn't parse as an integer
election_results <- read_csv(
  election_url,
  col_select = c(ContestNumber, PrecinctNumber, WardNumber, Votes),
  col_types = cols(.default = col_integer())
)

stop_for_problems(election_results)

# Contest -2 is registered voters and contest -1 is ballots cast, with one row
# per precinct for each
precinct_turnout <- election_results |>
  filter(ContestNumber %in% c(-2, -1)) |>
  mutate(
    precinct = str_c("Precinct ", PrecinctNumber),
    measure = if_else(ContestNumber == -2, "registered_voters", "ballots_cast")
  ) |>
  pivot_wider(
    id_cols = c(precinct, WardNumber),
    names_from = measure,
    values_from = Votes
  )

stopifnot(
  "Precincts in the election file don't match the precinct map" = setequal(
    precinct_turnout$precinct,
    precinct_geo$precinct
  )
)

tract_turnout <- precinct_turnout |>
  aggregate_precincts_to_tracts(
    precinct_tract_xwalk,
    c(registered_voters, ballots_cast)
  )

# Tract 11001980000 is a special land-use tract (the National Mall area). The
# 2024 ACS puts its population at 17 (MOE 25), but the 2020 census counted 167
# adults in 10 of its blocks, most of them in Precinct 130. The crosswalk uses
# the 2020 counts, so the tract gets about 139 registered voters and 105
# ballots. Drop or flag it before comparing registration or turnout counts
# against total_pop.
tract_election_geo <- tract_precinct_geo |>
  left_join(
    tract_turnout,
    by = join_by(GEOID == tract_geoid),
    relationship = "one-to-one"
  ) |>
  mutate(turnout = ballots_cast / registered_voters)

# Save --------------------------------------------------------------------

write_csv(precinct_tract_xwalk, "data-files/dc-precinct-tract-crosswalk.csv")
write_sf(tract_election_geo, "data-files/dc-turnout-by-tract-2024.gpkg")
