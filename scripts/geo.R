# Assign tracts to cities and places, create combined places geographies for cities

library(tigris)
library(sp)
library(sf)
library(ggplot2)
library(dplyr)
library(tidycensus)
library(gridExtra)


### geo_places.RDS
# Downloads from tigris all the census place shapefiles if file doesn't already exist in outputs folder
if (!file.exists("outputs/geo_places.RDS")) {
  # Pulls place data from tigris
  places <- places(state = state.abb, cb = TRUE)
  
  # Saves to outputs
  saveRDS(places, "outputs/geo_places.RDS")
}


### geo_tracts.RDS
# Downloads from tigris all the tract boundaries if file doesn't already exist in outputs folder
if (!file.exists("outputs/geo_tracts.RDS")) {
  # Initializes tracts
  tracts <- NULL
  
  for(state in state.abb){
    # Pulls tract data for a state
    state_tract <- tracts(state = state, cb = TRUE)
    
    # Adds tract data to other states of data
    tracts <- rbind(tracts, state_tract)
  }
  
  # Saves to outputs
  saveRDS(tracts, "outputs/geo_tracts.RDS")
}


# Goal: Return 2 .RDS files:
# File 1 (holc_geo.RDS): Maintains the original holc file structure, lists the name of the place next to each tract
# File 2 (holc_place.RDS): Each row is a city + state in the holc file, with the combined places geometry of intersected places. Should be 202 rows.
# Include places that have pop greater than 50k, don't need to have redlined. Structure is state, place, geoid for tract. 
# Include holc graded tracts that arent within places. New and old intersection, full join.
# PURPOSE: Granularity of state > city > place > tract


places <- readRDS("outputs/geo_places.RDS")
tracts <- readRDS("outputs/geo_tracts.RDS")
holc <- read.csv("raw data/All_Population_Weighted_HOLC_Grades_pt2_Threshold.csv", colClasses = c("GEOID" = "character"))


### AVOID RUNNING, TAKES ~6hrs
### final_places_tracts.RDS
# Creates dataframe with a tract's GEOID, state, place, and geometry.
# N.B. tracts can be split by the place borders in which they are contained.
if (!file.exists("outputs/final_places_tracts.RDS")) {
  
  run_code <- readline("This code will take ~6 hours to run. Do you want to run it? [y/n]")
  
  if (run_code != "y") {
    stop("Code execution halted.")
  }
  # Creates table with abbreviated state and corresponding GEOID
  state_lookup <- states() %>%
    filter(STUSPS %in% state.abb) %>%
    st_drop_geometry() %>%
    pull(STATEFP, STUSPS)
  
  places_tracts <- NULL
  for (state_name in state.abb){
    print(state_name)
  
    # Pulls GEOID for state
    state <- state_lookup[[state_name]]
  
    # Pulls places within state
    state_place <- places %>%
      filter(STATEFP == state) %>%
      select(STATEFP, NAME, geometry)
  
    # Pulls tracts within state
    state_tract <- tracts %>%
      filter(STATEFP == state) %>%
      select(GEOID, geometry)
  
    # Pulls the geometries for all tracts cropped by places
    state_place_tract <- st_intersection(state_tract, state_place)
  
    # Adds state data to data for other states
    places_tracts <- rbind(places_tracts, state_place_tract)
  }
  
  # Drops segmented geometry and adds tract geometry
  final_places_tracts <- places_tracts %>%
    st_drop_geometry() %>%
    left_join(tracts %>% select(GEOID, geometry), by = "GEOID")
  
  # Saves to outputs
  saveRDS(final_places_tracts, "outputs/final_places_tracts.RDS")
}


### all_acs_pops.RDS
# Creates dataframe with a tract's GEOID and population estimate for 2019
if (!file.exists("outputs/all_acs_pops.RDS")) {
  # Pulls tracts grouped by place
  final_places_tracts <- readRDS("outputs/final_places_tracts.RDS")
  
  # Pulls unique states for iteration
  place_statefp <- unique(final_places_tracts %>%
                            pull(STATEFP))
  
  all_acs_pops <- NULL
  for (i in 1:length(place_statefp)){
    # Pulls state
    state <- place_statefp[i]
    
    # Pulls population data for tracts in 2019 for state with GEOID and population
    acs_populations <- tidycensus::get_acs(geography = "tract", variables = "B01003_001", year = 2019, state = state, geometry = FALSE) %>%
      select(GEOID, estimate)
    
    # Adds data to dataframe for other states
    all_acs_pops <- rbind(all_acs_pops, acs_populations)
  }
  
  # Saves to outputs
  saveRDS(all_acs_pops, "outputs/all_acs_pops.RDS")
}


# Creates lookup table for states with GEOID and 2 letter abbreviation
state_lookup <- states() %>%
  filter(STUSPS %in% state.abb) %>%
  st_drop_geometry() %>%
  select(STATEFP, STUSPS)


### holc_geo.RDS
# Creates dataframe of tracts with their holc grade, assigned city, and assigned place
if (!file.exists("outputs/holc_geo.RDS")) {
  # Pulls tracts grouped by place
  places_tracts <- readRDS("outputs/final_places_tracts.RDS")
  
  # Tracts in a place with abbreviation instead of GEOID
  places_tracts_table <- places_tracts %>%
    left_join(state_lookup, by = "STATEFP") %>%
    select(-c(STATEFP)) %>%
    rename(PLACE = NAME, STUSPS.PLACE = STUSPS)
  
  # Table containing tract GEOID, holc grade, city, place
  holc_geo <- holc %>%
    rename(CITY = city, STUSPS.CITY = state) %>%
    left_join(places_tracts_table, by = "GEOID") %>%
    select(-c(geometry))
  
  # Saves to outputs
  saveRDS(holc_geo, "outputs/holc_geo.RDS")
}


### holc_place.RDS
# Each row is a city + state in the holc file, with the combined places geometry of intersected places.
if (!file.exists("outputs/holc_place.RDS")) {
  # Pulls tract data with city, place, and holc grade
  holc_geo <- readRDS("outputs/holc_geo.RDS")
  
  # Pulls cities and states; 202 total
  holc_cities <- holc_geo %>%
    select(CITY, STUSPS.CITY) %>%
    distinct()
  
  # Pulls a list of cities, with the places in each city
  city_places <- holc_geo %>%
    filter(!is.na(holc_grade_pop)) %>%
    select(CITY, STUSPS.CITY, PLACE, STUSPS.PLACE) %>%
    distinct()
  
  # Creates sf with union of places in a city
  # N.B. Places included must contain at least 1 holc-graded tract
  holc_place <- NULL
  
  # Adds state abbreviation to list of places
  places <- places %>%
    left_join(state_lookup, by = "STATEFP")
  
  # Iterate over all cities
  for (i in 1:nrow(holc_cities)) {
    # Pulls city and state
    holc_city <- holc_cities[i, 1]
    holc_state <- holc_cities[i, 2]
  
    # Pulls the places in the given city
    holc_places <- city_places %>%
      filter(CITY == holc_city & STUSPS.CITY == holc_state)
  
    # Pulls geometry for places in the city
    places_geo <- holc_places %>%
      left_join(places, by = c("PLACE" = "NAME", "STUSPS.PLACE" = "STUSPS")) %>%
      select(geometry) %>%
      st_as_sf()
  
    # Creates a union of the places geometries
    city_place_geo <- st_union(places_geo)
  
    # Creates a row for the city with the unified geometry
    temp_df <- data.frame(holc_city, holc_state, city_place_geo)
    colnames(temp_df) <- c("CITY", "STUSPS.CITY", "geometry")
  
    # Adds row for specific city as sf
    holc_place = rbind(holc_place, st_as_sf(temp_df))
  }
  
  # Saves to outputs
  saveRDS(holc_place, "outputs/holc_place.RDS")
}


### places_pop_gt_50k.RDS
# places with populations of greater than 50k
if (!file.exists("outputs/places_pop_gt_50k.RDS")) {
  # Pulls tracts grouped by place
  places_tracts <- readRDS("outputs/final_places_tracts.RDS")

  # Pulls states
  place_statefp <- unique(places_tracts %>%
                           pull(STATEFP))
  
  population_table <- NULL
  for (i in 1:length(place_statefp)) {
    # Pulls state
    state <- place_statefp[i]
    
    # Pulls population data for tracts in 2019 for state with GEOID and population
    acs_populations <- tidycensus::get_acs(geography = "tract", variables = "B01003_001", year = 2019, state = state, geometry = FALSE) %>%
      select(GEOID, estimate)
    
    # Creates dataframe with place, state, and total population estimate of place
    place_population <- places_tracts %>% 
      st_as_sf() %>%
      st_drop_geometry() %>%
      filter(STATEFP == state) %>%
      left_join(acs_populations, by = "GEOID") %>%
      group_by(STATEFP, NAME) %>%
      summarise(estimate = sum(estimate))
    
    # Coerces data to dataframe with column names
    temp_df <- data.frame(place_population)
    colnames(temp_df) <- c("STATEFP.PLACE", "PLACE", "POPULATION")
    
    # Adds data to total dataframe
    population_table <- rbind(population_table, temp_df)
  }
  
  # Filters to places with a population greater than 50k, adds geometry
  places_pop_gt_50k <- population_table %>%
    filter(POPULATION >= 50000) %>%
    left_join(places, by = c("STATEFP.PLACE" = "STATEFP", "PLACE" = "NAME"))
  
  # Saves to outputs
  saveRDS(places_pop_gt_50k, "outputs/places_pop_gt_50k.RDS")
}
