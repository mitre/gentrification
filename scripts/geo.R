library(tigris)
library(sp)
library(sf)
library(ggplot2)
library(dplyr)
library(tidycensus)
library(gridExtra)

census_api_key("ff942edcde192676c9bf58546ca77f7e97b58a46")

# Downloads from tigris all the census place shapefiles if file doesn't already exist in outputs folder
if (!file.exists("outputs/geo_places.RDS")) {
  places <- places(state = state.abb, cb = TRUE)
  saveRDS(places, "outputs/geo_places.RDS")
}

# Downloads from tigris all the tract boundaries if file doesn't already exist in outputs folder
if (!file.exists("outputs/geo_tracts.RDS")) {
  tracts <- NULL
  for(state in state.abb){
    state_tract <- tracts(state = state, cb = TRUE)
    tracts <- rbind(tracts, state_tract)
  }
  saveRDS(tracts, "outputs/geo_tracts.RDS")
}

## TO DO For AJ ----

# Goal: Return 2 .RDS files:
# File 1 (holc_geo.RDS): Maintains the original holc file structure, lists the name of the place next to each tract
# File 2 (holc_place.RDS): Each row is a city + state in the holc file, with the combined places geometry of intersected places. Should be 202 rows.
# Include places that have pop greater than 50k, don't need to have redlined. Structure is state, place, geoid for tract. 
# Include holc graded tracts that arent within places. New and old intersection, full join.
# PURPOSE: Granularity of state > city > place > tract


places <- readRDS("outputs/geo_places.RDS")
tracts <- readRDS("outputs/geo_tracts.RDS")
holc <- read.csv("raw data/All_Population_Weighted_HOLC_Grades_pt2_Threshold.csv", colClasses = c("GEOID" = "character"))


## Working with RI for short computation times

RI_tracts <- tracts %>%
  filter(STATEFP == "44")

RI_places <- places %>%
  filter(STATEFP == "44")

#plot of RI places over tracts

RI_tract_plot = ggplot() +
  geom_sf(data = RI_tracts) +
  geom_sf(data = RI_places, fill = "blue")

#intersect plots
RI_places_tracts <- st_intersection(RI_tracts, RI_places)

test_plot = ggplot() +
  geom_sf(data = RI_places_tracts)



## Overall for both

# Creates table with abbreviated state and corresponding GEOID
state_lookup <- states() %>%
  filter(STUSPS %in% state.abb) %>%
  st_drop_geometry() %>%
  pull(STATEFP, STUSPS)


### AVOID RUNNING, TAKES ~6hrs
# Creates dataframe with a tract's GEOID, state, place, and geometry.
# N.B. tracts can be split by the place borders in which they are contained.

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

  places_tracts <- rbind(places_tracts, state_place_tract)
}

saveRDS(places_tracts, "outputs/geo_tracts_places.RDS")


final_places_tracts <- places_tracts %>%
  st_drop_geometry() %>%
  left_join(tracts %>% select(GEOID, geometry), by = "GEOID")

saveRDS(final_places_tracts, "outputs/final_places_tracts.RDS")

place_statefp <- unique(final_places_tracts %>%
                          pull(STATEFP))

all_acs_pops <- NULL

for (i in 1:length(place_statefp)){
  # Pulls city and state
  state <- place_statefp[i]
  
  acs_populations <- tidycensus::get_acs(geography = "tract", variables = "B01003_001", year = 2019, state = state, geometry = FALSE) %>%
    select(GEOID, estimate)
  
  # places_tracts <- places_tracts %>%
  #   left_join(acs_populations, by = "GEOID")
  
  all_acs_pops <- rbind(all_acs_pops, acs_populations)
}

saveRDS(all_acs_pops, "outputs/all_acs_pops.RDS")


# final_places_tracts <- final_places_tracts %>%
#   left_join(all_acs_pops, by = "GEOID")
# 
# saveRDS(final_places_tracts, "outputs/final_places_tracts.RDS")


## INSTEAD OF INTERSECTION, JUST MAKE SURE THE TRACTS ARE CONTAINED THEN FILTER
# places_intersects <- NULL
# for (state_name in state.abb){
# # for (state_name in c("RI")){
#   print(state_name)
#   
#   # Pulls GEOID for state
#   state <- state_lookup[[state_name]]
#   
#   # Pulls places within state
#   state_place <- places %>%
#     filter(STATEFP == state) %>%
#     select(STATEFP, NAME, geometry)
#   
#   # Pulls tracts within state
#   state_tract <- tracts %>%
#     filter(STATEFP == state) %>%
#     select(GEOID, geometry)
#   
#   # Pulls the geometries for all tracts cropped by places
#   # state_place_tract <- state_tract %>%
#   #   filter(rowSums(st_intersects(., state_place, sparse = FALSE)) > 0L)
#   state_place_tract <- state_tract %>%
#     st_join(., state_place) %>%
#     filter(!is.na(NAME))
#   
#   places_intersects <- rbind(places_intersects, state_place_tract)
# }
# 
# saveRDS(places_intersects, "outputs/geo_tracts_places_intersection.RDS")


# holc_geo

state_lookup <- states() %>%
  filter(STUSPS %in% state.abb) %>%
  st_drop_geometry() %>%
  select(STATEFP, STUSPS)

places_tracts <- readRDS("outputs/geo_tracts_places.RDS")

# places_intersects <- readRDS("outputs/geo_tracts_places_intersection.RDS")

# Tracts in a place with abbreviation instead of GEOID
places_tracts_table <- places_tracts %>%
  left_join(state_lookup, by = "STATEFP") %>%
  select(-c(STATEFP)) %>%
  rename(PLACE = NAME, STUSPS.PLACE = STUSPS)

# Creates a table with a tracts GEOID and total area (before being split by place borders)
tract_areas_lookup <- tracts %>%
  filter(GEOID %in% holc$GEOID) %>%
  mutate(total_area = as.numeric(st_area(geometry))) %>%
  st_drop_geometry() %>%
  select(total_area, GEOID)

# Table containing tract GEOID, holc grade, city, place, and proportion of the total tract area the tract in a place takes up
holc_geo <- holc %>%
  rename(CITY = city, STUSPS.CITY = state) %>%
  left_join(places_tracts_table, by = "GEOID") %>%
  left_join(tract_areas_lookup, by = "GEOID") %>%
  mutate(tract_place_area = as.numeric(st_area(geometry))) %>%
  mutate(area_proportion = round(tract_place_area / total_area, digits = 3)) %>%
  select(-c(geometry, total_area, tract_place_area))

saveRDS(holc_geo, "outputs/holc_geo.RDS")

# # NOW WITH FILTERED RATHER THAN INTERSECTED
# 
# # Tracts in a place with abbreviation instead of GEOID
# places_intersects_table <- places_intersects %>%
#   left_join(state_lookup, by = "STATEFP") %>%
#   select(-c(STATEFP)) %>%
#   rename(PLACE = NAME, STUSPS.PLACE = STUSPS)
# 
# # Creates a table with a tracts GEOID and total area (before being split by place borders)
# tract_areas_lookup <- tracts %>%
#   filter(GEOID %in% holc$GEOID) %>%
#   mutate(total_area = as.numeric(st_area(geometry))) %>%
#   st_drop_geometry() %>%
#   select(total_area, GEOID)
# 
# # Table containing tract GEOID, holc grade, city, place, and proportion of the total tract area the tract in a place takes up
# holc_geo <- holc %>%
#   rename(CITY = city, STUSPS.CITY = state) %>%
#   left_join(places_intersects_table, by = "GEOID") %>%
#   left_join(tract_areas_lookup, by = "GEOID") %>%
#   mutate(tract_place_area = as.numeric(st_area(geometry))) %>%
#   mutate(area_proportion = round(tract_place_area / total_area, digits = 3)) %>%
#   select(-c(geometry, total_area, tract_place_area))
# 
# saveRDS(holc_geo, "outputs/holc_geo.RDS")



# holc_place

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

  holc_place = rbind(holc_place, st_as_sf(temp_df))
}

saveRDS(holc_place, "outputs/holc_place.RDS")



# PULLING POPULATION DATA FOR PLACES

place_statefp <- unique(places_tracts %>%
                         pull(STATEFP))

population_table <- NULL

for (i in 1:length(place_statefp)) {
  # Pulls city and state
  state <- place_statefp[i]
  
  acs_populations <- tidycensus::get_acs(geography = "tract", variables = "B01003_001", year = 2019, state = state, geometry = FALSE) %>%
    select(GEOID, estimate)
  
  place_population <- places_tracts %>% 
    st_drop_geometry() %>%
    filter(STATEFP == state) %>%
    left_join(acs_populations, by = "GEOID") %>%
    group_by(STATEFP, NAME) %>%
    summarise(estimate = sum(estimate))
  
  temp_df <- data.frame(place_population)
  colnames(temp_df) <- c("STATEFP.PLACE", "PLACE", "POPULATION")
  
  population_table <- rbind(population_table, temp_df)
}

saveRDS(population_table, "outputs/population_table.RDS")

places_pop_gt_50k <- population_table %>%
  filter(POPULATION >= 50000) %>%
  left_join(places, by = c("STATEFP.PLACE" = "STATEFP", "PLACE" = "NAME"))

saveRDS(places_pop_gt_50k, "outputs/places_pop_gt_50k")

all_acs_pops <- NULL

for (i in 1:length(place_statefp)){
  # Pulls city and state
  state <- place_statefp[i]
  
  acs_populations <- tidycensus::get_acs(geography = "tract", variables = "B01003_001", year = 2019, state = state, geometry = FALSE) %>%
    select(GEOID, estimate)
  
  # places_tracts <- places_tracts %>%
  #   left_join(acs_populations, by = "GEOID")
  
  all_acs_pops <- rbind(all_acs_pops, acs_populations)
}

places_tracts <- places_tracts %>%
  left_join(all_acs_pops, by = "GEOID")

# Another task would be create an .Rmd file in the notebooks folder with an descriptive stats on this data.
# Specifically, I'd be interested in seeing how many places intersect with the original holc boundaries.
# Also interested in seeing how much bigger these combined geographies are compared to the original holc boundaries.
