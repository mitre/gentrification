library(tigris)
library(sp)

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



places <- readRDS("outputs/geo_places.RDS") 
tracts <- readRDS("outputs/geo_tracts.RDS")
holc <- read.csv("raw data/All_Population_Weighted_HOLC_Grades_pt2_Threshold.csv")



# Another task would be create an .Rmd file in the notebooks folder with an descriptive stats on this data.
# Specifically, I'd be interested in seeing how many places intersect with the original holc boundaries. 
# Also interested in seeing how much bigger these combined geographies are compared to the original holc boundaries.