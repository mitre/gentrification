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


