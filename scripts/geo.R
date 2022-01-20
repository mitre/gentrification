library(tigris)
library(sp)

if (!file.exists("outputs/geo_places.RDS")) {
  places <- places(state = state.abb, cb = TRUE)
  saveRDS(places, "outputs/geo_places.RDS")
}

if (!file.exists("outputs/geo_tracts.RDS")) {
  tracts <- NULL
  for(state in state.abb){
    state_tract <- tracts(state = state, cb = TRUE)
    tracts <- rbind(tracts, state_tract)
  }
  saveRDS(tracts, "outputs/geo_tracts.RDS")
}

places <- readRDS("outputs/geo_places.RDS")
tracts <- readRDS("outputs/geo_tracts.RDS")

