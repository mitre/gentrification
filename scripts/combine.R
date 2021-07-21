# Creating Dataset of Features for 2010 CTs
library(tidyverse)

# Read in Data -----

# Load LTDB Data
ltdb <- readRDS("outputs/ltdb_combined.RDS")

# Load ACS Data
acs_2012 <- readRDS("outputs/acs_2012.RDS")
acs_2019 <- readRDS("outputs/acs_2019.RDS")

## Load & Process PLACES -----
places <- read.csv("raw data/PLACES__Census_Tract_Data__GIS_Friendly_Format___2020_release.csv")

# Make sure that codes are correct form
places <- places %>%
  mutate(CountyFIPS = str_pad(CountyFIPS,width = 5, "left", "0"),
         TractFIPS = str_pad(TractFIPS, width = 11, "left", "0"))


prev_cols <- colnames(places[str_detect(colnames(places), "CrudePrev")])
prev_cols_names <- str_remove(prev_cols, pattern = "_CrudePrev")

ci_cols <- colnames(places)[str_detect(colnames(places), "95CI")] 

# Remove unnecessary columns
places <- places %>%
  select(-c(1:4, Geolocation))
places[colnames(places) %in% ci_cols] <- NULL

# Rename column names
names(places)[names(places) %in% prev_cols] <- prev_cols_names


## Load & Process Redlining Data -----
redlining <-
  read.csv("raw data/All_Population_Weighted_HOLC_Grades_pt2_Threshold.csv")
redlining$GEOID <- str_pad(redlining$GEOID, width = 11, side = "left", pad = "0")
redlining$GEOID <- as.character(redlining$GEOID)
redlining$state <- NULL
redlining$city <- NULL
redlining <- redlining %>% 
  group_by(GEOID) %>%
  summarize(holc_grade_pop = mean(holc_grade_pop, na.rm = T)) 


# Combine Data ------

final <- bind_rows(ltdb, acs_2012, acs_2019)
final$GEOID <- as.character(final$GEOID)

final <- final %>% left_join(redlining, by = "GEOID") %>%
  left_join(places, by = c("GEOID" = "TractFIPS"))


# Cleaning names
# tibble()
# "La Porte" = "Laporte"
# "La Porte" = "La Porte",
# "De Soto" = "Desoto",
# "De Soto" = "DeSoto",
# "Du Page" = "Dupage",
# "Du Page" = "DuPage"


saveRDS(final, "outputs/combined.RDS")


