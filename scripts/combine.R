# Creating Dataset of Features for 2010 CTs
library(dplyr)
library(tidyr)
library(stringr)

# Process Indicators Data --------

# Load LTDB Data
ltdb <- readRDS("outputs/ltdb_combined.RDS")
  
# Load ACS Data
acs_2012 <- readRDS("outputs/acs_2012.RDS")
acs_2020 <- readRDS("outputs/acs_2020.RDS")

# Load Place Names + GEOID
places_names <- readRDS("outputs/final_places_tracts.RDS")[c("GEOID", "STATEFP", "NAME")]

# Combine and Output
indicators <- bind_rows(ltdb, acs_2012, acs_2020) %>% 
  mutate(`Percent Above Poverty` = 100 - `Percent in Poverty, Total`,
         GEOID = as.character(GEOID)) %>%
  select(YEAR, STATE, GEOID, `Total Pop`, 
         `Median Household Income, Total`,
         `Percent Non-Hispanic White`, `Percent Above Poverty`,
         `Percent Owner-Occupied Units`, `Percent Vacant Units`,
         `Median Rent`, `Median Home Value`, 
         `Percent Structures more than 30 years old`, 
         `Percent with 4-year College Degree or More`,
         `Households in neighborhood 10 years or less`) %>%
  rename("Median Household Income" = "Median Household Income, Total") %>%
  left_join(places_names, by = c("GEOID"))


saveRDS(indicators, "outputs/indicators.RDS")

# Create long format for indicators data
indicators_long <- indicators %>%
  select(GEOID, YEAR, STATE, NAME, `Total Pop`,
         `Median Rent`, 
         `Median Household Income`,
         `Percent with 4-year College Degree or More`, 
         `Median Home Value`,
         `Percent Above Poverty`,
         `Percent Non-Hispanic White`,
         `Percent Structures more than 30 years old`,
         `Households in neighborhood 10 years or less`) %>%
  gather(key = "Measure", value = "Value", -YEAR, -STATE, -NAME, -GEOID, -`Total Pop`) 

saveRDS(indicators_long, "outputs/indicators_long.RDS")

# Process Outcomes Data --------

# Load USA Life Expectancy
leep <- read.csv("raw data/US_A.CSV", colClasses = c("Tract.ID" = "character"))[,c(1,5,6)] 
colnames(leep) <- c("TractID", "Life Expectancy", "Life Expectancy SE")

# Load & Process PLACES 
places <- read.csv("raw data/PLACES 500 Cities/PLACES__Census_Tract_Data__GIS_Friendly_Format___2020_release.csv") %>%
  mutate(CountyFIPS = str_pad(CountyFIPS,width = 5, "left", "0"),
         TractFIPS = str_pad(TractFIPS, width = 11, "left", "0"))%>%
  unique()

prev_cols <- colnames(places[str_detect(colnames(places), "CrudePrev")])
prev_cols_names <- str_remove(prev_cols, pattern = "_CrudePrev")

ci_cols <- colnames(places)[str_detect(colnames(places), "95CI")] 

places <- places %>%
  select(-c(1:4, Geolocation))
places[colnames(places) %in% ci_cols] <- NULL

names(places)[names(places) %in% prev_cols] <- prev_cols_names


# Load & Process Redlining Data 
redlining <-
  read.csv("raw data/All_Population_Weighted_HOLC_Grades_pt2_Threshold.csv") %>%
  mutate(GEOID = str_pad(GEOID, width = 11, side = "left", pad = "0"),
         GEOID = as.character(GEOID)) %>%
  select(-state, -city) %>%
  group_by(GEOID) %>%
  summarize(`HOLC Grade` = mean(holc_grade_pop, na.rm = T))


# Combine and Output

outcomes <- redlining %>%
  full_join(leep, by = c("GEOID" = "TractID")) %>%
  left_join(places, by = c("GEOID" = "TractFIPS"))

saveRDS(outcomes, "outputs/outcomes.RDS")


