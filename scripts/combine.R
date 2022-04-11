# Creating Dataset of Features for 2010 CTs
library(tidyverse)


fix_county_names <- function(data){
  data %>%
    mutate(COUNTY = ifelse(COUNTY %in% c("Du Page", "Dupage", "DuPage"), yes = "DuPage", no = COUNTY),
           COUNTY = ifelse(COUNTY %in% c("De Soto", "Desoto", "DeSoto"), yes = "DeSoto", no = COUNTY ),
           COUNTY = ifelse(COUNTY %in% c("La Porte", "Laporte", "LaPorte"),yes = "La Porte", no = COUNTY),
           COUNTY = ifelse(COUNTY %in% c("La Porte", "Laporte", "LaPorte"),yes = "La Porte", no = COUNTY),
           COUNTY = str_remove(COUNTY, " Borough"),
           COUNTY = str_remove(COUNTY, " Census Area"),
           COUNTY = str_remove(COUNTY, " Municipality"),
           COUNTY = str_replace(COUNTY, "St ", "St. "),
           COUNTY = str_replace(COUNTY, "Prince Georges", "Prince George's"),
           COUNTY = str_to_title(COUNTY)) %>%
    unique()
}

# Read in Data -----

# Load LTDB Data
ltdb <- readRDS("outputs/ltdb_combined.RDS") %>% fix_county_names()
  
# Load ACS Data
acs_2012 <- readRDS("outputs/acs_2012.RDS") %>% fix_county_names()
acs_2020 <- readRDS("outputs/acs_2020.RDS") %>% fix_county_names()

# Load USA Life Expectancy
leep <- read.csv("raw data/US_A.CSV")
leep <- leep[,c(1,5,6)]
colnames(leep) <- c("TractID", "Life Expectancy", "SE Life Expectancy")
leep$TractID <- as.character(leep$TractID)

## Load & Process PLACES -----
places <- read.csv("raw data/PLACES 500 Cities/PLACES__Census_Tract_Data__GIS_Friendly_Format___2020_release.csv")

# Make sure that codes are correct form
places <- places %>%
  mutate(CountyFIPS = str_pad(CountyFIPS,width = 5, "left", "0"),
         TractFIPS = str_pad(TractFIPS, width = 11, "left", "0"))%>%
  unique()


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

final <- bind_rows(ltdb, acs_2012, acs_2020)
final$GEOID <- as.character(final$GEOID)

final <- final %>% left_join(redlining, by = "GEOID") %>%
  left_join(places, by = c("GEOID" = "TractFIPS")) %>%
  left_join(leep, by = c("GEOID" = "TractID"))



saveRDS(final, "outputs/combined.RDS")


