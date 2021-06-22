# Processing LTDB to clean format

## Identifies common columns between 1970 - 2000, need to use ACS for 2010 estimates
## https://s4.ad.brown.edu/projects/diversity/researcher/List%20of%20Available%20Variables.pdf
## https://s4.ad.brown.edu/projects/diversity/Researcher/LTBDDload/Dfiles/codebooks.pdf

library(foreign)
library(tidyverse)

if (getwd() != "/Users/kjiang/Documents/MIP/gentrification") {
  setwd("/Users/kjiang/Documents/MIP/gentrification")
}

# Read in Tract Files from 1970 - 2010
filenames <- list.files("raw data/LTDB", pattern = "*.dbf", full.names = T)
ltdb <- lapply(filenames, read.dbf)

# Remove last two year numbers from column names
for (i in 1:5) {
  colnames(ltdb[[i]]) <- gsub('[0-9]{2}$',"",colnames(ltdb[[i]]))
  # Making sure all years values are present
  ltdb[[i]]$YEAR <- 1960 + 10*i
  if (i %in% 4:5) {
    ltdb[[i]]$TRACTID  <- as.character(ltdb[[i]]$TRACTID)
  }
}

# Combine into long format
ltdb_combined <- bind_rows(ltdb)

# Rename columns for selected data
cname_map <- tibble(
  "POP" = "Total Pop",
  
  "HINC" = "Median Household Income, Total",
  "HINCW" = "Median Household Income, White",
  "HINCB" = "Median Household Income, Black",
  "HINCH" = "Median Household Income, Hispanic",
  "HINCA" = "Median Household Income, Asian/PI",
  
  "INCPC" = "Per Capita Income",
  "PPOV" = "Percent in Poverty, Total",
  "P65POV" = "Percent in Poverty, 65+",
  "PBPOV" = "Percent in Poverty, Black",
  "PWPOV" = "Percent in Poverty, White",
  "PNAPOV" = "Percent in Poverty, Native American",
  "PHPOV" = "Percent in Poverty, Hispanic",
  "PAPOV" = "Percent in Poverty, Asian/PI",
  
  "POWN" = "Percent Owner-Occupied Units",
  "PVAC" = "Percent Vacant Units",
  "PMULTI" = "Percent Multi-Family Units",
  
  "MRENT" = "Median Rent",
  "MHMVAL" = "Median Home Value",
  
  "PHS" = "Percent with High School Degree or Less",
  "PCOL" = "Percent with 4-year College Degree or More",
  "PUNEMP" = "Percent Unemployed"
) %>%
  pivot_longer(names_to = "original", values_to =  "readable", cols = everything())

# Make names pretty and remove -999 missing values
ltdb_combined <- ltdb_combined %>%
  rename_at(vars(cname_map$original), ~ cname_map$readable) %>%
  na_if("-999") 

# Get values to fill in missing State and County names
clean_ids <- function(data){

  data %>%
  mutate_at(c("STATEA", "COUNTYA", "TRACTA"), as.character) %>%
  filter_at(c("STATEA", "COUNTYA", "TRACTA"), is.na) %>%
  mutate(STATEA = substr(STATEA, 1,2),
          COUNTYA = substr(COUNTYA, 1,3),
          TRACTA = str_pad(TRACTA, width = 6, side = "left", pad = "0"),
          GEOID = paste0(STATEA, COUNTYA, TRACTA))
}

state_names <- ltdb_combined %>%
  clean_ids() %>%
  select(3:4) %>%
  na.omit() %>%
  unique()

county_names <- ltdb_combined %>% 
  clean_ids() %>%
  select(4:6) %>%
  na.omit() %>%
  unique()

# Recreate GEOID     
ltdb_combined <- ltdb_combined %>%
  clean_ids() %>%
  left_join(state_names, by = "STATEA", suffix = c("_old", "")) %>%
  left_join(county_names, by = c("STATEA", "COUNTYA"), suffix = c("_old", "")) %>%
  select(YEAR, STATE, COUNTY, GEOID, cname_map$readable)
 

# Write Out data  
write.csv(ltdb_combined, "outputs/ltdb_combined.csv" )
write_rds(ltdb_combined, "outputs/ltdb_combined.RDS")
