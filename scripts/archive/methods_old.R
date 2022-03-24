# Replicating methods for Gentrification assignment from Governing website

## Load Libraries
library(tidyverse)
library(tidycensus)
library(readxl)
library(foreign)
library(censusapi)

setwd("Documents/MIP/gentrification/")

## Redlining Data (Present Day CT)
redlining <-
  read.csv("data/All_Population_Weighted_HOLC_Grades_pt2_Threshold.csv")

## Outcomes (Present Day CT)
places <-
  read.csv("data/PLACES__Census_Tract_Data__GIS_Friendly_Format___2020_release.csv")

## Read in data from LTDB
filenames <- list.files("data/LTDB", pattern = "*.dbf", full.names = T)
ltdb <- lapply(filenames, read.dbf)


## Get data from ACS
acs <- get_acs(
  geography = "tract",
  output = "wide",
  year = 2019,
  state = state.abb,
  variables = c(
    `Total Population` = "B01001_001", #Total Pop
    `Bachelors Degree` = "B06009_006", #Bachelor's Degree
    `Median Income` = "B06011_001", #Median Income
    `Median Home Value` = "B25109_001" #Median Home Value
  )
)

years <- seq(1970, 2010, 10)

## Get data from Decennial (1970 - 2010)
pull_dec <- function(year){
  get_decennial(
  geography = "tract",
  output = "wide",
  year = year,
  state = state.abb,
  variables = c(
    `Total Population` = "B01001_001", #Total Pop
    `Bachelors Degree` = "B06009_006", #Bachelor's Degree
    `Median Income` = "B06011_001", #Median Income
    `Median Home Value` = "B25109_001" #Median Home Value
  )
)
}
pull_dec_sf <- function(year){
  get_decennial(
  geography = "tract",
  output = "wide",
  year = year,
  sumfile = "sf1",
  state = state.abb,
  variables = c(
    
    `Total Housing Units` = "H001001", #Housing Units
    
    # Rent vs. Own
    `Total Owned w/ Mortgage or Loan` = "H004002", # Tenure - Total!!Owned with a mortgage or a loan
    `Total Owned` = "H004003",  # Tenure - Total!!Owned free and clear
    `Total Renter Occupied` = "H004004",
    
    # Race/Ethnicity Householder
    `N Householder White Nonhispanic` = "H007003", # Total!!Not Hispanic or Latino householder!!Householder who is White alone	
    `N Householder Black` = "H006003", # Total!!Householder who is Black or African American alone	
    
    # Race/Ethnicity Population
    `Total Population` = "P001001", # Total Population
    `N Population Black` = "P003003", # Total!!Black or African American alone
    `N Population White Nonhispanic` = "P005002" #Total!!Not Hispanic or Latino!!White alone	
  )
)
}

## Gentrification Eligibility

bottom_40th <- tract_70 %>%
  select(STATE, COUNTY, HINC70, MHMVAL70) %>%
  group_by(STATE, COUNTY) %>%
  summarise_each(funs(quantile(., 0.4, na.rm = T)), HINC70, MHMVAL70)


# Append 40th percentile value to Tract 70 data
eligibile <- tract_70 %>%
  left_join(bottom_40th,
            by = c("STATE", "COUNTY"),
            suffix = c("", "_40th")) %>%
  mutate(
    pop_greater_500_70 = ifelse(POP70 > 500, T, F),
    hinc_less_40th = ifelse(HINC70 < HINC70_40th, T, F),
    mhm_less_40th = ifelse(MHMVAL70 < MHMVAL70_40th, T, F)
  ) %>%
  mutate(eligible = ifelse(pop_greater_500_70 &
                             hinc_less_40th &
                             mhm_less_40th, T, F)) %>%
  mutate(
    state = as.character(STATEA),
    county = as.character(COUNTYA),
    tract = as.character(TRACTA)
  ) %>%
  mutate(
    state = substr(state, 1, 2),
    county = substr(county, 1, 3),
    tract = str_pad(tract, 6, side = "right", pad = "0")
  ) %>%
  mutate(GEOID = paste0(state, county, tract)) %>%
  select(GISJOIN, GEOID, STATE, state, county, tract, MHMVAL70, 86:89)

## Identifies eligible tracts
eligibile %>%
  group_by(pop_greater_500_70, hinc_less_40th, mhm_less_40th, eligible) %>%
  count()


## Gentrification Determination
inflation <- 6.5891
redlining$GEOID <- as.character(redlining$GEOID)

determination <- acs %>%
  separate(NAME,
           into = c("Census Tract", "County", "State"),
           sep = ",") %>%
  mutate(`% with Bachelors` = 
           `Bachelors DegreeE` / `Total PopulationE`,
         `Adjusted Median Home Value (1970s)` = 
           `Median Home ValueE` / inflation) %>%
  right_join(redlining, by = "GEOID")

top_third <- determination %>% group_by(city) %>%
  summarise(
    top_third_bachelors = quantile(`% with Bachelors`, 1 / 3, na.rm = T),
    top_third_home_value = quantile(`Adjusted Median Home Value (1970s)`, 1 /
                                      3, na.rm = T)
  )


determination <- determination %>%
  left_join(top_third, by = "city") %>%
  mutate(
    bachelor_top_third = ifelse(`% with Bachelors` > top_third_bachelors, T, F),
    home_value_top_third = ifelse(`Adjusted Median Home Value (1970s)` > top_third_home_value, T, F)
  )



gentrified <-
  left_join(determination, eligibile, by = c("GEOID")) %>%
  mutate(determination = ifelse(bachelor_top_third &
                                  home_value_top_third, T, F)) %>%
  select(
    GEOID,
    County,
    State,
    `Median Home ValueE`,
    `Adjusted Median Home Value (1970s)`,
    `% with Bachelors`,
    `eligible`,
    `determination`,
    holc_grade_pop
  ) %>%
  mutate(gentrified = ifelse(eligible & determination, T, F))

gentrified %>% group_by(eligible, determination) %>%
  summarize(gentrified = n())

places$TractFIPS <- as.character(places$TractFIPS)
outcomes <-
  left_join(gentrified, places, by = c("GEOID" = "TractFIPS"))

saveRDS(outcomes, "outputs/outcomes.RDS")
