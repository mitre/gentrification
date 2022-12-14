# Pull data from American Community Survey (ACS) to parallel data available in LTDB

# Author: Karen Jiang
# Version: 2022-11-16

# Packages
library(tidyverse)
library(tidycensus)
# census_api_key("YOUR_KEY", install = T)

# Parameters
# setwd("path/to/cloned/repo/gentrification")
#===============================================================
# Code

# Get present day ACS data -------

get_acs_vars <- function(year){
  get_acs(
    geography = "tract",
    state = state.abb,
    #cache_table = T,
    survey = "acs5",
    output = "wide",
    year = year,
    variables = c(
      # Vars from tables listed codebook
      ## https://s4.ad.brown.edu/projects/diversity/Researcher/LTBDDload/Dfiles/codebooks.pdf
      
      `Total Pop` = "B01001_001",
      `Non-Hispanic White` = "B01001A_001",
      `Non-Hispanic Black` = "B01001H_001",
      `Median Household Income, Total`  = "B19013_001",
      `Median Household Income, White` = "B19013A_001",
      `Median Household Income, Black` = "B19013B_001",
      `Median Household Income, Hispanic` = "B19013I_001",
      `Median Household Income, Asian` = "B19013D_001",
      `Median Household Income, PI` = "B19013E_001",
      `Median Rent` = "B25058_001",
      `Median Home Value` = "B25077_001",
      
      dpov = "B17001_001", # Denominator: Total 
      npov = "B17001_002", # Numeration: number in poverty

      nbpov = "B17020B_002", # Numerator: Total - Black
      dbpov = "B17020B_001", # Denominator: number in poverty - Black
      
      nwpov = "B17020H_002", # Numerator: Total - White, non Hispanic
      dwpov = "B17020H_001", # Denominator: number in poverty - White, non Hispanic
      
      nhpov = "B17020I_002", # Numerator: Total - Hispanic
      dhpov = "B17020I_001", # Denominator: number in poverty - Hispanic
      
      hu = "B25002_001", # Estimate!! Total: OCCUPANCY STATUS
      ohu = "B25003_001", # Estimate!! Total: TENURE
      own = "B25003_002", # Owner Occupied
      rent = "B25003_003", # Renter Occupied
      vac = "B25002_003", # Vacant
      
      hs = "B15003_017", # high school or less
      bachelors = "B15003_022", # bachelors degree
      masters = "B15003_023", # masters degree
      professional = "B15003_024", # professional degree
      doctorate = "B15003_025", # doctorate degree
      ag25up = "B15003_001", # denominator for education, population 25+

      built_tot = "B25034_001",  # year structure built
      built_within5 = "B25034_002", # built within past 5 years
      built_5to10 = "B25034_003", # built within past 5-10 years
      built_10to20 = "B25034_004", # built within past 10-20 years
      built_20to30 = "B25034_005", # built within past 20-30 years


      moved_tot = "B25038_001", # tenure by year householder moved into unit
      moved_o_1 = "B25038_003", # Owner occupied:!!Moved in 2017 or later
      moved_o_2 = "B25038_004", # Owner occupied:!!Moved in 2015 to 2016
      moved_o_3 = "B25038_005", # Owner occupied:!!Moved in 2010 to 2014 used for 2020 only
      
      moved_r_1 = "B25038_010", # Renter occupied:!!Moved in 2017 or later
      moved_r_2 = "B25038_011", # Renter occupied:!!Moved in 2015 to 2016
      moved_r_3 = "B25038_012", # Renter occupied:!!Moved in 2010 to 2014 used for 2020 only
      
      total_employ = "C18120_001", # Denominator: Total 
      laborforce = "C18120_002", # Number in laborforce
      laborforce_employ = "C18120_003", # Number in laborforce employed
      laborforce_unemploy = "C18120_006" # Number in laborforce unemployed
      
    )
  )
}

# acs_2010 <- get_acs_vars(2010) # this one doesn't work?

if (!file.exists("outputs/acs_2012_raw.RDS")) {
  acs_2012 <- get_acs_vars(2012)
  saveRDS(acs_2012, "outputs/acs_2012_raw.RDS")
}
acs_2012 <- readRDS("outputs/acs_2012_raw.RDS")


if (!file.exists("outputs/acs_2020_raw.RDS")) {
  acs_2020 <- get_acs_vars(2020)
  saveRDS(acs_2020, "outputs/acs_2020_raw.RDS")
}
acs_2020 <- readRDS("outputs/acs_2020_raw.RDS")

# Calculate Measures -----

process_vars <- function(df,year) {
  
  # Removing the E-suffix from the Estimates to rename columns
  rename_cols <- c(names(df)[1:2],
                   sub("E", "", names(df)[-c(1:2)]))
  colnames(df) <- rename_cols
  
  
  df %>%
    
    # Separating ACS name into CT, County, and State Name
    separate(NAME,
             into = c("Census Tract", "COUNTY", "STATE"),
             sep = ", ") %>%
    mutate(COUNTY = str_replace(COUNTY, " County", "")) %>%
    
    # Removing all MOE values, since LTDB doesn't have them
    select(-ends_with("M", ignore.case = F)) %>%
    
    
    # Measure Calculations based as much as possible on notes provided in codebook
    mutate(
      `Per Capita Income` = `Median Household Income, Total` / `Total Pop`,
      `Percent in Poverty, Total` = npov / dpov * 100,
      `Percent in Poverty, Black` = nbpov / dbpov * 100,
      `Percent in Poverty, White` = nwpov / dwpov * 100,
      `Percent in Poverty, Hispanic` = nhpov / dhpov * 100,
      
      `Percent Owner-Occupied Units` = own / ohu * 100,

      `Percent Vacant Units` = vac / hu * 100,
      
      `Percent with High School Degree or Less` = hs / ag25up * 100,
      `Percent with 4-year College Degree or More` = (bachelors + masters + professional + doctorate) / ag25up * 100,
      `Percent Unemployed` = laborforce_unemploy / laborforce * 100,
      `Percent Labor Force Participation` = laborforce / total_employ * 100,
      
      `Median Household Income, Asian/PI` = `Median Household Income, Asian` + `Median Household Income, PI`,
      
      `Percent Non-Hispanic White` = `Non-Hispanic White` / `Total Pop` * 100,
      `Percent Non-Hispanic Black` = `Non-Hispanic Black` / `Total Pop` * 100,
      
      `Percent Structures more than 30 years old` = (built_tot - built_within5 - built_5to10 - built_10to20 - built_20to30 )/built_tot * 100,
      `Households in neighborhood 10 years or less` = case_when(year < 2015 ~ (moved_o_1 + moved_o_2 + moved_r_1 + moved_r_2)/moved_tot * 100,
                                                                year >= 201 ~ (moved_o_1 + moved_o_2 + moved_o_3 + moved_r_1 + moved_r_2 + moved_r_3)/moved_tot * 100)
    ) %>%
    
    # Removing original variables
    select(-c(dpov:laborforce_unemploy, 
              `Median Household Income, Asian`, 
              `Median Household Income, PI`,
              `Non-Hispanic White`,
              `Non-Hispanic Black`)) 

}
# Add column for year
acs_2012_processed <- process_vars(acs_2012, 2010) %>%
  mutate(YEAR = 2010)
acs_2020_processed <- process_vars(acs_2020, 2020) %>%
  mutate(YEAR = 2020)

#write_csv(acs_2012_processed, "outputs/acs_2012.csv")
saveRDS(acs_2012_processed, "outputs/acs_2012.RDS")

#write_csv(acs_2020_processed, "outputs/acs_2020.csv")
saveRDS(acs_2020_processed, "outputs/acs_2020.RDS")

