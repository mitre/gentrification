library(tidycensus)
library(tidyverse)

# census_api_key("YOUR_KEY", install = T)

# Get present day ACS data -------


get_acs_vars <- function(year){
  get_acs(
    geography = "tract",
    state = state.abb,
    survey = "acs5",
    output = "wide",
    year = year,
    variables = c(
      `Total Pop` = "B01001_001",
      `Median Household Income, Total`  = "B19013_001",
      `Median Household Income, White` = "B19013A_001",
      `Median Household Income, Black` = "B19013B_001",
      `Median Household Income, Hispanic` = "B19013I_001",
      `Median Household Income, Asian` = "B19013D_001",
      `Median Household Income, PI` = "B19013E_001",
      `Median Rent` = "B25058_001",
      `Median Home Value` = "B25077_001",
      
      dpov = "B17001_001",
      npov = "B17001_002",
      n65povm = "B17001_015",
      n75povm = "B17001_016",
      n65povf = "B17001_029",
      n75povf = "B17001_030",
      
      nbpov = "B17020B_002",
      dbpov = "B17020B_001",
      
      nwpov = "B17020H_002",
      dwpov = "B17020H_001",
      
      nhpov = "B17020I_002",
      dhpov = "B17020I_001",
      
      hu = "B25002_001", # Estimate!! Total: OCCUPANCY STATUS
      ohu = "B25003_001", # Estimate!! Total: TENURE
      own = "B25003_002", # Owner Occupied
      rent = "B25003_003", # Renter Occupied
      vac = "B25002_003", # Vacant
      
      hs = "B15003_017", # high school or less
      bachelors = "B15003_022", 
      masters = "B15003_023",
      professional = "B15003_024", 
      doctorate = "B15003_025", 
      ag25up = "B15003_001", # denominator for education, population 25+
      
      total_employ = "C18120_001",
      laborforce = "C18120_002",
      laborforce_employ = "C18120_003",
      laborforce_unemploy = "C18120_006"
    )
  )
}

# acs_2010 <- get_acs_vars(2010) # this one doesn't work?
acs_2012 <- get_acs_vars(2012) # But these do...?
acs_2019 <- get_acs_vars(2019)


# Calculate Measures -----

process_vars <- function(df) {
  
  # Removing the E-suffix from the Estimates
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
    
    
    # Measure Calculations
    mutate(
      `Per Capita Income` = `Median Household Income, Total` / `Total Pop`,
      `Percent in Poverty, Total` = npov / dpov,
      `Percent in Poverty, 65+` = (n65povm + n65povf + n75povm + n75povf) /
        dpov,
      `Percent in Poverty, Black` = nbpov / dbpov,
      `Percent in Poverty, White` = nwpov / dwpov,
      `Percent in Poverty, Hispanic` = nhpov / dhpov,
      
      `Percent Onwer-Occupied Units` = own / ohu,
      `Percent Vacant Units` = vac / hu,
      
      `Percent with High School Degree or Less` = hs / ag25up * 100,
      `Percent with 4-year College Degree or More` = (bachelors + masters + professional + doctorate) / ag25up * 100,
      `Percent Unemployed` = laborforce_unemploy / laborforce * 100,
      `Percent Labor Force Participation` = laborforce / total_employ * 100
    ) %>%
    
    # Removing original variables
    select(-c(dpov:laborforce_unemploy)) 


  
}

acs_2012_processed <- process_vars(acs_2012) %>%
  mutate(YEAR = 2010)
acs_2019_processed <- process_vars(acs_2019) %>%
  mutate(YEAR = 2019)

write_csv(acs_2012_processed, "outputs/acs_2012.csv")
saveRDS(acs_2012_processed, "outputs/acs_2012.RDS")

write_csv(acs_2019_processed, "outputs/acs_2019.csv")
saveRDS(acs_2019_processed, "outputs/acs_2019.RDS")

