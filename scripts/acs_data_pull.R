library(tidycensus)
library(tidyverse)

# census_api_key("YOUR_KEY", install = T)

# S2506_C01_009
# Owner-occupied housing units with a mortgage!!Owner-occupied housing units with a mortgage!!VALUE!!Median (dollars)

#S2507_C01_010
# Owner-occupied housing units with a mortgage!!Owner-occupied housing units without a mortgage!!VALUE!!Median (dollars)

# Get present day ACS data -------




median_housing <-
  get_acs(
    geography = "tract",
    state = state.abb,
    variables = c(
      `Total Pop` = "B01001_001", 
      
      `Median Household Income, Total`  = "B19013_001",
      `Median Household Income, White` = "B19013A_001",
      `Median Household Income, Black` = "B19013A_001",
      `Median Household Income, Hispanic` = "B19013I_001",
      `Median Household Income, Asian` = "B19013D_001",
      `Median Household Income, PI` = "B19013E_001",
      
      dpov = "B17001_001", # Denominator for Poverty Measures
      
      npov = "B17001_002",
      n65povm = "B17001_015",
      n75povm = "B17001_016",
      n65povf = "B17001_029",
      n75povf = "B17001_030",
      
      nbpov = "B17001B_002",

        
      med.value.housing.mortgage = "S2506_C01_009",
      med.monthly.housing.mortgage = "S2506_C01_039",
      perc.no.mortgage = "S2507_C01_001",
      med.value.housing.no.mortgage = "S2507_C01_010",
      med.monthly.housing.no.mortgage = "S2507_C02_032"
    ),
    survey = "acs5",
    year = c(2010, 2019)
  )

# TO DO -----

# Add Asian + PI
# Calculate Per Captia Income INCPC (B19301 / Total Pop)
# Calculate Percent In poverty Npov/ dpov
# Calculate Percent in Poverty 65+

# Get decennial data --------



        
housing <-
  get_acs(
    geography = "tract",
    state = "CA",
    survey = "acs5",
    year = 2019,
    variables = c(
      median.smoc.mortgage = "DP04_0101",
      median.smoc.no.mortgage = "DP04_0109",
      median.rent = "DP04_0134"
    )
  )



