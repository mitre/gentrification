library(tidycensus)
library(tidyverse)

# census_api_key("YOUR_KEY")

# S2506_C01_009
# Owner-occupied housing units with a mortgage!!Owner-occupied housing units with a mortgage!!VALUE!!Median (dollars)

#S2507_C01_010
# Owner-occupied housing units with a mortgage!!Owner-occupied housing units without a mortgage!!VALUE!!Mediain (dollars)


median_housing <-
  get_acs(
    geography = "tract",
    state = state.abb,
    variables = c(
      med.value.housing.mortgage = "S2506_C01_009",
      med.monthly.housing.mortgage = "S2506_C01_039",
      perc.no.mortgage = "S2507_C01_001",
      med.value.housing.no.mortgage = "S2507_C01_010",
      med.monthly.housing.no.mortgage = "S2507_C02_032"
    ),
    survey = "acs5",
    year = 2019
  )

        
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

housing <- housing %>% select(!ends_with("M")) %>%
  separate(
    col = NAME,
    into = c("Census Tract", "County", "State"),
    sep = ","
  )

