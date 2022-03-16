# Create indicator variables to assess gentrification eligibility and determination

library(tidyverse)

combined <- readRDS("outputs/combined.RDS")

# Get County References for each Year --------
## TO DO Replace County as grouping factor to intersecting Places

  # county reference points to serve as the compactor for all tracts within that county
county_ref <- combined %>% 
  group_by(YEAR, STATE, COUNTY) %>%
  summarize(mhi_p50 = quantile(`Median Household Income, Total`, 0.5, na.rm = TRUE),
            mhi_p40 = quantile(`Median Household Income, Total`, 0.4, na.rm = TRUE),
            mhv_p50 = quantile(`Median Home Value`, 0.5, na.rm = TRUE),
            mhv_p40 = quantile(`Median Home Value`, 0.4, na.rm = TRUE),
            mhv_p66 = quantile(`Median Home Value`, 2/3, na.rm = TRUE),
            mr_p50 = quantile(`Median Rent`, 0.5, na.rm = TRUE),
            mr_p40 = quantile(`Median Rent`, 0.4, na.rm = TRUE),
            edu_p50 = quantile(`Percent with 4-year College Degree or More`, 0.5, na.rm = T),
            edu_p66 = quantile(`Percent with 4-year College Degree or More`, 2/3, na.rm = T)
            )


# Eligibility 
combined_indicators <- combined %>% 
  left_join(county_ref, by = c("YEAR", "STATE", "COUNTY")) %>%
  mutate(eligib_pop500 = ifelse(`Total Pop` < 500, 0, 1),
         eligib_mhi40 = ifelse(`Median Household Income, Total` <= mhi_p40, 1, 0),
         eligib_mhi50 = ifelse(`Median Household Income, Total` <= mhi_p50, 1, 0),
         eligib_mhv40 = ifelse(`Median Home Value` <= mhi_p40, 1, 0),
         eligib_mhv50 = ifelse(`Median Home Value` <= mhi_p50, 1, 0)
         ) %>%

# Determination
  mutate(determin_edu50 = ifelse(`Percent with 4-year College Degree or More` > edu_p50, 1, 0),
         determin_edu66 = ifelse(`Percent with 4-year College Degree or More` > edu_p66, 1, 0),
         determin_mhv66 = ifelse(`Median Home Value` > mhv_p66, 1, 0))


write_rds(combined_indicators, "outputs/combined_indicators.RDS")

