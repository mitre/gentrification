combined <- readRDS("outputs/combined.RDS")


# Get County References for each Year --------
## TO DO Replace County as grouping factor to interecting Places

  # county reference points to serve as the comparator for all tracts within that county
county_ref <- combined %>% 
  group_by(YEAR, STATE, COUNTY) %>%
  summarize(mhi_p50 = quantile(`Median Household Income, Total`, 0.5, na.rm = TRUE),
            mhi_p40 = quantile(`Median Household Income, Total`, 0.4, na.rm = TRUE),
            mhv_p50 = quantile(`Median Home Value`, 0.5, na.rm = TRUE),
            mhv_p40 = quantile(`Median Home Value`, 0.4, na.rm = TRUE),
            mr_p50 = quantile(`Median Rent`, 0.5, na.rm = TRUE),
            mr_p40 = quantile(`Median Rent`, 0.4, na.rm = TRUE),
            hs_p50 = quantile(`Percent with 4-year College Degree or More`, 0.5, na.rm = T),
            hs_p66 = quantile(`Percent with 4-year College Degree or More`, 2/3, na.rm = T)
            )


# Eligibility --------
combined %>% 
  mutate(eligib_pop500 = ifelse(`Total Pop` < 500, 0, 1))

# Determination --------