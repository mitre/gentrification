combined_indicators <- readRDS("outputs/combined_indicators.RDS")

get_determination_years <- function(year_start, year_end){
  start <- filter(combined_indicators, YEAR == year_start)
  end <- filter(combined_indicators, YEAR == year_end)
  
  duplicates <- start %>%
    group_by(GEOID)%>%
    count() %>%
    filter(n == 2)
  
  stable_tracts <- intersect(start$GEOID, end$GEOID)
  
  print(paste(
    round(length(stable_tracts)/ nrow(start) * 100, 2), 
    "% of tracts in", 
    year_start,
    "and",
    round(length(stable_tracts)/ nrow(end) * 100, 2),
    "% of tracts in",
    year_end,
    "are combined."))
  
  start <- start %>%
    filter(GEOID %in% stable_tracts) %>%
    arrange(GEOID) 
  
  end <- end %>%
    filter(GEOID %in% stable_tracts) %>%
    arrange(GEOID)
  
  joined_data <- left_join(start, end,
                           by = c("STATE", "COUNTY", "GEOID"),
                           suffix = c("_start", 
                                      "_end")) %>%
    filter(!is.na(MHLTH_end))
  
  return(joined_data)
}
