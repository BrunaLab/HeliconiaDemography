#' Combine demographic data with lagged SPEI data
#'
#' @param demog completed demographic data
#' @param xa lagged SPEI data calculated using data from Xavier et al.
#'
join_filter_demog_spei <- function(demog, xa) {
  #manually combine by which plots are in which grid cells.
  demog2 <-
    demog %>% 
    mutate(lon = case_when(
      plot %in% c("Florestal-CF", "5752", "5751", "5750", "5756", "CaboFrio-CF") ~ "-59.875",
      plot %in% c("2206", "2108", "2107", "Dimona-CF", "5753", "PortoAlegre-CF") ~ "-60.125",
      TRUE ~ NA_character_
    ), .before = plot) %>% 
    select(-row, -column)
  
  xa2 <- 
    xa %>%
    filter(month(yearmonth) == 2) %>% 
    mutate(year = year(yearmonth), .after =yearmonth) %>% 
    separate(latlon, into = c("lat", "lon"), sep = "_")
  
  data_joined <- 
    left_join(demog2, xa2, by = c("lon", "year")) %>% 
    select(-lon, -lat, -yearmonth)
  
  data_joined %>% 
    filter(ht < 200 | is.na(ht), ht_prev < 200 | is.na(ht_prev)) %>%
    filter(
      # having no aboveground biomass is possible, but categorically different
      # than just being small. Exclude plants with 0 shoots or 0 height
      shts_prev > 0 | is.na(shts_prev),
      ht_prev > 0 | is.na(ht_prev),
      # for survival data, must include plants with NA for shts and height (i.e.
      # dead plants)
      shts > 0 | is.na(shts), 
      ht > 0 | is.na(ht)
    ) %>% 
    # only use post-seedlings for this analysis
    filter(code_notes != "sdlg (1)" | is.na(code_notes))
}