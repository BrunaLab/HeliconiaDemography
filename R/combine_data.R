#' Combine demographic data with lagged SPEI data
#'
#' @param demog completed demographic data
#' @param xa lagged SPEI data calculated using data from Xavier et al.
#'
join_demog_spei <- function(demog, xa) {
  #manually combine by which plots are in which grid cells.
  demog2 <-
    demog %>% 
    mutate(lon = case_when(
      plot %in% c("Florestal-CF", "5752", "5751", "5750", "5756", "CaboFrio-CF") ~ "-59.875",
      plot %in% c("2206", "2108", "2107", "Dimona-CF", "5753", "PortoAlegre-CF") ~ "-60.125",
      TRUE ~ NA_character_
    ), .before = plot)
  
  xa2 <- 
    xa %>%
    filter(month(yearmonth) == 2) %>% 
    mutate(year = year(yearmonth), .after =yearmonth) %>% 
    separate(latlon, into = c("lat", "lon"), sep = "_")
  
  left_join(demog2, xa2, by = c("lon", "year")) %>% 
    select(-lon, -lat, -yearmonth)
}