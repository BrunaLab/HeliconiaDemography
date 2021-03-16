read_tidy_trmm <- function(path) {
  read_csv(path) %>% 
    unite(latlon, lat, long, sep = "_") %>% 
    mutate(yearmonth = yearmonth(date), .after = date)
}

calc_spei_trmm <- function(trmm, xa_spei) {
  # # fortunately xavier et al data has same resolution and overlap in latlon, so can join eto
  
  # unique(xa_spei$latlon) %in% unique(trmm2$latlon)
  
  trmm_ts <- 
    left_join(trmm, xa_spei %>% select(yearmonth, latlon, eto), by = c("yearmonth", "latlon")) %>% 
    #I guess I only need grid cells that are also in the Xavier et al. dataset?
    filter(!is.na(eto)) %>% 
    as_tsibble(index = yearmonth, key = latlon)
  
  #calculate SPEI
  trmm_ts %>% 
    mutate(cb = precip - eto) %>% 
    group_by(latlon) %>% 
    group_nest() %>% 
    mutate(trmm_spei = map(data, ~as.numeric(spei(.x$cb, scale = 3)$fitted))) %>% 
    unnest(c(trmm_spei, data)) %>% 
    select(-cb)
}