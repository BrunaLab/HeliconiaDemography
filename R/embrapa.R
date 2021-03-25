#' Read in and tidy rainfall data from EMBRAPA stations
#'
#' @param rpde EMBRAPA dataset for Rio Preta do Eva station
#' @param manaus EMBRAPA dataset for Manaus station
#'
tidy_embrapa <- function(rpde, manaus) {
 rpde <- 
   rpde %>% 
    select(date = Data,
           precip = Precipitacao,
           eto = Evatotranspiracao,
    )
 manaus <-
   manaus %>% 
   select(date = Data,
          precip = Precipitacao,
          eto = Evatotranspiracao,
   )

 bind_rows("rpde" = rpde, "manaus" = manaus, .id = "station") %>% 
   mutate(yearmonth = yearmonth(date), .after = date) %>% 
   group_by(yearmonth, station) %>% 
   summarize(across(c(precip, eto), sum), .groups = "drop") %>% 
   as_tsibble(index = yearmonth, key = station)
}

#' Calculate SPEI
#'
#' @param embrapa_mon monthly aggregated data from EMBRAPA stations
#'
calc_spei_embrapa <- function(embrapa_mon, scale = 3) {
  embrapa_spei <-
    embrapa_mon %>% 
    #calculate climate balance
    mutate(cb = precip - eto) %>% 
    group_by(station) %>% 
    group_nest() %>% 
    mutate(st_spei = map(data, ~as.numeric(spei(.x$cb, scale = scale)$fitted))) %>% 
    unnest(c(st_spei, data)) %>% 
    select(-cb) 
  
  embrapa_spei %>% 
    select(yearmonth, station, st_spei) %>% 
    pivot_wider(names_from = station,
                values_from = st_spei,
                names_prefix = "spei_") %>% 
    rowwise() %>% 
    mutate(st_spei = mean(c(spei_manaus, spei_rpde))) %>% 
    ungroup()
}