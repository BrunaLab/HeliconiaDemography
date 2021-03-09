calc_spei_xa <- function(data) {
  data %>% 
    unite(latlon, lat, lon) %>% 
    mutate(yearmonth = yearmonth(date)) %>% 
    group_by(latlon, yearmonth) %>% 
    summarize(precip = sum(precip), eto = sum(eto)) %>% 
    mutate(cb = precip - eto) %>% 
    as_tsibble(index = yearmonth, key = latlon) %>% 
    mutate(spei = as.numeric(spei(cb, scale = 3)$fitted))
}

lag_spei <- function(data, maxlag) {
  data %>%
    group_by(latlon) %>% 
    mutate(spei_history = Lag(spei, 0:maxlag),
           L = matrix(0:maxlag, nrow = n(), ncol = maxlag + 1, byrow = TRUE))%>% 
    select(latlon, yearmonth, spei, spei_history, L)
}
 
