#' Calculate SPEI using data from Xavier et al.
#'
#' @param data tidied data from Xavier et al.
#'
calc_spei_xa <- function(data, scale = 3) {
  data %>% 
    unite(latlon, lat, lon) %>% 
    mutate(yearmonth = yearmonth(date)) %>% 
    group_by(latlon, yearmonth) %>% 
    dplyr::summarize(precip = sum(precip), eto = sum(eto)) %>% 
    mutate(cb = precip - eto) %>% 
    as_tsibble(index = yearmonth, key = latlon) %>% 
    mutate(spei = as.numeric(spei(cb, scale = scale)$fitted))
}

#' Create data with matrix columns for lagged SPEI
#' 
#' Converts a dataframe with a SPEI timeseries in a `spei` column into a
#' dataframe with two matrix columns: one for the lagged SPEI history and a
#' column describing the lag structure, `L`.
#'
#' @param data dataframe with an `spei` column
#' @param maxlag how many lags to calculate
#'
lag_spei <- function(data, maxlag) {
  data %>%
    group_by(latlon) %>% 
    mutate(spei_history = tsModel::Lag(spei, 0:maxlag),
           L = matrix(0:maxlag, nrow = n(), ncol = maxlag + 1, byrow = TRUE))%>% 
    dplyr::select(latlon, yearmonth, spei, spei_history, L)
}
 
