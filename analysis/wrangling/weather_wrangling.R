library(tidyverse)
library(here)
library(lubridate)
library(tsibble)
library(tsModel)
library(SPEI)
library(conflicted)
conflict_prefer("lag", "dplyr")
conflict_prefer("filter", "dplyr")

# Set maximum lag in months
maxlag = 24

# Imputed rain gauge SPEI

precip <- read_csv(here("analysis", "data", "raw_data", "mon_precip_spi_imputed.csv"))

# Goal: a dataframe with rows for each year:site combination, columns with SPEI in jan, dec, nov, oct, ...jan_prev, dec_prev, etc. Say, 24 months.
# Census occurrs late Jan, early Feb.

df_spei <- 
  precip %>% 
  mutate(yearmonth = yearmonth(date)) %>% 
  select(yearmonth, site, spei)

spei_lag <-
  df_spei %>%
  group_by(site) %>%
  arrange(site, yearmonth) %>%
  mutate(spei_history = Lag(spei, 0:maxlag),
         L = matrix(0:maxlag, nrow = n(), ncol = maxlag + 1, byrow = TRUE))

#to join with demography, filter to get only history for february
# filter(spei_lag, month(yearmonth) == 2)

write_rds(spei_lag, here("analysis", "data", "derived_data", "spei_lags.rds"))

# Repeat with gridded data from Xavier et al.
xa <- read_csv(here("analysis", "data", "raw_data", "xavier_daily_0.25x0.25.csv"))

xa_spei <-
  xa %>% 
  unite(latlon, lat, lon) %>% 
  mutate(yearmonth = yearmonth(date)) %>% 
  group_by(latlon, yearmonth) %>% 
  summarize(precip = sum(precip), eto = sum(eto)) %>% 
  mutate(cb = precip - eto) %>% 
  as_tsibble(index = yearmonth, key = latlon) %>% 
  mutate(spei = as.numeric(spei(cb, scale = 3)$fitted))


xa_spei_lags <-
  xa_spei %>%
  group_by(latlon) %>% 
  mutate(spei_history = Lag(spei, 0:maxlag),
         L = matrix(0:maxlag, nrow = n(), ncol = maxlag + 1, byrow = TRUE))%>% 
  select(latlon, yearmonth, spei, spei_history, L)


# p <-
#   ggplot(xa_spei_lags, aes(x = yearmonth, y = spei, color = latlon)) +
#   geom_line() +
#   facet_wrap(~year(yearmonth), scales = "free_x")

# annotate_spei(p)  

write_rds(xa_spei_lags, here("analysis", "data", "derived_data", "xa_spei_lags.rds"))
