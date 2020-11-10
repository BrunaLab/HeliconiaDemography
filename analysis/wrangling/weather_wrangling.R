library(tidyverse)
library(here)
library(lubridate)
library(tsibble)

library(conflicted)
conflict_prefer("lag", "dplyr")


precip <- read_csv(here("analysis", "data", "raw_data", "mon_precip_spi_imputed.csv"))

# Goal: a dataframe with rows for each year:site combination, columns with SPEI in jan, dec, nov, oct, ...jan_prev, dec_prev, etc. Say, 24 months.
# Census occurrs late Jan, early Feb.

df_spei <- 
  precip %>% 
  mutate(year = year(date),
         month = month(date, label = TRUE),
         yearmonth = yearmonth(date)) %>% 
  select(date, year, month, yearmonth, site, spei)

# An alternative way of doing lags that might be useful in the future.
# df_spei %>% 
#   mutate(Q = tsModel::Lag(spei, 1:24))

df_wide <- 
  df_spei %>% 
  mutate(month = fct_rev(month)) %>% #reverse order of months
  arrange(site, year, month) %>% 
  pivot_wider(c(year, month, site, spei), names_from = month, values_from = spei)

# df_wide
# cool, going backwards now, but I want to start in january.

spei_lag <- 
  df_wide %>% 
  group_by(site) %>% 
  transmute(census_year = year, spei_1 = Jan, across(Dec:Feb, lag, .names = "spei_{2:12}")) %>% 
  mutate(across(spei_1:spei_12, lag, .names = "spei_{13:24}"))
spei_lag

#spei_1 is Jan, spei_2, is Dec of the previous year, spei_3 is Nov of the previous year, etc.

write_csv(spei_lag, here("analysis", "data", "derived_data", "spei_lags.csv"))

# Repeat with gridded data from Xavier et al.
xa <- read_csv(here("analysis", "data", "raw_data", "xavier_daily_0.25x0.25.csv"))
library(SPEI)
library(tsModel)
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
  mutate(Q_spei = Lag(spei, 1:24)) 

# p <-
#   ggplot(xa_spei_lags, aes(x = yearmonth, y = spei, color = latlon)) +
#   geom_line() +
#   facet_wrap(~year(yearmonth), scales = "free_x")

# annotate_spei(p)  

write_rds(xa_spei_lags, here("analysis", "data", "derived_data", "xa_spei_lags.rds"))
read_rds(here("analysis", "data", "derived_data", "xa_spei_lags.rds"))
