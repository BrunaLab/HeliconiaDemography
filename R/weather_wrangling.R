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
