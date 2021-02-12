# Combine demographic data with SPEI data ---------------------------------------
#' Author: Eric R. Scott
#' Creation Date: 2020-01-08


# load packages -----------------------------------------------------------
library(tidyverse)
library(here)
library(tsibble)

# Load Data ---------------------------------------------------------------
# SPEI data
xa <- read_rds(here("analysis", "data", "derived_data", "xa_spei_lags.rds"))

# cleaned demography data
demog <-
  read_csv(
    here("analysis", "data", "derived_data", "ha_survey.csv"),
    col_names = TRUE,
    cols(plot = col_character(),
         bdffp_reserve_no = col_character())
  )

# plot lat long estimates, for joining data
plot_coords <- read_csv(here("analysis", "data", "raw_data", "plot_coords.csv"))


# Prep SPEI data ----------------------------------------------------------

# Only want rows starting with February, the month the census is conducted in.
xa2 <- 
  xa %>%
  filter(month(yearmonth) == 2) %>% 
  mutate(year = year(yearmonth), .after =yearmonth) %>% 
  separate(latlon, into = c("lat", "lon"), sep = "_")

# Figure out which sites correspond to which grid cells.  The `xa` lat lon are the centers of grid cells that are 0.25º^2
# So, from -2.25 to -2.5

unique(xa2$lon)
mean(c(-59.5, -59.75))
mean(c(-59.75, -60)) # Florestal-CF, 5752, 5751, 5750, 5756, CaboFrio-CF
mean(c(-60, -60.25)) # 2206, 2108, 2107, Dimona-CF, 5753, PortoAlegre-CF


# Prep demography data ----------------------------------------------------
# manually match up site names with lat long values in SPEI dataset.
demog2 <-
  demog %>% 
  mutate(lon = case_when(
    plot %in% c("Florestal-CF", "5752", "5751", "5750", "5756", "CaboFrio-CF") ~ "-59.875",
    plot %in% c("2206", "2108", "2107", "Dimona-CF", "5753", "PortoAlegre-CF") ~ "-60.125",
    TRUE ~ NA_character_
  ), .before = plot)


# Join data ---------------------------------------------------------------
demog_full <- 
  left_join(demog2, xa2, by = c("lon", "year")) %>% 
  select(-lon, -lat, -yearmonth)


# Write to disk -----------------------------------------------------------
# This unfortunately has to be written as a .rds because of the matrix columns.
write_rds(demog_full, here("analysis", "data", "derived_data", "ha_spei_combined.rds"))