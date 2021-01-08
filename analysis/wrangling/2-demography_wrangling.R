library(tidyverse)
library(here)
library(janitor)
library(lubridate)
library(tsibble)
library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

demog <- 
  read_csv(here("analysis", "data", "raw_data", "Ha_survey_with_Zombies.csv"), col_names = TRUE,
           cols(plot = col_character(),
                bdffp_reserve_no = col_character(),
                shts = col_integer(),
                year = col_integer(),
                infl = col_integer(),
                tag_number = col_character(),
                HA_ID_Number = col_character())) %>% 
  clean_names()
# one plant's x-coordinate entered with a comma.
demog <- 
  demog %>%
  mutate(x_09 = ifelse(
    ha_id_number == 5800,
    2.0,
    x_09
  ))

# When is a plant really dead?  It "dies" after the last non-`NA` value for ht.

demog <-
  demog %>% 
  group_by(ha_id_number) %>% 
  # Routine for figuring out last year of data collected
  mutate(x = cumsum(ht > 0 & !is.na(ht))) %>%
  mutate(n = 1:n()) %>% 
  mutate(surv = if_else(
    n == which.max(x == max(x)),
    0L,
    1L
  )) %>% 
  # remove rows after plant is dead
  filter(row_number() <= which(surv == 0)[1]) %>% 
  # If last year of data collected at that plot, it's not dead yet unless the notes say so.
  group_by(plot) %>% 
  mutate(surv = if_else(
    year == max(year) & code_notes != "dead (2)",
    NA_integer_,
    surv)) %>% 
  select(-n, -x) %>% 
  ungroup()

# count(demog, surv) # 2230 dead plants over course of study

# add height and number shoots in the next year

demog <- 
  demog %>% 
  group_by(ha_id_number) %>% 
  mutate(ht_next = lead(ht),
         shts_next = lead(shts)) %>% 
  ungroup()

# arrange columns
demog <- 
  demog %>%
  select(ranch, bdffp_reserve_no, plot, habitat, #site level
         row, column, x_09, y_09, ha_id_number, tag_number, #plot level
         year,
         ht, ht_next, shts, shts_next, infl, surv, code_notes, code2) #plant level

write_csv(demog, here("analysis", "data", "derived_data", "ha_survey.csv"))


# combine demography with SPEI history ------------------------------------

xa <- read_rds(here("analysis", "data", "derived_data", "xa_spei_lags.rds"))
plot_coords <- read_csv(here("analysis", "data", "raw_data", "plot_coords.csv"))


#Prep SPEI data. Get only rows for february, the month demographic surveys were conducted in
xa2 <- 
  xa %>%
  filter(month(yearmonth) == 2) %>% 
  mutate(year = year(yearmonth), .after =yearmonth) %>% 
  separate(latlon, into = c("lat", "lon"), sep = "_")

# Match site names to grid cell coordinates in SPEI data
demog2 <-
  demog %>% 
  mutate(lon = case_when(
    plot %in% c("Florestal-CF", "5752", "5751", "5750", "5756", "CaboFrio-CF") ~ "-59.875",
    plot %in% c("2206", "2108", "2107", "Dimona-CF", "5753", "PortoAlegre-CF") ~ "-60.125",
    TRUE ~ NA_character_
  ), .before = plot)

# Join
demog_full <- 
  full_join(demog2, xa2, by = c("lon", "year")) %>% 
  select(-lon, -lat, -yearmonth)

# Write to disk

write_csv(demog_full, here("analysis", "data", "derived_data", "ha_demog_spei.csv"))
