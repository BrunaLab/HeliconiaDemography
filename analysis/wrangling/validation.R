library(here)
library(tidyverse)
library(pointblank)

ha <-
  read_csv(
    here("analysis", "data", "derived_data", "ha_survey.csv"),
    col_names = TRUE,
    cols(plot = col_character(),
         bdffp_reserve_no = col_character())
  )

ha %>% 
  create_agent(
    actions = action_levels(warn_at = 0.1, stop_at = 0.2)
  ) %>% 
  col_is_character(vars(ranch, bdffp_reserve_no, plot, habitat, row, code_notes, code2)) %>% 
  col_is_numeric(vars(ha_id_number, tag_number, year, ht, ht_next, shts, shts_next, infl_num, surv, flwr)) %>% 
  col_vals_in_set(vars(surv, flwr), c(0,1, NA)) %>% 
  col_vals_in_set(vars(column), 1:10) %>% 
  col_vals_in_set(vars(row), LETTERS[1:10]) %>% 
  col_vals_in_set(vars(habitat), c("1-ha", "CF", "10-ha")) %>% 
  col_vals_between(vars(shts, shts_next), 1, 20, na_pass = TRUE) %>% 
  col_vals_between(vars(ht, ht_next), 0, 200, na_pass = TRUE) %>% 
  rows_distinct() %>% 
  interrogate()
