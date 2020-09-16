library(tidyverse)
library(here)
library(janitor)
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
         row, column,ha_id_number, tag_number, #plot level
         year,
         ht, ht_next, shts, shts_next, infl, surv, code_notes, code2) #plant level

write_rds(demog, here("analysis", "data", "derived_data", "ha_survey.rds"))
