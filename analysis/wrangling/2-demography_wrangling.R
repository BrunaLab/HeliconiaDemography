
# Wrangling demographic survey data ---------------------------------------
#' Author: Eric R. Scott
#' Creation Date: 2020-01-08


# Load packages -----------------------------------------------------------
library(tidyverse)
library(here)
library(janitor)
library(conflicted)
source(here("R", "utils.R"))
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("lag", "dplyr")
# Load data ---------------------------------------------------------------
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

# A few entries don't read in because x_09 was entered with a comma or semicolon instead of a decimal.
fails <- problems(demog)
fails <-
  fails %>% 
  mutate(corrected = str_replace(actual, "[\\,;]", "\\."))

demog$x_09[fails$row] <- fails$corrected

# Survival ----------------------------------------------------------------
# The `surv` column should be 1 if a plant is currently alive in `year` and 0 if it's dead in `year`.
# tibble(year = 1990:2000, x = c(12, 34, NA, 12, 13, 14, 16, NA, NA, NA, NA)) %>%
#   mutate(alive = as_living(x)) %>%
#   filter(alive == 1 | is.na(lag(alive)))
# 
demog <-
  demog %>% 
  group_by(ha_id_number) %>% 
  arrange(ha_id_number, year) %>% 
  mutate(surv = as_living(ht, n = 3)) %>%  #1 if alive, 0 if dead (assumed dead after 2 trailing NAs)
  mutate(surv = if_else(!is.na(code_notes) & code_notes == "dead (2)", 0L, surv)) %>% 
  filter(surv == 1 | lag(surv) == 1) %>%  #remove entries after last year alive.
  ungroup()

count(demog, surv) #2673 dead plants over course of study

# Flowering ---------------------------------------------------------------
#' Number of inflorescences recorded, but flowering should just be 1 or 0
#' According to Emilio, there were no years when flowering wasn't surveyed, so
#' all NAs for flowering should be zero if height or shoot number was recorded.
#' 
#' Interestingly, there seem to be years when infl > 0 is recorded but there is
#' no record for ht or shts.
count(demog, infl)
demog <- 
  demog %>% 
  mutate(infl = ifelse(is.na(infl) & (!is.na(shts) | !is.na(ht)), 0, infl)) %>% 
  mutate(flwr = ifelse(infl > 0, 1, 0), .after = infl)

# Add year t-1 ------------------------------------------------------------
# add height and number shoots in the next year for each observation

demog <- 
  demog %>% 
  group_by(ha_id_number) %>% 
  mutate(ht_prev = lag(ht),
         shts_prev = lag(shts)) %>% 
  ungroup()

# arrange columns
demog <- 
  demog %>%
  select(ranch, bdffp_reserve_no, plot, habitat, #site level
         row, column, x_09, y_09, ha_id_number, tag_number, #plot level
         year,
         ht, ht_prev, shts, shts_prev, infl_num = infl, flwr, surv, code_notes, code2) #plant level


write_csv(demog, here("analysis", "data", "derived_data", "ha_survey.csv"))
