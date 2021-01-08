
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
#TODO: Change this so plants are only dead after 2 years of NAs or if marked dead.

demog <-
  demog %>% 
  group_by(ha_id_number) %>% 
  arrange(ha_id_number, year) %>% 
  mutate(alive = as_living(ht, n = 3)) %>%  #1 if alive, 0 if dead (assumed dead after 2 trailing NAs)
  mutate(alive = if_else(!is.na(code_notes) & code_notes == "dead (2)", 0L, alive)) %>% 
  mutate(surv = lead(alive)) %>% # did a plant survive to the next year?
  filter(alive == 1) %>%  #remove entries after last year alive.
  select(-alive) %>% #remove to reduce confusion
  ungroup()

count(demog, surv) #2673 dead plants over course of study

# Flowering ---------------------------------------------------------------
#number of inflorescences recorded, but flowering should just be 1 or 0
#according to Emilio, there were no years when flowering wasn't surveyed, so all NAs for flowering should be zero if height or shoot number was recorded
count(demog, infl)
demog <- 
  demog %>% 
  mutate(infl = ifelse(is.na(infl) & (!is.na(shts) | !is.na(ht)), 0, infl)) %>% 
  mutate(flwr = ifelse(infl > 0, 1, 0), .after = infl)

# Add year t+1 ------------------------------------------------------------
# add height and number shoots in the next year for each observation

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
         ht, ht_next, shts, shts_next, infl_num = infl, flwr, surv, code_notes, code2) #plant level


write_csv(demog, here("analysis", "data", "derived_data", "ha_survey.csv"))
