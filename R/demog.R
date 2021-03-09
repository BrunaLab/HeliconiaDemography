read_fix_demog <- function(path) {
  demog <- 
    read_csv(path, col_names = TRUE,
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
  return(demog)
}


#' Calculate a binary survival column
#' The `surv` column should be 1 if a plant is currently alive in `year` and 0 if it's dead in `year`.
#'
#' @param data the heliconia demography data
#' @param n_years number of years missing before assumed dead (at last year recorded)
#'
add_surv <- function(data, n_years = 3) {
  data %>% 
    group_by(ha_id_number) %>% 
    arrange(ha_id_number, year) %>% 
    mutate(surv = as_living(ht, n = n_years)) %>%  #1 if alive, 0 if dead (assumed dead after 2 trailing NAs)
    mutate(surv = if_else(!is.na(code_notes) & code_notes == "dead (2)", 0L, surv)) %>% 
    filter(surv == 1 | lag(surv) == 1) %>%  #remove entries after last year alive.
    ungroup()
}

tidy_demog <- function(data) {
  data %>% 
    #add binary column for flowering
    mutate(infl = ifelse(is.na(infl) & (!is.na(shts) | !is.na(ht)), 0, infl)) %>% 
    mutate(flwr = ifelse(infl > 0, 1, 0), .after = infl) %>% 
    # add height and number shoots in the next year for each observation
    group_by(ha_id_number) %>% 
    mutate(ht_prev = lag(ht),
           shts_prev = lag(shts)) %>% 
    ungroup() %>% 
    # arrange columns
    select(ranch, bdffp_reserve_no, plot, habitat, #site level
           row, column, x_09, y_09, ha_id_number, tag_number, #plot level
           year,
           ht, ht_prev, shts, shts_prev, infl_num = infl, flwr, surv, code_notes, code2) #plant level
}
