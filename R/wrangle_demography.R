#' Read in demography data and fix entry errors
#'
#' @param path location of file
#'
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
  
  #If ht is between 0 and 1, round up to 1 cm. All other data is recorded to the
  #nearest cm, so these values should be as well.
  demog <-
    demog %>% 
    mutate(ht = if_else(ht < 1 & ht > 0, 1, ht))

  return(demog)
}


#' Wrangle demography data
#' 
#' Adds a binary columns for flowering and survival, calculates plant size, adds
#' columns for values in previous year, and sets appropriate columns to factors.
#'
#' @param data Heliconia demography dataset
#' @param n_years number of years missing before assumed dead (at last year
#'   recorded)
#'
wrangle_demog <- function(data, n_years = 3) {
  #Calculate binary survival column
  data_surv <-
    data %>% 
    group_by(ha_id_number) %>% 
    arrange(ha_id_number, year) %>% 
    mutate(surv = as_living(ht, n = n_years)) %>%  #1 if alive, 0 if dead (assumed dead after 2 trailing NAs)
    mutate(surv = if_else(!is.na(code_notes) & code_notes == "dead (2)", 0L, surv)) %>% 
    filter(surv == 1 | lag(surv) == 1) %>%  #remove entries after last year alive.
    ungroup()
  
  # add height and number shoots in the next year for each observation.
  # calculate size as shts * ht
  data_surv_size <-
    data_surv %>%
    group_by(ha_id_number) %>% 
    mutate(ht_prev = lag(ht),
           shts_prev = lag(shts)) %>% 
    ungroup() %>% 
    mutate(
      size = shts * ht,
      size_prev = shts_prev * ht_prev,
      log_size = log(size),
      log_size_prev = log(size_prev)
    )
  
  data_surv_size %>% 
    #only use 1ha fragments and continuous forest
    filter(habitat %in% c("CF", "1-ha")) %>% 
    #add binary column for flowering
    mutate(infl = if_else(is.na(infl) & (!is.na(shts) | !is.na(ht)), 0L, infl)) %>% 
    mutate(flwr = if_else(infl > 0, 1L, 0L), .after = infl) %>% 
    mutate(flwr_prev = lag(flwr)) %>% 
    #create factors
    mutate(across(
      c(ranch, bdffp_reserve_no, plot, habitat, ha_id_number, flwr_prev),
      as.factor
    )) %>%
    mutate(year_fac = as.factor(year)) %>% 
    # arrange columns
    select(ranch, bdffp_reserve_no, plot, habitat, #site level
           row, column,
           # x_09, y_09, 
           ha_id_number, #plot level
           year,
           ht, ht_prev, shts, shts_prev,
           size, size_prev, log_size, log_size_prev,
           infl_num = infl, flwr, flwr_prev, surv,
           code_notes)
}
