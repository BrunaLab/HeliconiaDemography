prep_bdffp <- function(bdffp_daily) {
  # remove accumulations
  bdffp2 <-
    bdffp_daily %>% 
    filter(!flag %in% c("A", "U"))
  
  # spread and add rows for missing dates
  bdffp_wide <- 
    bdffp2 %>% 
    #complete all the dates
    as_tsibble(key = site, index = date) %>% 
    fill_gaps() %>% 
    select(date, site, precip) %>% 
    as_tibble() %>% 
    pivot_wider(names_from = site, values_from = precip) %>% 
    clean_names()
  
  # Cluster sites by ranch to reduce missingness
  bdffp_wide2 <-
    bdffp_wide %>% 
    rowwise() %>% 
    mutate(colosso_clust = mean(c(colosso, florestal, cabo_frio, gaviao),
                                na.rm = TRUE),
           km_clust = mean(c(km37, km41),
                           na.rm = TRUE)) %>% 
    mutate(across(colosso_clust:km_clust, ~ifelse(is.nan(.x), NA, .x))) %>% 
    select(-cabo_frio, -colosso, -florestal, -gaviao, -km37, -km41)
  
  # add additional representations of time for Amelia.
  bdffp_wide2 %>% 
    mutate(
      year = year(date),
      doy = yday(date),
      yearmonth = yearmonth(date),
      .before = everything()
    )
}

impute_bdffp <- function(bdffp_full) {
  # Impute missing data -----------------------------------------------------
  # round(runif(1, 1, 1000))
  set.seed(937)
  all_cols <- colnames(select(bdffp_full, -year, -doy, -date))
  
  # variables with log-normal-ish distributions
  log_cols <-
    all_cols[str_detect(all_cols, "precip") |
               all_cols %in% c("km_clust", "colosso_clust", "porto_alegre", "dimona")]
  # do multiple imputation
  amelia(
    as.data.frame(bdffp_full),
    p2s = 0,
    m = 10,
    ts = "doy",
    cs = "year",
    intercs = TRUE,
    polytime = 3,
    logs = log_cols,
    idvars = c("date"),
    parallel = "multicore",
    ncpus = detectCores() - 1,
    empri = .01 * nrow(bdffp_full) #ridge penalty because of high degree of missingness
  )
}

calc_spei_bdffp <- function(bdffp_imputations, embrapa_mon) {
  imp_mon <-
    map(bdffp_imputations$imputations, ~{
      .x %>% 
        rename_with(~paste0(., ".precip"), c(dimona, porto_alegre, colosso_clust, km_clust)) %>% 
        select(date, ends_with(".precip")) %>% 
        mutate(yearmonth = tsibble::yearmonth(date)) %>% 
        group_by(yearmonth) %>% 
        summarize(across(-date, ~sum(.x, na.rm = TRUE))) %>% 
        #remove first and last month, as they aren't complete
        slice(-1, -nrow(.))
    })
  # Add mean ETO from EMBRAPA data
  imp_mon <-
    map(imp_mon, ~ {
      left_join(.x, embrapa_mon %>% summarize(eto = mean(eto)), by = "yearmonth")
    }) %>% 
    map(., ~{
      mutate(.x, across(ends_with(".precip"),
                        ~ .x - eto,
                        .names = "{str_remove(col, '.precip')}.cb"))
    })
  
  
  imp_spei <-
    map(imp_mon, ~{
      .x %>% 
        mutate(across(ends_with(".cb"), ~as.numeric(SPEI::spei(.x, scale = 3)$fitted),
                      .names = "{str_remove(col, '.cb')}.spei")) %>%
        select(yearmonth, ends_with(c(".precip", ".spi", ".spei")))
    }) 
  
  # imp_spei$imp1
  # Combine multiply imputed SPI and SPEI results by taking mean
  
  imp_spei %>%
    bind_rows(.id = "imp") %>% 
    #replace any infinite values with NAs
    mutate(across(ends_with(".spei"), ~ ifelse(is.infinite(.x), NA, .x))) %>% 
    group_by(yearmonth) %>% 
    summarize(across(where(is.numeric), ~mean(., na.rm = TRUE))) %>% 
    #replace NaN's with NAs
    mutate(across(ends_with(".spei"), ~ ifelse(is.nan(.x), NA, .x))) %>% 
    rowwise() %>% 
    mutate(bdffp_spei = mean(c_across(ends_with(".spei")))) %>% 
    ungroup()
}