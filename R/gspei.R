#' Read in and tidy global SPEI data
#'
#' @param path path to data file
#'
read_tidy_gspei <- function(path) {
  read_delim(path, delim = ";") %>% 
    rename(date = "days since 1900-1-1", g_spei = spei) %>% 
    mutate(yearmonth = yearmonth(date), date = NULL)
}