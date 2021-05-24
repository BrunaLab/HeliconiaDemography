#' Filter demography data for modeling
#' 
#' There are plants with 0 shoots and 0 ht.  I will remove those, as having no
#' above ground biomass is really not a size, but a different category. There's
#' also a couple of plants that are very likely data entry errors in height that
#' I'll remove
#' 
#' @param data combined demography and SPEI dataset
#'
filter_data <- function(data) {
  data %>% 
    filter(ht < 200 | is.na(ht), ht_prev < 200 | is.na(ht_prev)) %>%
    filter(
      # having no aboveground biomass is possible, but categorically different than just being small. Exclude plants with 0 shoots or 0 height
      shts_prev > 0,
      ht_prev > 0,
      #for survival data, must include plants with NA for shts and height (i.e. dead plants)
      shts > 0 | is.na(shts), 
      ht > 0 | is.na(ht)
    ) %>% 
    #only use post-seedlings for this
    filter(code_notes != "sdlg (1)" | is.na(code_notes))
}

subset_cf <- function(model_data_cf, n_plants = 1010) {
  set.seed(123)
  plant_ids <- unique(model_data_cf$ha_id_number)
  plant_sample <- sample(plant_ids, n_plants)
  model_data_cf %>% filter(ha_id_number %in% plant_sample)
}