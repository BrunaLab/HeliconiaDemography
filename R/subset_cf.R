
subset_cf <- function(model_data_cf, n_plants = 1375) {
  set.seed(342)
  plant_ids <- unique(model_data_cf$ha_id_number)
  plant_sample <- sample(plant_ids, n_plants)
  model_data_cf %>% dplyr::filter(ha_id_number %in% plant_sample)
}