
# There are plants with 0 shoots and 0 ht.  I will remove those, as having no above ground biomass is really not a size, but a different category.
# There's also a couple of plants that are probably data entry errors in height.
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