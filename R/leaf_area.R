read_tidy_la <- function(file) {
 la_raw <- read_excel(here("data", "HA-la-stems-ht.xlsx")) 
 la <- la_raw %>%
   janitor::clean_names() %>% 
   dplyr::rename(leaf_area = leaf_area_feb_99,
                 leaves = leaves_feb_99,
                 ht = height_feb_99,
                 shts = stems_feb_99)
}