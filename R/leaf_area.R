read_tidy_la <- function(file) {
 la_raw <- read_excel(here("data", "HA-la-stems-ht.xlsx")) 
 la <- la_raw %>%
   janitor::clean_names() %>% 
   dplyr::rename(leaf_area = leaf_area_feb_99,
                 leaves = leaves_feb_99,
                 ht = height_feb_99,
                 shts = stems_feb_99) %>% 
   mutate(size = shts * ht) %>% 
   #log transform all size variables
   mutate(across(c(leaf_area, leaves, ht, shts, size), list(log = log), .names = "{.fn}_{.col}")) %>% 
   filter(!is.na(leaf_area) & !is.na(ht) & !is.na(shts)) %>% 
   filter(shts > 0)
}