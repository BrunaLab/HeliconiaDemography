## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## Set options
options(tidyverse.quiet = TRUE)
tar_option_set()

## tar_plan supports drake-style targets and also tar_target()
tar_plan(
  tar_target(maxlag, 36),
  tar_target(xa_file, here("analysis", "data", "raw_data", "xavier_daily_0.25x0.25.csv"), format = "file"),
  tar_target(xa_raw, read_csv(xa_file)),
  tar_target(xa_spei, calc_spei_xa(xa_raw)),
  tar_target(xa_lag, lag_spei(xa_spei, maxlag)),
  
  tar_target(demog_file, here("analysis", "data", "raw_data", "Ha_survey_with_Zombies.csv"), format = "file"),
  tar_target(demog_raw, read_fix_demog(demog_file)),
  tar_target(demog_surv, add_surv(demog_raw)),
  tar_target(demog_done, tidy_demog(demog_surv)),
  tar_target(demog_spei, join_demog_spei(demog_done, xa_lag))
)
