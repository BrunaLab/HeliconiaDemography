## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## Set options
options(tidyverse.quiet = TRUE)
tar_option_set()

## tar_plan supports drake-style targets and also tar_target()
tar_plan(
  # Prep SPEI data
  maxlag = 36,
  tar_target(xa_file, here("analysis", "data", "raw_data", "xavier_daily_0.25x0.25.csv"), format = "file"),
  xa_raw = read_csv(xa_file),
  xa_spei = calc_spei_xa(xa_raw),
  xa_lag = lag_spei(xa_spei, maxlag),
  
  # Prep demographic data
  tar_target(demog_file, here("analysis", "data", "raw_data", "Ha_survey_with_Zombies.csv"), format = "file"),
  demog_raw = read_fix_demog(demog_file),
  demog_surv = add_surv(demog_raw),
  demog_surv_size = add_size(demog_surv),
  demog_done = tidy_demog(demog_surv_size),
  demog_spei = join_demog_spei(demog_done, xa_lag),
  model_data = filter_data(demog_spei),
  model_data_cf = filter(model_data, habitat == "CF"),
  model_data_1ha = filter(model_data, habitat == "1-ha"),
  
  # Fit demographic models
  s_cf = fit_surv(model_data_cf),
  s_1ha = fit_surv(model_data_1ha),
  g_cf = fit_growth(model_data_cf),
  g_1ha = fit_growth(model_data_1ha),
  f_cf = fit_flwr(model_data_cf),
  f_1ha = fit_flwr(model_data_1ha),
  
  # Validate and summarize results
  tar_render(validate_models, "doc/validate_models.Rmd"),
  tar_render(model_summary, "doc/model_summary.Rmd"),
  
  # Descriptive / Exploratory Data Analysis Figures
  normals = normals_data(),
  normals_plot = plot_normals(normals),
  plot_dates = get_plot_daterange(model_data, maxlag),
  
  eda_spei = plot_eda_spei(model_data, xa_lag, plot_dates),
  eda_surv = plot_eda_surv_cohort(demog_done, plot_dates),
  eda_size = plot_eda_size(model_data, plot_dates),
  eda_flwr = plot_eda_flwr(model_data, plot_dates),
  eda_plot = plot_eda_combine(eda_size, eda_surv, eda_flwr, eda_spei)
  
  # Model output figures
  
  
)
