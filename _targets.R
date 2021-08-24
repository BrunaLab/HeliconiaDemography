## Set options for using tar_make_clustermq()
## Uncomment and edit these lines to run model targets on a cluster computer via SSH
options(
  clustermq.scheduler = "ssh",
  clustermq.ssh.host = "ericscott@hpg.rc.ufl.edu", # use your user and host
  clustermq.ssh.log = "~/cmq_ssh.log" # log for easier debugging
  # clustermq.worker.timeout = 2400
)

# ## Uncomment these lines to run locally on multiple cores
# options(
#   clustermq.scheduler = "multiprocess"
# )

## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source) 

## Set options
options(tidyverse.quiet = TRUE)
# tar_option_set(deployment = "main") #run targets locally by default

## tar_plan supports drake-style targets and also tar_target()
tar_plan(
  # Prep SPEI data
  maxlag = 36,
  tar_target(xa_file, here("data", "xavier_daily_0.25x0.25.csv"), format = "file"),
  xa_raw  = read_csv(xa_file),
  xa_spei = calc_spei_xa(xa_raw),
  xa_lag  = lag_spei(xa_spei, maxlag),
  
  # Prep demographic data
  tar_target(demog_file, here("data", "Ha_survey_with_Zombies.csv"), format = "file"),
  demog_raw  = read_fix_demog(demog_file),
  ## filter_dupes() removes some duplicate HA id numbers.  Will eventually be fixed in raw data
  demog_done = demog_raw %>% filter_dupes() %>% wrangle_demog(), 
  model_data        = join_filter_demog_spei(demog_done, xa_lag),
  model_data_cf     = dplyr::filter(model_data, habitat == "CF"),
  model_data_1ha    = dplyr::filter(model_data, habitat == "1-ha"),
  model_data_cf_sub = subset_cf(model_data),
  
  # Data validation
  tar_render(validate_data, "doc/validate_data.Rmd", deployment = "main"),
  
  # Select number of knots for GAMs
  tar_render(knot_choice, "doc/knot_choice.Rmd", deployment = "main"),
  
  # Fit demographic models
  tar_target(s_cf,  fit_surv(model_data_cf, k = c(10,15,15))),
  tar_target(s_1ha, fit_surv(model_data_1ha, k = c(10,10,10))),
  tar_target(g_cf,  fit_growth(model_data_cf, k = c(25,5,15))),
  tar_target(g_1ha, fit_growth(model_data_1ha, k = c(10,5,15))),
  tar_target(f_cf, fit_flwr(model_data_cf, k = c(10,15,15))),
  tar_target(f_1ha, fit_flwr(model_data_1ha, k = c(10,10,18))),
  
  # Validate and summarize results
  ### Check for edf differences due to sample size
  tar_target(g_cf_sub, fit_growth(model_data_cf_sub)),
  tar_target(f_cf_sub, fit_flwr(model_data_cf_sub)),
  tar_target(s_cf_sub, fit_surv(model_data_cf_sub)),
  tar_render(validate_models, "doc/validate_models.Rmd", deployment = "main"),

  # Descriptive / Exploratory Data Analysis Figures
  normals = normals_data(),
  normals_plot = plot_normals(normals),
  plot_dates = get_plot_daterange(model_data, maxlag),
  
  eda_spei = plot_eda_spei(xa_lag, plot_dates),
  eda_surv_ts = plot_eda_surv_ts(model_data, plot_dates),
  eda_size = plot_eda_size_foldchange(model_data, plot_dates),
  eda_flwr = plot_eda_flwr(model_data, plot_dates),
  eda_plot = plot_eda_combine(eda_size, eda_surv_ts, eda_flwr, eda_spei),
  
  surv_curve = plot_eda_surv_cohort(demog_done),
  
  # Model output figures

  ## Survival
  s_cf_eval = my_eval_smooth(s_cf),
  s_1ha_eval = my_eval_smooth(s_1ha),
  s_spei_plot = plot_cb_3panel(s_cf_eval, s_1ha_eval,
                               response_lab = "P(survival)"),

  ## Growth
  g_cf_eval  = my_eval_smooth(g_cf),
  g_1ha_eval = my_eval_smooth(g_1ha),
  g_spei_plot = plot_cb_3panel(g_cf_eval, g_1ha_eval,
                               response_lab = "$log(size_{t+1})$"),

  ## Flowering
  f_cf_eval  = my_eval_smooth(f_cf),
  f_1ha_eval = my_eval_smooth(f_1ha),
  f_spei_plot = plot_cb_3panel(f_cf_eval, f_1ha_eval,
                               response_lab = "P(flowering)"),

  ## Size covariate
  s_covar_plot = plot_covar_smooth(frag_model = s_1ha, cf_model = s_cf,
                                   covar = "log_size_prev") + 
    labs(x = TeX("$log(size_t)$"), y = "P(survival)"),
  g_covar_plot = plot_covar_smooth(frag_model = g_1ha, cf_model = g_cf,
                                   covar = "log_size_prev") +
    labs(x = TeX("$log(size_t)$"), y = TeX("log(size_{t+1})")),
  f_covar_plot = plot_covar_smooth(frag_model = f_1ha, cf_model = f_cf,
                                   covar = "log_size_prev") +
    labs(x = TeX("$log(size_t)$"), y = "P(flowering)"),
  size_plot = make_size_plot(
    s = s_covar_plot,
    g = g_covar_plot,
    f = f_covar_plot,
    model_data = model_data
  ), 

  # Main text
  tar_render(paper, "doc/paper.Rmd", deployment = "main"),

  # Supplemental
  tar_target(rpde_file,
             here("data", "supplemental", "Estacao_Rio Preto da Eva_1980-01-01_2016-12-31.csv"),
             format = "file"),
  tar_target(manaus_file,
             here("data", "supplemental", "Estacao_Manaus_1980-01-01_2016-12-31.csv"),
             format = "file"),
  rpde = read_csv(rpde_file),
  manaus = read_csv(manaus_file),
  embrapa_mon = tidy_embrapa(rpde,  manaus),
  embrapa_wide = calc_spei_embrapa(embrapa_mon),
  tar_target(trmm_file, here("data", "supplemental", "trmm.csv"), format = "file"),
  trmm = read_tidy_trmm(trmm_file),
  tar_target(gspei_file,
             here("data", "supplemental", "global_spei_-59.75_-2.25.csv"),
             format = "file"),
  gspei = read_tidy_gspei(gspei_file),
  trmm_spei  = calc_spei_trmm(trmm, xa_spei),
  tar_target(la_file, here("data", "HA-la-stems-ht.xlsx"), format = "file"),
  la_data = read_tidy_la(la_file),

  tar_render(supplemental, "doc/supplemental.Rmd", deployment = "main")

)
