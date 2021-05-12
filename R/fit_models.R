
#' Fit survival GAM
#'
#' Fits the distributed lag non-linear model GAM for survival as a function of
#' plant size in previous year and SPEI history.  Makes use of parallel
#' computing with `mgcv::bam()`
#' @param data prepped model data
#' @param flwr_prev logical; include flowering in previous year as covariate?
#'
fit_surv <- function(data, flwr_prev = FALSE) {
  ncores <- detectCores()
  cl <- makeForkCluster(ncores - 2)
  
  f <- surv ~ 
    s(log_size_prev, bs = "cr") +
    s(plot, bs = "re") + #random intercept
    s(spei_history, L,
      bs = "cb",
      k = c(3, 35),  #bimodal response (at most) to drought, one knot per month lag.
      xt = list(bs = "cr"))
  
  if (flwr_prev == TRUE) {
    f <- update(f, .~. + flwr_prev)
  }
  
  bam(f,
      family = binomial,
      data = data,
      method = "REML",
      select = TRUE,
      cluster = cl)
}

#' Fit size GAM
#' 
#' Fits the distributed lag non-linear model GAM for plant size as a function of
#' plant size in previous year and SPEI history.  Makes use of parallel
#' computing with `mgcv::bam()`
#' 
#' @param data prepared model data
#' @param flwr_prev logical; include flowering in previous year as covariate?
#'
fit_growth <- function(data, flwr_prev = FALSE){
  ncores <- detectCores()
  cl <- makeForkCluster(ncores - 2)
  # use only living plants
  data2 <- data %>% filter(surv == 1, !is.na(log_size))
  
  f <- log_size ~ 
    # flwr_prev +
    s(log_size_prev, bs = "cr") + 
    s(plot, bs = "re") + #random effect of plot on intercept
    s(spei_history, L,
      bs = "cb",
      k = c(3, 35),  #unimodal-ish response to drought, but one knot per month lag.
      xt = list(bs = "cr"))
  
  if (flwr_prev == TRUE) {
    f <- update(f, .~. + flwr_prev)
  }
  
  bam(f,
      # family = gaussian(link = "identity"),
      family = scat(link = "identity"), #like leptokurtic gaussian.
      data = data2,
      method = "REML",
      select = TRUE,
      cluster = cl)
}

#' Fit size GAM
#' 
#' Fits the distributed lag non-linear model GAM for plant flowering probability
#' as a function of plant size in previous year and SPEI history.  Makes use of
#' parallel computing with `mgcv::bam()`
#' 
#' @param data prepared model data
#' @param flwr_prev logical; include flowering in previous year as covariate?
#' 
fit_flwr <- function(data, flwr_prev = FALSE) {
  ncores <- detectCores()
  cl <- makeForkCluster(ncores - 2)
  # use only living plants
  data2 <- data %>% filter(surv == 1, !is.na(log_size))
  
  f <- flwr ~ 
    # flwr_prev +
    s(log_size_prev, bs = "cr") +
    s(plot, bs = "re") +
    s(spei_history, L,
      bs = "cb",
      k = c(3, 35), 
      xt = list(bs = "cr"))
  
  if (flwr_prev == TRUE) {
    f <- update(f, .~. + flwr_prev)
  }
  
  bam(f,
      family = binomial,
      data = data2,
      method = "REML",
      select = TRUE,
      cluster = cl)
}


# fit_recruit <- function(data) {
#   
#   anon_recruit <-
#     data %>%
#     group_by(plot, year) %>%
#     summarize(total_sdlgs = sum(code_notes == "sdlg (1)", na.rm = TRUE),
#               total_infl = sum(infl_num, na.rm = TRUE)) %>%
#     mutate(total_infl_prev = lag(total_infl))
#   
#   #keep weather history
#   weather <-
#     data %>%
#     group_by(plot, year) %>%
#     slice(1) %>% select(spei_history, L)
#   
#   df <- full_join(anon_recruit, weather, by = c("plot", "year")) %>% 
#     filter(!is.na(total_infl_prev))
#   
#   #fit poisson model
#   m <- gam(total_sdlgs ~ 
#         s(plot, bs = "re") +
#         # s(total_infl_prev, bs = "cr") +
#         total_infl_prev +
#         s(spei_history, L,
#           bs = "cb",
#           k = c(3, 35),
#           xt = list(bs = "cr")),
#       # offset = log(total_infl_prev),
#       family = poisson,
#       select = TRUE,
#       method = "REML",
#       data = df)
#   
#   
# }