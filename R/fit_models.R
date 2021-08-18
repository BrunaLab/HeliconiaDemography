
#' Fit survival GAM
#'
#' Fits the distributed lag non-linear model GAM for survival as a function of
#' plant size in previous year and SPEI history.  Makes use of parallel
#' computing with `mgcv::bam()`
#' @param data prepped model data
#' @param k vector of knots to pass to `s(log_size_prev)` (default 10), and
#'   `s(spei_history, L)` (default 3, 35).
#'   
fit_surv <- function(data, k = c(10, 3, 35)) {
  stopifnot(length(k) == 3L) #check that 3 values for k are supplied
  data2 <- data %>% mutate(flwr_prev = factor(flwr_prev))
  f <- surv ~ 
    flwr_prev +
    s(log_size_prev, bs = "cr", k = k[1]) +
    s(plot, bs = "re") + #random intercept
    s(spei_history, L,
      bs = "cb",
      k = k[2:3], 
      xt = list(bs = "cr"))
  
  bam(f,
      family = binomial,
      data = data2,
      method = "fREML",
      select = TRUE)
}

#' Fit size GAM
#' 
#' Fits the distributed lag non-linear model GAM for plant size as a function of
#' plant size in previous year and SPEI history.  Makes use of parallel
#' computing with `mgcv::bam()`
#' 
#' @param data prepared model data
#' @param k vector of knots to pass to `s(log_size_prev)` (default 10), and
#'   `s(spei_history, L)` (default 3, 35).
#'
fit_growth <- function(data, k = c(10, 3, 35)){
  stopifnot(length(k) == 3L) #check that 3 values for k are supplied
  # use only living plants
  data2 <- data %>% dplyr::filter(surv == 1, !is.na(log_size)) %>% 
    mutate(flwr_prev = factor(flwr_prev))
  
  f <- log_size ~ 
    flwr_prev +
    s(log_size_prev, bs = "cr", k = k[1]) + 
    s(plot, bs = "re") + #random effect of plot on intercept
    s(spei_history, L,
      bs = "cb",
      k = k[2:3],
      xt = list(bs = "cr"))
  
  bam(f,
      # family = gaussian(link = "identity"),
      family = scat(link = "identity"), #like leptokurtic gaussian.
      data = data2,
      method = "fREML",
      select = TRUE)
}

#' Fit size GAM
#' 
#' Fits the distributed lag non-linear model GAM for plant flowering probability
#' as a function of plant size in previous year and SPEI history.  Makes use of
#' parallel computing with `mgcv::bam()`
#' 
#' @param data prepared model data
#' @param ind_raneff logical; include individual-level random effect (i.e. plant ID as a random effect)?
#' 
fit_flwr <- function(data, k = c(10, 3, 35), ind_raneff = FALSE) {
  stopifnot(length(k) == 3L) #check that 3 values for k are supplied
  # use only living plants
  data2 <- data %>% dplyr::filter(surv == 1, !is.na(log_size)) %>%
    mutate(flwr_prev = factor(flwr_prev))
  
  f <- flwr ~ 
    flwr_prev +
    s(log_size_prev, bs = "cr", k = k[1]) +
    s(plot, bs = "re") +
    s(spei_history, L,
      bs = "cb",
      k = k[2:3], 
      xt = list(bs = "cr"))
  
  if (ind_raneff == TRUE) {
    f <- update(f, .~. + s(ha_id_number, bs = "re"))
  }
  
  bam(f,
      family = binomial,
      data = data2,
      method = "fREML",
      select = TRUE)
}