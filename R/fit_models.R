
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