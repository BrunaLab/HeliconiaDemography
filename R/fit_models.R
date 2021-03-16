
fit_surv <- function(data) {
  ncores <- detectCores()
  cl <- makeForkCluster(ncores - 2)
  bam(surv ~ 
        s(log_size_prev, bs = "cr") +
        s(plot, bs = "re") + #random intercept
        # s(plot, log_size_prev, bs = "re") + #random slope
        s(spei_history, L,
          bs = "cb",
          k = c(3, 35),  #bimodal response (at most) to drought, one knot per month lag.
          xt = list(bs = "cr")),
      family = binomial,
      data = data,
      method = "REML",
      select = TRUE,
      cluster = cl)
}

fit_growth <- function(data){
  ncores <- detectCores()
  cl <- makeForkCluster(ncores - 2)
  # use only living plants
  data2 <- data %>% filter(surv == 1, !is.na(log_size))
  bam(log_size ~ 
        s(log_size_prev, bs = "cr") + 
        s(plot, bs = "re") + #random effect of plot on intercept
        s(spei_history, L,
          bs = "cb",
          k = c(3, 35),  #unimodal-ish response to drought, but one knot per month lag.
          xt = list(bs = "cr")),
      # family = gaussian(link = "identity"),
      family = scat(link = "identity"), #like leptokurtic gaussian.
      data = data2,
      method = "REML",
      select = TRUE,
      cluster = cl)
}

fit_flwr <- function(data) {
  ncores <- detectCores()
  cl <- makeForkCluster(ncores - 2)
  # use only living plants
  data2 <- data %>% filter(surv == 1, !is.na(log_size))
  bam(flwr ~ 
        s(log_size_prev, bs = "cr") +
        s(plot, bs = "re") +
        s(spei_history, L,
          bs = "cb",
          k = c(3, 35), 
          xt = list(bs = "cr")),
      family = binomial,
      data = data2,
      method = "REML",
      select = TRUE,
      cluster = cl)
}