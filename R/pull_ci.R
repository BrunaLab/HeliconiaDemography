#' Get CI around parametric terms of gam
#'
#' @param model a gam
#' @param conf.level confidence level. Default is 0.84
#'
#' @return formatted estimate [lower, upper]
#'
pull_ci <- function(model, conf.level = 0.84) {
  linkinv <- model$family$linkinv
  ci <-
    broom::tidy(model, conf.int = TRUE, parametric = TRUE, conf.level = conf.level) %>%
    mutate(across(c(estimate, conf.low, conf.high), ~linkinv(.x) %>% round(2))) %>%
    mutate(ci = glue::glue("{estimate} [{conf.low}, {conf.high}]"))
  return(ci$ci[1])
}