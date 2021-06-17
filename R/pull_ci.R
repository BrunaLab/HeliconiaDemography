#' Get CI around parametric terms of gam
#'
#' @param model a gam
#' @param conf.level confidence level. Default is 0.84
#'
#' @return formatted estimate [lower, upper]
#'
pull_ci <- function(model, conf.level = 0.84, digits = 3, type = c("response", "link")) {
  linkinv <- model$family$linkinv
  type <- match.arg(type)
  intercept <- coef(model)[1]
  ci <- confint.default(model, parm = "(Intercept)", level = conf.level)
  if (type == "response") {
    intercept <- linkinv(intercept)
    ci <- linkinv(ci)
  }
  intercept <- round(intercept, digits)
  ci <- round(ci, digits)
  ci_string <- glue::glue("{intercept} [{ci[1]}, {ci[2]}]")
  return(ci_string)
}
