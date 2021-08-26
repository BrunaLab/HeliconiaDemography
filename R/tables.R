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



#' Make results table showing model intercepts ± confidence limits
#' 
#' Shows intercepts on a response scale with 84% confidence intervals.
#'
#' @param s_cf survival in CF model
#' @param s_1ha survival in FF model
#' @param g_cf size in CF model
#' @param g_1ha size in FF model
#' @param f_cf flowering in CF model
#' @param f_1ha flowering in FF model
#' 
make_intercept_table <- function(s_cf, s_1ha, g_cf, g_1ha, f_cf, f_1ha) {
  set_flextable_defaults(font.family = "Cambria", font.size = 10)
  expand_grid(`vital rate` = c("survival", "size", "flowering"), 
              habitat = c("CF", "1 ha")) %>% 
    add_column(intercept = map_chr(list(s_cf, s_1ha, g_cf, g_1ha, f_cf, f_1ha), pull_ci)) %>% 
    flextable() %>% 
    autofit() %>% 
    merge_v(j = 1) %>% 
    valign(j = 1, valign = "top") %>% 
    align(j = 3, align = "right", part = "all") %>% 
    bold(j = 3, i = c(3:6)) %>% 
    fix_border_issues()
}

#' Helper function for formatting EDF/DF
#' 
#' Degrees of freedom are integers and should be displayed with no decimal
#' places while EDF can have decimal places.  Needed this helper to format the
#' DF/EDF column correctly with `flextable`.
#'
#' @param x numeric vector
#' @param digits digits for non-integers
#'
int_or_round <- function(x, digits = 2) {
  foo <- function(x, digits) {
    if(is.na(x)) {
      return(NA)
    } else if(x%%1 == 0) {
      return(format(round(x)))
    } else {
      return(format(round(x, digits)))
    }
  }
  sapply(x, foo, digits)
}


make_results_df <- function(cf_model, ff_model) {
  ff <- summary(ff_model)
  ff_param <- ff$pTerms.table
  ff_smooth <- ff$s.table
  ff_r2 <- ff$r.sq
  
  cf <- summary(cf_model)
  cf_param <- cf$pTerms.table
  cf_smooth <- cf$s.table
  cf_r2 <- cf$r.sq
  
  ff_table <- bind_rows(
    ff_param %>% as_tibble(rownames = "term") %>% rename(`(e)df` = df),
    ff_smooth %>% as_tibble(rownames = "term") %>%  select(- Ref.df) %>% rename(`(e)df` = edf)
  ) 
  ff_table <- bind_rows(tibble(R2 = ff_r2), ff_table)

  
  cf_table <- bind_rows(
    cf_param %>% as_tibble(rownames = "term") %>% rename(`(e)df` = df),
    cf_smooth %>% as_tibble(rownames = "term") %>%  select(- Ref.df) %>% rename(`(e)df` = edf)
  ) 
  cf_table <- bind_rows(tibble(R2 = cf_r2), cf_table)
  
  df <- 
    bind_rows("CF" = cf_table, "1 ha" = ff_table, .id = "habitat") %>% 
    mutate(`p-value` = scales::pvalue(`p-value`)) %>% 
    mutate("(e)df" = int_or_round(`(e)df`, 2)) %>% 
    mutate(term = case_when(
      term == "flwr_prev"          ~ "flowered",
      term == "s(log_size_prev)"   ~ "s(log(size_t))",
      term == "s(spei_history,L)"  ~ "s(SPEI, lag)",
      TRUE                         ~ term
    ))
  names(df)[5] <- "test statistic"
  df
}

make_results_table <- function(s_cf, s_1ha, g_cf, g_1ha, f_cf, f_1ha) {
  cf_list <- list(s_cf, g_cf, f_cf)
  ff_list <- list(s_1ha, g_1ha, f_1ha)
  
  df_list <- 
    purrr::map2(cf_list, ff_list, ~make_results_df(.x, .y))
  
  vrates <- list("survival", "size_next", "flowering")
  
  df <- map2_df(vrates, df_list, ~bind_rows(tibble(`vital rate` = .x), .y))
  set_flextable_defaults(font.family = "Cambria", font.size = 10)
  flextable(df) %>% 
    merge_v(j = c(2)) %>% 
    valign(j = c(2), valign = "top") %>% 
    align(j = c(5, 7), align = "right", part = "all") %>%
    colformat_double(j = c(3,6), digits = 2) %>% 
    fix_border_issues() %>% 
    flextable::compose(j = 3, part = "header", value = as_paragraph("R", as_sup("2"))) %>% 
    flextable::compose(j = 4, i = c(4,9,15,20,26,31),
                       value = as_paragraph("s(log(size", as_sub("t"), "))")) %>%
    flextable::compose(j = 1, i = 12, 
                       value = as_paragraph("log(size", as_sub("t+1"), ")")) %>% 
    autofit()
}



