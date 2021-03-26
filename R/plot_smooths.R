#' Evaluate a smooth with confidence intervals and backtransform
#'
#' Evaluates a smooth, adds the model intercept, confidence intervals, and
#' backtransforms to the response scale. See help file for
#' gratia::smooth_estimates() for more details.
#'
#' @param model a model object produced by gam() or bam()
#' @param smooth which smooth, in quotes.
#' @param ... other arguments passed to gratia::smooth_estimates()
#'
my_eval_smooth <- function(model, smooth, ...) {
  
  linkinv <- model$family$linkinv
  
  gratia::smooth_estimates(model, smooth, ...) %>% 
    add_confint() %>% 
    mutate(across(c(est, lower_ci, upper_ci), ~linkinv(.x + coef(model)[1])))
}



#' Plots smooth covariates from two models
#'
#' @param cf_model gam model fit to continuous forest data
#' @param frag_model gam model fit to 1-ha fragment data
#' @param covar the smooth covariate, in quotes (e.g. "log_size_prev")
#' @param ... other arguments passed to gratia::smooth_estimates()
#'
plot_covar_smooth <- function(cf_model, frag_model, covar) {
  cf   <- my_eval_smooth(cf_model, covar, unconditional = TRUE)
  frag <- my_eval_smooth(frag_model, covar, unconditional = TRUE)
  
  data <- bind_rows("1-ha" = frag, "CF" = cf, .id = "habitat")
  
  ggplot(data, aes_string(x = covar, color = "habitat")) +
    geom_line(aes_string(y = "est")) +
    geom_ribbon(aes_string(ymin = "lower_ci", ymax = "upper_ci", color = NULL, fill = "habitat"), alpha = 0.25) +
    geom_rug(data = bind_rows(
      "1-ha" = model.frame(frag_model),
      "CF" = model.frame(cf_model), 
      .id = "habitat"),
      color = "black") +
    theme_bw()
}

#' Makes a little bar indicating wet and dry seasons.
#'
#' @param wet_color color for wet seasons
#' @param dry_color color for dry seasons
#' 
make_season_bar <- function(wet_color = "black", dry_color = "white") {
  wet_xmaxs = c(3, 15, 27, 36)
  wet_xmins = c(0, 8, 20, 32)
  dry_xmaxs = c(8, 20, 32)
  dry_xmins = c(3, 15, 27)
  df <- tibble(x = 0:36)
  ggplot(df, aes(x = x, y = 1)) +
    annotate(
      geom = "rect",
      xmin = wet_xmins,
      xmax = wet_xmaxs,
      ymin = 0.975,
      ymax = 1.025,
      fill = wet_color
    ) +
    annotate(
      geom = "rect",
      xmin = dry_xmins,
      xmax = dry_xmaxs,
      ymin = 0.975,
      ymax = 1.025,
      fill = dry_color
    )+
    scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0)) +
    theme_void()
}


#' Plot heatmap of SPEI crossbasis function
#' 
#' Creates figures from evaluated tensor product smooths
#' 
#'
#' @param eval_df Evaluated smooth data
#' @param fill_lims Shared limits for the color bar
#' @param binwidth binwidth for contour lines
#' @param response_lab label for response (color bar)
#' @param breaks breaks for x-axis
#'
plot_spei_heatmap <-
  function(eval_df,
           fill_lims,
           binwidth,
           response_lab,
           breaks = seq(0, 36, by = 2)) {
    
    season_bar <- make_season_bar()
    
    ggplot(eval_df, aes_string(y = "spei_history", x = "L")) +
      geom_raster(aes_string(fill = "est")) +
      geom_contour(aes_string(z = "est"), color = "black", binwidth = binwidth) +
      geom_hline(aes(yintercept = 0), color = "grey", linetype = 2) +
      scale_fill_viridis_c(response_lab, option = "viridis", limits = fill_lims) +
      scale_x_continuous("lag (months before census)",
                         breaks = breaks,
                         expand = c(0, 0)) +
      scale_y_continuous(TeX("SPEI_3_"), expand = expansion(mult = c(0.025, 0))) + #leave room for season bar at bottom
      theme_bw() +
      annotation_custom(
        grob = ggplotGrob(season_bar),
        ymin = -Inf,
        ymax = min(eval_df$spei_history),
        xmin = -Inf,
        xmax = Inf
      )
  }

#' Create 2-panel plots comparing evaluated crossbasis smooths for fragmented
#' and continuous forest habitat
#'
#' @param cf_model model object for continuous forest
#' @param frag_model model object for 1-ha fragment
#' @param smooth name of smooth to evaluate
#' @param response_lab response label (for color bar)
#' @param binwidth binwidth for contour lines
#'
plot_cb_2panel <-
  function(cf_model, frag_model, smooth = "spei_history", response_lab, binwidth) {
    df_cf <- my_eval_smooth(cf_model, smooth, dist = 0.1)
    df_frag <- my_eval_smooth(frag_model, smooth, dist = 0.1)
    
    fill_lims <- c(min(df_frag$est, df_cf$est, na.rm = TRUE),
                   max(df_frag$est, df_cf$est, na.rm = TRUE))
    
    frag_plot <-
      plot_spei_heatmap(
        df_frag,
        binwidth = binwidth,
        fill_lims = fill_lims,
        response_lab = response_lab
      )
    cf_plot <-
      plot_spei_heatmap(
        df_cf,
        binwidth = binwidth,
        fill_lims = fill_lims,
        response_lab = response_lab
      )
    
    (cf_plot + theme(axis.title.x = element_blank())) /
      (frag_plot) +
      plot_layout(guides = "collect") +
      plot_annotation(tag_levels = "a", tag_suffix = ")") &
      labs(title = NULL) #remove plot titles
    
  }

#' Plot difference between evaluated crossbasis smooths in two habitats
#'
#' @param cf_model model object for continuous forest
#' @param frag_model model object for 1-ha fragment
#' @param smooth name of smooth
#' @param response_lab label for response (color bar)
#' @param binwidth binwidth for contour lines
#' @param breaks breaks along lag axis
#'
#' @return
#' @export
#'
#' @examples
plot_cb_diff <- function(cf_model, frag_model, smooth = "spei_history", response_lab, binwidth, breaks = seq(0, 36, by = 2)) {
  df_cf <- my_eval_smooth(cf_model, smooth, dist = 0.1)
  df_frag <- my_eval_smooth(frag_model, smooth, dist = 0.1)
  season_bar <- make_season_bar()
  df <-
    bind_cols(
      df_cf %>% rename_with(.fn = ~glue("cf_{.}")),
      df_frag %>% rename_with(.fn = ~glue("frag_{.}"))
    ) %>%
    mutate(est = cf_est - frag_est) %>%
    select(est, L = cf_L, spei_history = cf_spei_history)
  
  plot_spei_heatmap(df, fill_lims = NULL, binwidth = binwidth, response_lab = response_lab)    
}
