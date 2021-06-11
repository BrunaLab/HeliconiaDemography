
#' Plots smooth covariates from two models
#'
#' @param cf_model gam model fit to continuous forest data
#' @param frag_model gam model fit to 1-ha fragment data
#' @param covar the smooth covariate, in quotes (e.g. "log_size_prev")
#' @param ... other arguments passed to gratia::smooth_estimates()
#'
plot_covar_smooth <- function(cf_model, frag_model, covar) {

  cf <- gratia::smooth_estimates(cf_model, covar, unconditional = TRUE) %>% add_confint()
  frag <- gratia::smooth_estimates(frag_model, covar, unconditional = TRUE) %>% add_confint()
  data <- bind_rows("1-ha" = frag, "CF" = cf, .id = "habitat")
  
  p <- 
    ggplot(data, aes_string(x = covar, color = "habitat", linetype = "habitat")) +
    geom_line(aes_string(y = "est")) +
    geom_ribbon(
      aes_string(
        ymin = "lower_ci",
        ymax = "upper_ci",
        color = NULL,
        fill = "habitat"
      ),
      alpha = 0.4,
      key_glyph = "path"
    )+
    theme_classic()
  p
}

make_size_plot <- function(s, g, f, model_data) {
  
  d <-
    ggplot(model_data, aes(x = log_size_prev, fill = habitat, color = habitat, linetype = habitat)) +
    geom_density(alpha = 0.4, key_glyph = "path") +
    labs(y = "Density", x = TeX("$log(size_t)$")) +
    theme_classic() +
    theme(legend.position = "none")
  
  top <-
    g /
    s /
    f & 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
  
  top /
    d +
    plot_layout(guides = "collect", ) +
    plot_annotation(tag_levels = "a", tag_suffix = ")") &
    theme(plot.margin = margin()) &
    # set color for all panels
    scale_color_manual("Habitat", values = c("#E66101", "#5E3C99"),
                       aesthetics = c("color", "fill")) &
    guides(linetype = guide_legend(title = "Habitat"))
    
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

#adds intercept and back-transforms smooth to response scale
my_eval_smooth <- function(model, smooth, ...) {
  linkinv <- model$family$linkinv
  gratia::smooth_estimates(model, smooth, dist = 0.1, ...) %>% 
    gratia::add_confint() %>% 
    add_column(intercept = coef(model)[1]) %>% 
    mutate(across(c(est, lower_ci, upper_ci), ~linkinv(.x + intercept)),
           intercept = linkinv(intercept))
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
           response_lab,
           breaks = seq(0, 36, by = 2),
           ci = TRUE) {
    
    season_bar <- make_season_bar()
    
    p <- 
      ggplot(eval_df, aes_string(y = "spei_history", x = "L", fill = "est")) +
      geom_raster() +
      geom_hline(aes(yintercept = 0), color = "grey", linetype = 2) +
      scale_fill_viridis_c(response_lab, option = "viridis", limits = fill_lims) +
      scale_x_continuous("lag (months before census)",
                         breaks = breaks,
                         expand = c(0, 0)) +
      scale_y_continuous(TeX("SPEI_3_"), expand = expansion(mult = c(0.025, 0))) + #leave room for season bar at bottom
      theme_classic() +
      annotation_custom(
        grob = ggplotGrob(season_bar),
        ymin = -Inf,
        ymax = min(eval_df$spei_history),
        xmin = -Inf,
        xmax = Inf
      )
    
    if (ci == TRUE) {
      #kludge to outline areas where 95%CI doesn't overlap intercept
      
      mask <- 
        eval_df %>% 
        rowwise() %>%
        dplyr::filter(!between(intercept, lower_ci, upper_ci)) %>% 
        ungroup()
      
      p <-
        p + 
        geom_tile(data = mask, color = "black", size = 0.75, linejoin = "round") +
        geom_raster(data = mask)
    }
    
    return(p)
  }




#' Create 2-panel plots comparing evaluated crossbasis smooths for fragmented
#' and continuous forest habitat
#'
#' @param cf_model model object for continuous forest
#' @param frag_model model object for 1-ha fragment
#' @param response_lab response label (for color bar)
#'
plot_cb_3panel <-
  function(cf_eval, frag_eval, response_lab) {
    
    fill_lims <- c(min(frag_eval$est, cf_eval$est, na.rm = TRUE),
                   max(frag_eval$est, cf_eval$est, na.rm = TRUE))
    
    frag_plot <-
      plot_spei_heatmap(
        frag_eval,
        fill_lims = fill_lims,
        response_lab = TeX(response_lab)
      )
    cf_plot <-
      plot_spei_heatmap(
        cf_eval,
        fill_lims = fill_lims,
        response_lab = TeX(response_lab)
      )
    
    diff_df <-
      bind_cols(
        cf_eval %>% rename_with(.fn = ~glue("cf_{.}")),
        frag_eval %>% rename_with(.fn = ~glue("frag_{.}"))
      ) %>%
      mutate(est = cf_est - frag_est) %>%
      select(est, L = cf_L, spei_history = cf_spei_history)
    
    diff_plot <-
      plot_spei_heatmap(
        diff_df,
        fill_lims = NULL,
        response_lab = "",
        ci = FALSE
      ) +
      scale_fill_gradient2(TeX(glue::glue("$\\Delta${response_lab} (CF-1ha)")),
                           low = "#5E3C99", high = "#E66101") #from colorbrewer 5-class PuOr
    cf_plot / 
      (frag_plot + theme(legend.position = "none")) /
      diff_plot + plot_layout(guides = "keep") &
      theme(legend.justification = "left") &
      plot_annotation(tag_levels = "a", tag_suffix = ")") &
      theme(legend.justification = "left")
  }












#old
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
plot_spei_heatmap_contour <-
  function(eval_df,
           fill_lims,
           binwidth,
           response_lab,
           breaks = seq(0, 36, by = 2)) {
    
    season_bar <- make_season_bar()
    
    ggplot(eval_df, aes_string(y = "spei_history", x = "L")) +
      geom_raster(aes_string(fill = "est")) +
      geom_contour(aes_string(z = "est"), color = "black", binwidth = binwidth, size = 0.3) +
      geom_hline(aes(yintercept = 0), color = "grey", linetype = 2) +
      scale_fill_viridis_c(response_lab, option = "viridis", limits = fill_lims) +
      scale_x_continuous("lag (months before census)",
                         breaks = breaks,
                         expand = c(0, 0)) +
      scale_y_continuous(TeX("SPEI_3_"), expand = expansion(mult = c(0.025, 0))) + #leave room for season bar at bottom
      theme_classic() +
      annotation_custom(
        grob = ggplotGrob(season_bar),
        ymin = -Inf,
        ymax = min(eval_df$spei_history),
        xmin = -Inf,
        xmax = Inf
      )
  }
