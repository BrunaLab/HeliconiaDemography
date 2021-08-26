#' Get range for x axis limits for plots
#' 
#' The range is the first date of the survey minus the maximum lag for the
#' models to the last date of the survey data.
#'
#' @param demog_post 
#' @param maxlag 
#'
get_plot_daterange <- function(demog_post, maxlag) {
  dates <-
    range(demog_post$year) %>%
    paste0(., "-02-01") %>%
    ymd()
  dates[1] <- dates[1] - months(maxlag)
  dates
}

#' Plot SPEI
#'
#' @param xa SPEI data from Xavier et al.
#' @param date_lims x-axis limits
#'
#' @return
#' @export
#'
#' @examples
plot_eda_spei <- function(xa, date_lims) {

  date_breaks <- seq(date_lims[1], date_lims[2], by = "year")
  
  spei <-
    xa %>%
    select(latlon, yearmonth, spei) %>%
    filter(!is.na(spei)) %>% 
    ggplot(aes(x = yearmonth, y = spei)) + 
    geom_line(aes(group = latlon), alpha = 0.2) +
    stat_summary(geom = "line", fun = "mean") +
    scale_y_continuous("SPEI", expand = expansion(mult = c(0, 0.05), add = 0)) +
    scale_x_yearmonth("Date", limits = date_lims, breaks = date_breaks, date_minor_breaks ="1 month", expand = expansion(mult = 0.02)) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  spei <- annotate_spei(spei)
  spei
  
}

#' Create data frame for plotting demographic data
#'
#' @param demog_post demography data filtered to only have post-seedlings
#'
eda_plot_df <- function(demog_post) {
  demog_plotdf <-  
    demog_post %>% 
    filter(habitat !="10-ha") %>% 
    mutate(date = paste(year, "-02-01") %>% ymd(),
           yearmonth = yearmonth(date))
  demog_plotdf
}

#' Survival time series plot
#' 
#' Plots mean survival rate over time in both habitats
#'
#' @param demog_post demography data
#' @param dates_lims x-axis limits
#'
plot_eda_surv_ts <- function(demog_post, date_lims) {
  
  date_breaks <- seq(date_lims[1], date_lims[2], by = "year")
  
  demog_plotdf <- eda_plot_df(demog_post)
  
  survival <-
    demog_plotdf %>%  
    # filter(shts_prev >= 4) %>%
    ggplot(aes(x = yearmonth, y = surv, color = habitat, linetype = habitat)) +
    stat_summary(geom = "line", fun = "mean", fun.args = list(na.rm = TRUE)) +
    # stat_summary(geom = "pointrange", fatten = 1) + #not sure plot-level SD makes sense
    scale_x_yearmonth(
      limits = date_lims,
      breaks = date_breaks,
      date_minor_breaks = "1 month",
      expand = expansion(mult = 0.02)
    ) +
    scale_y_continuous("P(survived)") +
    theme_classic() +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "none"
    )
  survival
}

#' Survival curves plots
#' 
#' plots survival curves for the 1998 cohort of plants
#'
#' @param demog demography data
#' @param date_lims x-axis limits
#'
plot_eda_surv_cohort <- function(demog) {
  
  cohort <-
    demog %>% 
    filter(year == 1998) %>% 
    pull(ha_id_number) %>% 
    unique()
  
  surv_curve_df <-
    demog %>% 
    filter(ha_id_number %in% cohort) %>% 
    filter(surv == 1, habitat != "10-ha") %>% 
    group_by(year, habitat) %>% 
    summarize(n = n()) %>%
    group_by(habitat) %>% 
    mutate(p_surv = n/first(n)*100,
           date = ymd(paste0(year, "-02-01")),
           yearmonth = yearmonth(date))
  
  surv_curve <-
    ggplot(surv_curve_df, aes(x = date, y = p_surv, color = habitat, linetype = habitat)) +
    geom_step() +
    scale_x_date(
      "Census Year",
      date_breaks = "1 year",
      date_labels = "%Y",
      date_minor_breaks = "1 month",
      expand = expansion(mult = 0.04)
    ) +
    scale_y_continuous("% surviving") +
    scale_color_manual(values = c("#E66101", "#5E3C99"),
                        aesthetics = c("color", "fill")) +
    guides(col = guide_legend(title = "Habitat"),
           linetype = guide_legend(title = "Habitat")) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  surv_curve
}

plot_eda_size_foldchange <- function(demog_post, date_lims) {
  date_breaks <- seq(date_lims[1], date_lims[2], by = "year")
  demog_plotdf <- eda_plot_df(demog_post) %>% mutate(log2_growth = log(size / size_prev, 2))
  
  size <-
    demog_plotdf %>% 
    ggplot(aes(x = yearmonth, y = log2_growth, color = habitat, linetype = habitat)) +
    geom_hline(yintercept = 0, color = "grey50") +
    stat_summary(
      geom = "line",
      fun = "mean",
      fun.args = list(na.rm = TRUE),
      position = position_dodge(width = 100),
      aes(group = habitat)
    ) +
    stat_summary(
      geom = "pointrange",
      fatten = 1,
      fun.data = "mean_sdl",
      fun.args = list(mult = 1),
      position = position_dodge(width = 100)
    ) +
    scale_x_yearmonth(
      breaks = date_breaks,
      date_minor_breaks = "1 month",
      expand = expansion(mult = 0.04)
    ) +
    scale_y_continuous("fold-change in size") +
    coord_cartesian(xlim = date_lims) + #limits here so points don't get removed
    theme_classic() +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "none"
    )
  size
}

#' Mean size timeseries plot
#'
#' @param demog_post demography data
#' @param date_lims x-axis limits
#'
plot_eda_size <- function(demog_post, date_lims) {
  date_breaks <- seq(date_lims[1], date_lims[2], by = "year")
  demog_plotdf <- eda_plot_df(demog_post)

  size <-
    demog_plotdf %>% 
    ggplot(aes(x = yearmonth, y = log(size), color = habitat, linetype = habitat)) +
    stat_summary(
      geom = "line",
      fun = "mean",
      fun.args = list(na.rm = TRUE),
      position = position_dodge(width = 100),
      aes(group = habitat)
    ) +
    stat_summary(
      geom = "pointrange",
      fatten = 1,
      fun.data = "mean_sdl",
      fun.args = list(mult = 1),
      position = position_dodge(width = 100)
    ) +
    scale_x_yearmonth(
      breaks = date_breaks,
      date_minor_breaks = "1 month",
      expand = expansion(mult = 0.04)
    ) +
    scale_y_continuous("log(size)") +
    coord_cartesian(xlim = date_lims) + #limits here so points don't get removed
    theme_classic() +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "none"
    )
  size
}

#' Flowering rate plot
#' 
#' plots proportion of plants flowering over time
#'
#' @param data demography data
#' @param date_lims x-axis limits
#' @param repro_size a size cutoff for plants that are considered reproductive.
#'  The default, 165, corresponds to the upper 90th percentile of the size of
#'  all flowering plants in the dataset.
#'  
plot_eda_flwr <- function(data, date_lims, repro_size = 165) {
  demog_plotdf <- eda_plot_df(data)
  date_breaks <- seq(date_lims[1], date_lims[2], by = "year")
  
   flowering_df <-
    demog_plotdf %>% 
    filter(size >= repro_size) %>% 
    group_by(yearmonth, habitat) %>% 
    summarize(n = n(), flwr = sum(flwr == 1, na.rm = TRUE)) %>% 
    mutate(prop_flwr = flwr/n)
  
  flowering <-
    ggplot(flowering_df, aes(x = yearmonth, y = prop_flwr, color = habitat, linetype = habitat)) + 
    geom_line() +
    scale_x_yearmonth(
      limits = date_lims,
      breaks = date_breaks,
      date_minor_breaks = "1 month",
      expand = expansion(mult = 0.04)
    ) +
    scale_y_continuous("P(flowering)") +
    theme_classic() +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  flowering
}

#' Combine exploratory data analysis plots
#'
#' @param ... The plots to combine
#' 
#' @import patchwork
#' @import ggplot2
#'
plot_eda_combine <- function(...) {
  p <- 
    wrap_plots(..., ncol = 1) +
    plot_layout(guides = "collect") &
    plot_annotation(tag_levels = "a", tag_suffix = ")") &
    theme(plot.margin = margin(2,1,1,1)) &
    scale_color_manual(values = c("#E66101", "#5E3C99"),
                       aesthetics = c("color", "fill")) &
    guides(col = guide_legend(title = "Habitat"),
           linetype = guide_legend(title = "Habitat"))
  return(p)
}
