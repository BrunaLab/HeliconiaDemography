get_plot_daterange <- function(demog_post, maxlag) {
  dates <-
    range(demog_post$year) %>%
    paste0(., "-02-01") %>%
    ymd()
  dates[1] <- dates[1] - months(36)
  dates
}

plot_eda_spei <- function(demog_post, xa, date_lims) {

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
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  spei <- annotate_spei(spei)
  spei
  
}

eda_plot_df <- function(demog_post) {
  demog_plotdf <-  
    demog_post %>% 
    filter(habitat !="10-ha") %>% 
    select(ranch, bdffp_reserve_no, plot, habitat, year, ht, shts, shts_prev, size, surv, flwr) %>% 
    mutate(date = paste(year, "-02-01") %>% ymd(),
           yearmonth = yearmonth(date))
  demog_plotdf
}

plot_eda_surv_ts <- function(demog_post, dates_lims) {
  
  date_breaks <- seq(date_lims[1], date_lims[2], by = "year")
  
  demog_plotdf <- eda_plot_df(demog_post)
  
  survival <-
    demog_plotdf %>%  
    filter(shts_prev >= 4) %>%
    ggplot(aes(x = yearmonth, y = surv, color = habitat)) +
    stat_summary(geom = "line", fun = "mean", fun.args = list(na.rm = TRUE)) +
    stat_summary(geom = "pointrange", fatten = 1) +
    scale_x_yearmonth(limits = date_lims, breaks = date_breaks, date_minor_breaks ="1 month", expand = expansion(mult = 0.02)) +
    scale_y_continuous("P(survived)") +
    theme_bw() +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position = "none"
    )
  survival
}

plot_eda_surv_cohort <- function(demog, date_lims) {
  
  date_breaks <- seq(date_lims[1], date_lims[2], by = "year")
  
  cohort <- demog %>% filter(year == 1998) %>% pull(ha_id_number) %>% unique()
  
  surv_curve_df <-
    demog %>% 
    filter(ha_id_number %in% cohort) %>% 
    filter(surv == 1, habitat != "10-ha") %>% 
    group_by(year, habitat) %>% 
    summarize(n = n()) %>%
    group_by(habitat) %>% 
    mutate(p_surv = n/first(n),
           date = ymd(paste0(year, "-02-01")),
           yearmonth = yearmonth(date))
  
  surv_curve <-
    ggplot(surv_curve_df, aes(x = yearmonth, y = p_surv, color = habitat)) +
    geom_step() +
    scale_x_yearmonth(limits = date_lims, breaks = date_breaks, date_minor_breaks ="1 month", expand = expansion(mult = 0.04)) +
    scale_y_continuous("P(survived)") +
    theme_bw() +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
  surv_curve
}



plot_eda_size <- function(demog_post, date_lims) {
  date_breaks <- seq(date_lims[1], date_lims[2], by = "year")
  demog_plotdf <- eda_plot_df(demog_post)

  size <-
    demog_plotdf %>% 
    ggplot(aes(x = yearmonth, y = log(size), color = habitat)) +
    stat_summary(
      geom = "line",
      fun = "mean",
      fun.args = list(na.rm = TRUE),
      position = position_dodge(width = 100),
      aes(group = habitat)
    )+
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
    theme_bw() +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position = "none"
    ) +
    NULL
}

plot_eda_flwr <- function(demog_post, date_lims) {
  demog_plotdf <- eda_plot_df(demog_post)
  date_breaks <- seq(date_lims[1], date_lims[2], by = "year")
  
   flowering_df <-
    demog_plotdf %>% 
    filter(shts >= 4) %>% 
    group_by(yearmonth, habitat) %>% 
    summarize(n = n(), flwr = sum(flwr == 1, na.rm = TRUE)) %>% 
    mutate(prop_flwr = flwr/n)
  
  flowering <-
    ggplot(flowering_df, aes(x = yearmonth, y = prop_flwr, color = habitat)) + 
    geom_line() +
    scale_x_yearmonth(
      limits = date_lims,
      breaks = date_breaks,
      date_minor_breaks = "1 month",
      expand = expansion(mult = 0.04)
    ) +
    scale_y_continuous("P(flowering)") +
    theme_bw() +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
  flowering
}

plot_eda_combine <- function(...) {
  p <- wrap_plots(..., ncol = 1) +
    plot_layout(guides = "collect") &
    plot_annotation(tag_levels = "a", tag_suffix = ")") &
    theme(plot.margin = margin(2,1,1,1)) &
    guides(col = guide_legend(title = "Habitat"))
  return(p)
}
