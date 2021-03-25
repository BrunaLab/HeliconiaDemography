#' Return a tibble of temperature and precipitation normals for Manaus
#' 
#' Temperature and precipitation normals from 1981-2010 for the Manaus INMET weather station. 
#' @source Data from INMET https://portal.inmet.gov.br/normais. 
#' @return a tibble of climate normals
#' @export
#'
#' @examples
normals_data <- function() {
  tibble(
    month = 1:12,
    temp_mean = c(26.30, 26.30, 26.30, 26.40, 26.60, 26.70, 27.00, 27.60, 28.00, 28.00, 27.60, 26.90),
    temp_min  = c(23.10, 23.10, 23.20, 23.20, 23.40, 23.00, 23.10, 23.40, 23.70, 23.90, 23.70, 23.50),
    temp_max  = c(30.90, 30.80, 30.90, 31.00, 31.10, 31.40, 32.10, 33.10, 33.50, 33.40, 32.60, 31.70),
    precip    = c(287.0, 295.1, 300.0, 319.0, 246.9, 118.3, 75.40, 64.30, 76.30, 104.1, 169.2, 245.6)
  )
}


plot_normals <- function(normals) {
  precip <-
    ggplot(normals, aes(x = as.factor(month), y = precip)) + 
    geom_col(fill = "darkblue") +
    scale_y_continuous("Precipitation (mm)", expand = expansion(mult = c(0.05, 0.1))) +
    theme_bw()
  
  temp <- 
    ggplot(normals, aes(x = month)) +
    geom_line(aes(y = temp_mean), color = "red") +
    geom_line(aes(y = temp_min), color = "red", linetype = 2) +
    geom_line(aes(y = temp_max), color = "red", linetype = 2) +
    scale_x_continuous("Month", breaks = 1:12) +
    scale_y_continuous("Temperature (ºC)", expand = expansion(mult = 0.3)) +
    theme_bw()
  
    (precip + theme(axis.title.x = element_blank(), axis.text.x = element_blank()))/
    (temp) + plot_annotation(tag_levels = "a", tag_suffix = ")")
}