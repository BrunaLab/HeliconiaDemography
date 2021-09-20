#' Add SPI or SPEI indicators to a ggplot
#'
#' @param p a ggplot object
#' @param ... other arguments passed to [annotate()], e.g. `alpha`
#' @return a ggplot object
#' @import ggplot2
#' @export
#'
#' @examples
#' \dontrun{
#' p #a ggplot object plotting SPEI over time
#' annotate_spei(p)
#' }
annotate_spei <- function(p, ...) {
  spei_rects <- list(
    annotate(geom = "rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = -2, fill = "#e31a1c", ...),
    annotate(geom = "rect", xmin = -Inf, xmax = Inf, ymin = -2, ymax = -1.5, fill = "#fd8d3c", ...),
    annotate(geom = "rect", xmin = -Inf, xmax = Inf, ymin = -1.5, ymax = -1, fill = "#fecc5c", ...),
    annotate(geom = "rect", xmin = -Inf, xmax = Inf, ymin = -1, ymax = 0, fill = "#ffffb2", ...) 
  )
  #add rectangles
  P2 <- p + spei_rects
  #now, move the last 4 layers to the bottom
  nlayers <- length(P2$layers)
  P2$layers <- c(P2$layers[(nlayers-3):nlayers], P2$layers[1:(nlayers-4)])
  return(P2)
}



#' Which element(s) of a vector is (are) nearest to some value?
#' 
#' For use in dplyr::filter() to get all the rows with the value closest to some target value.
#'
#' @param x a numeric vector
#' @param val a numeric scalar
#' @seealso dplyr::near()
#'
#' @return a logical vector
#' @export
#'
#' @examples
#' x <- seq(1, 10, 0.3)
#' nearest(x, 5)
nearest <- function(x, val) {
  abs(x - val) == min(abs(x - val))
}



#' Check crossbasis for adequate knots
#'
#' Implements a strategy described in detail in `?mgcv::choose.k` for checking
#' if there are adequate knots.  Basically looking for any pattern in residuals
#'
#' @param model a gam with a crossbasis smooth described by `s(spei_history, L,
#'   bs = "cb")`
#'   
check_res_edf <- function(model) {
  res <- residuals(model)
  mgcv::gam(res ~ te(spei_history, L, k = c(20, 35), bs = "cs"),
      gamma = 1.4,
      data = model.frame(model)) %>%
    gratia::edf() %>% 
    dplyr::mutate(edf = round(edf, 1))
}
