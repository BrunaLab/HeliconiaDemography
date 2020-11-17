#' Matrix iteration operator
#' 
#' A wrapper around [matrixcalc::matrix.power]. A%^%k multiplies matrix A by itself k times.
#' 
#' @rdname matrix_power_operator
#' @param x a matrix
#' @param k a power
#' 
#' @importFrom matrixcalc matrix.power
#' @return a matrix
#' @export
#'
#' @examples
#' A <- matrix(1:9, nrow = 3)
#' B <- A %^% 3
#' B == A %*% A %*% A
`%^%` <- function(x, k) {
  matrixcalc::matrix.power(x, k)
}


#' Calculate euclidean distance between two points
#'
#' @param p1 numeric vector of length 2; x and y coordinates of point 1
#' @param p2 numeric vector of length 2; x and y coordinates of point 2
#'
#' @return Euclidean distance between the points
#' @export
#'
#' @examples
#' x = c(0,0)
#' y = c(3,3)
#' eu_dist(x, y)
eu_dist <- function(p1, p2) {
  sqrt(sum((p1 - p2)^2))
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


#' Add SPI or SPEI indicators to a ggplot
#'
#' @param p a ggplot object
#' @param ... other arguments passed to [annotate()], e.g. `alpha`
#'
#' @return a ggplot object
#' @export
#'
#' @examples
annotate_spei <- function(p, ...) {
  ymin <- min(ggplot_build(p)$layout$panel_scales_y[[1]]$range$range[1], -2.5)
  spei_rects <- list(
    annotate(geom = "rect", xmin = -Inf, xmax = Inf, ymin = ymin, ymax = -2, fill = "#e31a1c", ...),
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
