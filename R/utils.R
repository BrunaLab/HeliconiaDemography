#' Matrix iteration operator
#'
#' @param x a matrix
#' @param k a power
#' 
#' @importFrom matrixcalc matrix.power
#' @return
#' @export
#'
#' @examples
#' A <- matrix(1:9, nrow = 3)
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

