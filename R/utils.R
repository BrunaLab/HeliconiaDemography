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
