#' Make Projection Matrix K
#'
#' @param m number of mesh points
#' @param L lower boundary
#' @param U upper boundary
#' @param P_z1z kernel function for growth and survival
#' @param F_ziz kernel function for fecundity
#' 
#' @return a list with elements K, P, F, and meshpts
#' @export
#'
#' @examples
#' \dontrun{
#' mk_K(m = 100, L = 2.4, U = 200.2)
#' }
mk_K <- function(P_z1z, F_z1z, m = 100, L, U) {
  # mesh points 
  h <- (U - L)/m
  meshpts <- L + ((1:m) - 1/2) * h
  P <- h * (outer(meshpts, meshpts, P_z1z))
  F <- h * (outer(meshpts, meshpts, F_z1z))
  K <- P + F
  return(list(K = K, meshpts = meshpts, P = P, F = F))
}