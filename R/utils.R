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


#' Create masking matrices with rectangles
#' 
#' Creates a list of matrices with all 0's except for a rectangle of 1's around
#' a focal "cell". Used for figuring out how many neighboring plants each plant
#' has.
#'
#' @param D distance from focal cell for rectangle
#' @param nrow number of rows of matrix
#' @param ncol number of columns of matrix
#' @author Lyndsay Troyer
#' @return a list of matrices
#' @export
#' 
#' @import glue
#' 
#' @examples
#' I <- mat_rect(D = 1, nrow = 5, ncol = 5)
#' y <- matrix(round(runif(25, 1, 10), 0), nrow = 5, ncol = 5)
#' map_dbl(I, ~ sum(y * .x)) %>%
#'   enframe(value = "n_plants_2") %>%
#'   separate(name, into = c("row", "column"), sep = ",", convert = TRUE)
mat_rect <- function(D, nrow, ncol) {
  mlist <- vector("list", 1)
  for(r in 1:nrow) {
    for(c in 1:ncol) {
      m <- matrix(0, ncol = ncol, nrow = nrow)
      r1 <- r + D
      r1 <- r1[r1 <= nrow]
      c1 <- c + (-D:D)
      c1 <- c1[c1 > 0 & c1 <= ncol]
      m[r1, c1] <- 1
      
      r2 <- r - D
      r2 <- r2[r2 > 0]
      c2 <- c - (-D:D)
      c2 <- c2[c2 > 0 & c2 <= ncol]
      m[r2, c2] <- 1
      
      r3 <- r - (-D:D)
      r3 <- r3[r3 > 0 & r3 <= nrow]
      c3 <- c - D
      c3 <- c3[c3 > 0]
      m[r3, c3] <- 1
      
      r4 <- r + (-D:D)
      r4 <- r4[r4 > 0 & r4 <= nrow]
      c4 <- c + D
      c4 <- c4[c4 <= ncol]
      m[r4, c4] <- 1
      
      m <- list(m)
      names(m) <- glue::glue("{r}, {c}")
      mlist <- c(mlist, m)
    }
  }
  return(mlist[-1])
}


# WIP: function to get data for plotting results from GAM with crossbasis smooth.
# 
# library(rlang)
# pred_cb <- function(Q, L, model) {
#   Q_name <- enquo(Q)
#   L_name <- enquo(L)
#   df <- model$model
#   
#   testvals <- seq(min(Q), max(Q), length.out = 200)
#   Q_new <- matrix(mean(Q), nrow = length(testvals), ncol = lag)
#   L_new <- matrix(1:ncol(L), nrow = nrow(Q_new), ncol = ncol(L), byrow = TRUE)
#   
#   #keep all other numeric terms constant and make new levels for factors
#   #TODO: Figure out how to predict marginal effect with fixed effects.
#   newdata <-
#     df %>% 
#     summarize(
#       across(c(-!!L_name, -!!Q_name) & where(is.numeric), mean),
#       across(where(is.factor), ~factor(".newdata"))
#       # across(where(is.factor), ~factor("1-ha"))
#     ) 
#   newdata <- uncount(newdata, 200) %>% add_column(!!L_name := L_new)
#   
#   resp <- array(dim = c(length(testvals), ncol(Q_new)))
#   rownames(resp) <- testvals
#   
#   #loop through columns of matrix, replace with testvals, predict fitted.
#   for(i in 1:ncol(Q_new)) {
#     P1_i <- Q_new
#     P1_i[, i] <- testvals
#     resp[, i] <-
#       suppressWarnings( #new levels of random effects are on purpose
#         predict(
#           model,
#           newdata = newdata %>% add_column(!!Q_name := P1_i),
#           type = "response"
#         )
#       )
#   }
#   out <-
#     resp %>%
#     as_tibble(rownames = "x") %>%
#     pivot_longer(
#       cols = starts_with("V"),
#       names_to = "lag",
#       names_prefix = "V",
#       values_to = "fitted"
#     ) %>%
#     mutate(lag = as.double(lag), x = as.double(x)) 
#   return(out)
# }
