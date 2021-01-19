#' Calculate marginal effects of a crossbasis smooth
#' 
#' Calculate marginal effects of a crossbasis smooth created with the `cb` basis
#' from the `dlnm` package in a model created with `gam()`. Everything is kept
#' average (or at reference value for factors) and response values are predicted
#' using the range of values of Q at each lag defined by L.
#'
#' @param Q The matrix of predictor values used to generate a crossbasis smooth
#' @param L The matrix of lags used to generate a crossbasis smooth
#' @param model a gam with a crossbasis smooth
#' @param ref_data reference data passed to the `newdata` argument of `predict`
#'   (optional). This one-row data frame should contain values for all model
#'   terms **except** `Q` and `L`.
#' @param meshpts vector of length 2; The number of meshpoints for values of Q
#'   and L, respectively, to use to generate fitted values
#' @param calc_dist logical; Calculate distance between predicted values and
#'   actual data points?  This is inspired by the `too.far` argument of
#'   `plot.gam()`.  If `TRUE` (default), it adds the column `min_dist` which is
#'   useful for filtering data before plotting because "smooths tend to go wild
#'   away from data". This step takes a long time, so if you don't need it,
#'   consider setting to `FALSE`
#' @param ... Other arguments passed to `predict()`, for example, in the case of
#'   a model created with `bam()`, the `cluster` argument.  Use with caution.
#' @return a tibble suitable for plotting marginal effects as a heatmap or
#'   contour plot.  `x` is the meshpoint values across the range of the
#'   predictor, `Q`; `lag` is the values of `L`; `fitted` and `se.fit` are the
#'   results of `predict.gam()`; min_dist is the euclidean distance on the unit
#'   square from the fitted values to the actual data used to fit the model.
#' @export
#' @importFrom rlang enquo abort
#' @import purrr
#' @import dplyr
#' @import mgcv
#' @import tidyr
#' @importFrom stats predict
#'
#' @examples
#' \dontrun{
#' library(dlnm)
#' library(mgcv)
#' library(tsModel)
#' data("chicagoNMMAPS")
#' Q <- Lag(chicagoNMMAPS$temp, 0:25) #temperature data, lagged
#' L <- matrix(0:25,nrow(Q),ncol(Q),byrow=TRUE) #matrix of 0-25
#' # Fit DLNM model
#' gam1 <- gam(death ~ s(Q, L, bs="cb", k=10) + s(pm10) + dow,
#'             family=quasipoisson(), 
#'             data = chicagoNMMAPS,
#'             method='REML')
#' # Calculate marginal effect of lagged temperature, all else being held average.
#' cb_margeff(Q, L, gam1)
#' }
cb_margeff <- 
  function(model, Q, L, ref_data = NULL, meshpts = c(50, 50), calc_dist = TRUE, ...) {
    # Q_name <- quo(Q)
    # L_name <- quo(L)
    
    if (!inherits(model, c("gam", "bam"))) {
      abort("This is only for GAMs made with the `mgcv` package including cross-basis smooths from the `dlnm` package.")
    } 
    
    Q_name <- rlang::enquo(Q)
    L_name <- rlang::enquo(L)
    df <- model$model
    
    # Get the Q and L matrices from the model dataframe
    Q <- dplyr::pull(df, !!Q_name)
    L <- dplyr::pull(df, !!L_name)
    
    testvals <- seq(min(Q, na.rm = TRUE), max(Q, na.rm = TRUE), length.out = meshpts[1])
    Q_new <- matrix(mean(Q, na.rm = TRUE), nrow = meshpts[1], ncol = meshpts[2])
    lvals <- seq(min(L), max(L), length.out = meshpts[2])
    L_new <- matrix(lvals, nrow = meshpts[1], ncol = meshpts[2], byrow = TRUE)
    
    # For newdata, keep everything constant except varying Q.
    # Keep numeric values constant at mean.
    # Set random effects to a new level to "trick" predict().
    # Set parametric factors to reference level.
    terms_raneff <-
      model$smooth %>% 
      purrr::map_if(~inherits(.x, "random.effect"),
                    ~pluck(.x, "term"),
                    .else = function(x) return(NULL)) %>%
      purrr::compact() %>% 
      purrr::as_vector()
    
    terms_fac <- names(model$xlevels)
    
    
    if (is.null(ref_data)) {
      #TODO newdata columns must be the same class as the model data.  I think this
      #breaks if there is a fixed-effect factor put in as a character vector.
      ref_data <-
        df %>%
        dplyr::summarize(
          across(c(-!!L_name, -!!Q_name) & where(is.numeric), mean),
          across(all_of(terms_raneff) & where(is.factor), ~factor(".newdata")),
          across(all_of(terms_fac) & where(is.factor), ~factor(levels(.x)[1], levels = levels(.x)))
        )
    }
    newdata <- uncount(ref_data, meshpts[1]) %>% add_column(!!L_name := L_new)
    resp <- array(dim = c(length(testvals), ncol(Q_new)))
    rownames(resp) <- testvals
    colnames(resp) <- lvals
    se <- resp
    #loop through columns of matrix representing different lags/distances, replace
    #with testvals, predict response.
    for (i in 1:ncol(Q_new)) {
      # is there some way I can use outer() or rbind() to just make one big matrix instead of this loop?
      P1_i <- Q_new
      P1_i[, i] <- testvals
      p <- suppressWarnings( #new levels of random effects are on purpose
        predict(
          model,
          newdata = newdata %>% add_column(!!Q_name := P1_i),
          se.fit = TRUE,
          type = "link",
          ...
        )
      )
      resp[, i] <- p$fit
      se[, i] <- p$se.fit
    }
    fitted <-
      resp %>%
      dplyr::as_tibble(rownames = "x", .name_repair = "unique") %>%
      tidyr::pivot_longer(
        cols = -x,
        names_to = "lag",
        values_to = "fitted"
      ) %>%
      dplyr::mutate(lag = as.double(lag), x = as.double(x))
    
    se.fitted <-
      se %>%
      dplyr::as_tibble(rownames = "x", .name_repair = "unique") %>%
      tidyr::pivot_longer(
        cols = -x,
        names_to = "lag",
        values_to = "se.fit"
      ) %>%
      dplyr::mutate(lag = as.double(lag), x = as.double(x))
    
    pred <- dplyr::full_join(fitted, se.fitted, by = c("x", "lag"))
    if (isTRUE(calc_dist)) {
      out <- add_min_dist(df, Q_name, L_name, pred)
    } else {
      out <- pred
    }
    return(out)
  }

#' Calculate distance between predicted values and actual data points on a grid
#' 
#' This is inspired by the `too.far` argument in `plot.gam()`.  It takes
#' predicted values and adds the distance to the model data.  You can then use
#' the `min_dist` column to filter data for plotting.
#'
#' @param df data frame; model data
#' @param Q_name quosure; the name for the Q matrix
#' @param L_name quosure; the name for the L matrix
#' @param pred data frame; the predicted values
#' @import purrr
#' 
#'
#' @return a tibble
add_min_dist <- function(df, Q_name, L_name, pred) {
  d <-
    df %>%
    pull(!!Q_name) %>% 
    as_tibble(.name_repair = ~as.character(pull(df, !!L_name)[1, ])) %>% 
    pivot_longer(everything(),
                 names_to = "lag",
                 names_transform = list(lag = as.numeric),
                 values_to = "x")
  
  grid <-
    pred %>% 
    mutate(min_g_x = min(.data$x, na.rm = TRUE),
           min_g_y = min(.data$lag, na.rm = TRUE),
           g_x = .data$x - .data$min_g_x,
           g_y = .data$lag - .data$min_g_y) %>% 
    mutate(max_g_x = max(.data$g_x, na.rm = TRUE),
           max_g_y = max(.data$g_y, na.rm = TRUE),
           g_x = .data$g_x / .data$max_g_x,
           g_y = .data$g_y / .data$max_g_y)
  
  d <-
    d %>% 
    mutate(d_x = (.data$x - first(grid$min_g_x)) / first(grid$max_g_x),
           d_y = (.data$lag - first(grid$min_g_y)) / first(grid$max_g_y))
  
  #where dat is a 2-column matrix of x and y coords of the true data used to build the model
  min_dist <- function(g_x, g_y, d) {
    d[,1] <- d[,1] - g_x
    d[,2] <- d[,2] - g_y
    min(
      sqrt(
        (d[,1]^2 + d[,2]^2)
      ), na.rm = TRUE
    )
  }
  
  #I think this is the super slow step.  Would be great if it didn't have to be rowwise().
  out <-
    grid %>% 
    rowwise() %>% 
    mutate(min_dist = min_dist(g_x, g_y, cbind(d$d_x, d$d_y))) %>% 
    select(-g_x, -g_y, -max_g_x, -max_g_y, -min_g_x, -min_g_y) %>% 
    ungroup()
  
  return(out)
}
