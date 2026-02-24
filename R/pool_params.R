#' Pool distribution parameters
#'
#' @param n      Numeric vector of length 2 giving sample sizes.
#' @param params List of length 2; each element must contain `mean` and `sd`.
#'
#' @description
#' Pools mean and standard deviation across two samples using a
#' weighted mean and pooled standard deviation (equal-variance assumption).
#'
#' @return
#' A list of length 2, each element containing the pooled `mean` and `sd`.

pool_params <- function(n, params) {
  n1 <- as.integer(n[1]); n2 <- as.integer(n[2])
  stopifnot(length(n) == 2, n1 >= 2, n2 >= 2)
  
  n_tot <- n1 + n2
  mu0 <- (n1 * params[[1]]$mean + n2 * params[[2]]$mean) / n_tot
  
  sd0 <- sqrt(
    ((n1 - 1) * params[[1]]$sd^2 + (n2 - 1) * params[[2]]$sd^2) / (n_tot - 2)
  )
  
  list(mean = mu0, sd = sd0)
}
