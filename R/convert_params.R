#' convert_params
#'
#' @param   n       number of samples. This should have two elements
#' @param   dist    The distribution family
#' @param   params  2 level list with mu and sd for both modes or samples
#'
#' @description
#' This function takes the list of mean and SD parameters, as well as the distribution ution family,
#' log-normal, gaussian, gamma, beta, or Weibull; and converts the mean and SD into the respective distribution parameters.
#' 
#'
#' @return         params.transform
#'
#' @import          mixdist
#'
#' @export

convert_params <- function(dist, params) {
  stopifnot(is.list(params), length(params) == 2)
  stopifnot(all(c("mean","sd") %in% names(params[[1]])))
  stopifnot(all(c("mean","sd") %in% names(params[[2]])))
  
  m1 <- params[[1]]$mean; s1 <- params[[1]]$sd
  m2 <- params[[2]]$mean; s2 <- params[[2]]$sd
  
  stopifnot(is.finite(m1), is.finite(m2), is.finite(s1), is.finite(s2))
  stopifnot(s1 > 0, s2 > 0)
  
  params.transformed <- switch(
    dist,
    
    norm = params,
    
    beta = {
      if (!(m1 > 0 && m1 < 1 && m2 > 0 && m2 < 1)) {
        stop("For beta: means must be in (0,1).", call. = FALSE)
      }
      if (!(s1^2 < m1 * (1 - m1) && s2^2 < m2 * (1 - m2))) {
        stop("For beta: sd^2 must be < mean*(1-mean).", call. = FALSE)
      }
      
      v1 <- s1^2
      k1 <- m1 * (1 - m1) / v1 - 1
      shape1_1 <- m1 * k1
      shape2_1 <- (1 - m1) * k1
      
      v2 <- s2^2
      k2 <- m2 * (1 - m2) / v2 - 1
      shape1_2 <- m2 * k2
      shape2_2 <- (1 - m2) * k2
      
      if (any(!is.finite(c(shape1_1, shape2_1, shape1_2, shape2_2))) ||
          any(c(shape1_1, shape2_1, shape1_2, shape2_2) <= 0)) {
        stop("For beta: derived shape parameters must be positive and finite.", call. = FALSE)
      }
      
      list(
        list(shape1 = shape1_1, shape2 = shape2_1),
        list(shape1 = shape1_2, shape2 = shape2_2)
      )
    },
    
    gamma = {
      if (!(m1 > 0 && m2 > 0)) stop("For gamma: means must be > 0.", call. = FALSE)
      
      shape_1 <- (m1 / s1)^2
      scale_1 <- s1^2 / m1
      shape_2 <- (m2 / s2)^2
      scale_2 <- s2^2 / m2
      
      list(
        list(shape = shape_1, scale = scale_1),
        list(shape = shape_2, scale = scale_2)
      )
    },
    
    weibull = {
      if (!(m1 > 0 && m2 > 0)) stop("For weibull: means must be > 0.", call. = FALSE)
      
      wp1 <- mixdist::weibullpar(mu = m1, sigma = s1)
      wp2 <- mixdist::weibullpar(mu = m2, sigma = s2)
      
      list(
        list(shape = wp1$shape, scale = wp1$scale),
        list(shape = wp2$shape, scale = wp2$scale)
      )
    },
    
    lnorm = {
      if (!(m1 > 0 && m2 > 0)) stop("For lnorm: means must be > 0.", call. = FALSE)
      
      sdlog_1 <- sqrt(log(1 + (s1^2 / m1^2)))
      meanlog_1 <- log(m1) - 0.5 * sdlog_1^2
      
      sdlog_2 <- sqrt(log(1 + (s2^2 / m2^2)))
      meanlog_2 <- log(m2) - 0.5 * sdlog_2^2
      
      list(
        list(meanlog = meanlog_1, sdlog = sdlog_1),
        list(meanlog = meanlog_2, sdlog = sdlog_2)
      )
    },
    
    stop("Unsupported dist: ", dist, call. = FALSE)
  )
  
  params.transformed
}
