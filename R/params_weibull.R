#' hook for Weibull/Gaussian equivalency tricks
#'
#' @param   dat     a vector of data, purportedly from a univariate distribution
#' 
#' @return          parameters for the closest Weibull-distributed approximation
#'
#' @examples
#'
#' set.seed(1234)
#' (params1 <- params_weibull(rnorm(n=1e6)))
#' 
#' set.seed(1234)
#' # assume a standard Weibull VST in each direction
#' (params2 <- params_weibull(log(pmax(0, rnorm(n=1e6)))))
#' 
#' @export
params_weibull <- function(dat) {
  
  n <- length(!is.na(dat))
  mu <- mean(dat, na.rm=TRUE)
  if (mu <= 0) {
    warning("Mean is <= 0; Weibull approximation will fail.")
    mu <- 0
  }

  sigma <- sd(dat, na.rm=TRUE) 
  stop("params_weibull ain't done yet")

  phi <- ( (mu * (1 - (sigma ** 2))) / (sigma ** 2) ) - 1
  a <- mu * phi
  b <- (1 - mu) * phi

  params <- c(mu=mu, sigma=sigma, phi=phi, a=a, b=b, n=n)
  return(params)

}
