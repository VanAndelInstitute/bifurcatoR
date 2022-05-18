#' hook for beta/Gaussian equivalency tricks
#'
#' @param   dat     a vector of data, purportedly from a univariate distribution
#' 
#' @return          parameters for the closest beta-distributed approximation
#'
#' @examples
#'
#' set.seed(1234)
#' (params1 <- params_beta(rnorm(n=1e6)))
#' 
#' set.seed(1234)
#' # assume a standard beta VST in each direction
#' (params2 <- params_beta(bifurcatoR:::.expit(rnorm(n=1e6))))
#' 
#' @export
params_beta <- function(dat) {
  
  n <- length(!is.na(dat))
  mu <- mean(dat, na.rm=TRUE)
  if (mu <= 0) {
    warning("Mean is <= 0; beta approximation will fail.")
    mu <- 0
  }

  sigma <- sd(dat, na.rm=TRUE) 
  phi <- ( (mu * (1 - (sigma ** 2))) / (sigma ** 2) ) - 1
  a <- mu * phi
  b <- (1 - mu) * phi

  params <- c(mu=mu, sigma=sigma, phi=phi, a=a, b=b, n=n)
  return(params)

}


# helper fn
.expit <- function(x, sqz=0) { 

  p <- exp(x) / (1 + exp(x))
  if (sqz > 0) p <- (((((p * 2) - 1) / (1 - sqz)) + 1) / 2)
  return(p)

} 


# helper fn
.logit <- function(p, sqz=0) { 

  if (sqz > 0) {
    p[ which(p < sqz) ] <- sqz 
    p[ which(p > (1 - sqz)) ] <- (1 - sqz)
  }
  x <- log(p / (1 - p))
  return(x)

}
