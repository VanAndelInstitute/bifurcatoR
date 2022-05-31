#' hook for Weibull/Gaussian equivalency tricks (and traps)
#'
#' The canonical Weibull distribution is expressed as
#' 
#' X ~ Weibull(a, b)
#' 
#' where a > 0 is the scale parameter and b > 0 is the shape parameter.
#' This function calls MASS::fitdistr(dat) to estimate these parameters and 
#' packages up the result as a list (see MASS::fitdistr for more possibilities).
#' The starting parameters for MASS::fitdistr are computed via the methods of 
#' moments, and the final estimates are then fitted by maximum likelihood. The
#' code for the method of moments estimate is modified from EnvStats::eweibull.
#'
#' @param   dat     a vector of data, purportedly from a Weibull distribution
#' @param   ...     additional arguments to be passed on to MASS::fitdistr
#' 
#' @return          an object of class `fitdistr` (see MASS::fitdistr for more)
#' 
#' @examples
#'
#' set.seed(1234)
#' dat <- rnorm(n=1000, mean=6)
#' stopifnot(all(dat > 0)) # fitdistr fails if any(dat <= 0)
#' parameters_weibull(dat)
#' 
#' @seealso         EnvStats::eweibull
#' @seealso         MASS::fitdistr
#' 
#' @importFrom      MASS fitdistr
#'
#' @export
parameters_weibull <- function(dat, ...) {

  dat <- dat[!is.na(dat)]
  n <- length(dat)
  mu <- mean(dat)
  
  if (mu <= 0) warning("Mean is <= 0; Weibull approximation will fail.")
 
  sigma <- sd(dat, na.rm=TRUE) 
  mcf <- function(x, mu, sigma) { # borrowed from EnvStats
    ((sigma / mu) - sqrt((gamma((x + 2) / x) / (gamma((x + 1) / x)^2)) - 1))^2
  }
  lower <- .Machine$double.eps
  phi <- ( (mu * (1 - (sigma ** 2))) / (sigma ** 2) ) - 1
  scale0 <- nlminb(start=1, objective=mcf, lower=lower, mu=mu, sigma=sigma)$par
  shape0 <- mu / gamma((scale0 + 1) / scale0)
  fd <- fitdistr(dat, "weibull", start=list(scale=scale0, shape=shape0), ...)
  fd$distribution <- "Weibull"
  return(fd) 

}
