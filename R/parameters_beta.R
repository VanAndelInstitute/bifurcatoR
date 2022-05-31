#' hook for beta/Gaussian equivalency tricks
#'
#' The canonical Beta distribution is expressed as
#' 
#' X ~ Beta(a, b)
#' 
#' where a > 0 is a shape parameter and b > 0 is another shape parameter.
#' `a` often represents the number of successes and `b` the number of failures
#' in a series of trials, whereupon the Beta represents the conjugate prior for
#' the binomial distribution where the success probability `p` is (a / (a + b)).
#' The MASS::fitdistr function requires start parameters for a Beta distribution
#' which we compute via the method of moments and then feed to MASS::fitdistr().
#'
#' @param   dat     a vector of data, purportedly from a Beta distribution
#' @param   ...     additional arguments to be passed on to MASS::fitdistr
#' 
#' @return          an object of class `fitdistr` (see MASS::fitdistr for more)
#'
#' @examples
#'
#' set.seed(1234)
#' dat <- rbeta(n=1000, 17, 39)
#' parameters_beta(dat)
#' 
#' @seealso         MASS::fitdistr
#' 
#' @import          MASS
#' 
#' @export
parameters_beta <- function(dat, ...) {
  
  dat <- dat[!is.na(dat)]
  n <- length(dat)
  mu <- mean(dat)

  if (mu <= 0) {
    warning("Mean is <= 0; beta approximation will fail.")
    mu <- 0
  }

  sigma <- sd(dat)
  phi <- ( (mu * (1 - (sigma ** 2))) / (sigma ** 2) ) - 1
  shape1 <- mu * phi
  shape2 <- (1 - mu) * phi
  fd <- fitdistr(dat, "beta", start=list(shape1=shape1, shape2=shape2), ...)
  fd$distribution <- "Beta"
  return(fd) 

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
