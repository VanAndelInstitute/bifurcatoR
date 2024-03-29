% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parameters_weibull.R
\name{parameters_weibull}
\alias{parameters_weibull}
\title{hook for Weibull/Gaussian equivalency tricks (and traps)}
\usage{
parameters_weibull(dat, ...)
}
\arguments{
\item{dat}{a vector of data, purportedly from a Weibull distribution}

\item{...}{additional arguments to be passed on to MASS::fitdistr}
}
\value{
\if{html}{\out{<div class="sourceCode">}}\preformatted{     an object of class `fitdistr` (see MASS::fitdistr for more)
}\if{html}{\out{</div>}}
}
\description{
hook for Weibull/Gaussian equivalency tricks (and traps)
}
\details{
The canonical Weibull distribution is expressed as

X ~ Weibull(a, b)

where a > 0 is the scale parameter and b > 0 is the shape parameter.
This function calls MASS::fitdistr(dat) to estimate these parameters and
packages up the result as a list (see MASS::fitdistr for more possibilities).
The starting parameters for MASS::fitdistr are computed via the methods of
moments, and the final estimates are then fitted by maximum likelihood. The
code for the method of moments estimate is modified from EnvStats::eweibull.
}
\examples{

set.seed(1234)
dat <- rnorm(n=1000, mean=6)
stopifnot(all(dat > 0)) # fitdistr fails if any(dat <= 0)
parameters_weibull(dat)

}
\seealso{
\if{html}{\out{<div class="sourceCode">}}\preformatted{    EnvStats::eweibull
}\if{html}{\out{</div>}}

\if{html}{\out{<div class="sourceCode">}}\preformatted{    MASS::fitdistr
}\if{html}{\out{</div>}}
}
