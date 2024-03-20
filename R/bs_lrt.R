
#' bootstrap likelihood ratio tests for mixR fits
#'
#' The mixR package provides fast implementations for a number of 1-dimensional
#' mixture models, along with the bs.test function for performing bootstrap
#' likelihood ratio tests of component numbers (default is "one lump or two?").
#' Note that the fitting and bootstrapping of non-normal mixtures can be
#' substantially sped up by binning the data. See mixR::bin for details.
#'
#' @param   x         raw data for a mixR fit (vector or 3-column matrix)
#' @param   H0        the number of components in the null model (1)
#' @param   H1        the number of components in the alternative model (2)
#' @param   family    fit distribution ("normal","weibull","gamma","lnorm")

#' @param   nboot     number of bootstraps to perform (100)
#' @param   iter      maximum iterations for EM algorithm (1000)
#' @param   ...       additional arguments, passed to mixR::bs.test()
#'
#' @return  an object of class "bootEM" with three items (see mixR::bs.test)
#'
#' @seealso mixR::bin
#' @seealso mixR::mixfit
#' @seealso mixR::bs.test
#' @seealso mixR
#'
#' 

#' @import mixR
#'
#' @export
bs_lrt <- function(x, H0=1, H1=2, family="normal", nboot=1e2, iter=1e3, ...){

  # check input
  if(!is.numeric(H0) || H0 < 1) stop("H0 must be a positive integer.")
  if(!is.numeric(H1) || H1 < H0) stop("H1 must be an integer greater than H0.")

  # perform the bootstrap test
  bs.test(x, ncomp=c(H0, H1), family=family, B=nboot, max_iter=iter, ...)

}
