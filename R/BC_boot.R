#' BC_boot
#'
#' @param   x     Generic name for a vector of data to bootstrap sample BCs
#' @param   alpha significnace level
#'
#' @return        bootstrapped p-value
#'
#' @import          mousetrap
#'
#' @export

BC_boot = function(x,nboot){
  tmp.boot = unname(unlist(lapply(1:nboot, function(z) mousetrap::mt_check_bimodality(as.data.frame(x[sample(1:length(x),replace=T)]),method="BC")$BC)))
  return(sum(I(tmp.boot<=0.5555555))/nboot)
}


