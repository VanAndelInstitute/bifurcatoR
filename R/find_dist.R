#' find_dist
#' 
#' @param   n1    number of [somethings] in group 1
#' @param   n2    number of [somethings] in group 2 
#' @param   alpha default significance level (0.05)
#' @param   nsim  number of simulations (20) 
#'
#' @return        a data.frame
#' 
#' @importFrom stats rnorm
#' 
#' @export
find_dist = function(n1,n2,alpha,nsim){
  power = sapply(seq(.25,10,.25), function(z)  sum(sapply(1:nsim,function(y) as.numeric(I(dip.test( c(rnorm(n1,0,1),rnorm(n2,z,1)))$p.value<alpha))))/nsim)
  return(data.frame(Distance = seq(.25,10,.25), power ))
}
