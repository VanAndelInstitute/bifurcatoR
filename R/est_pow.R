#' est_pow
#' 
#' @param   n     number of [somethings] 
#' @param   p     pr(event)
#' @param   alpha default significance level (0.05)
#' @param   x     I have no idea what this does 
#' @param   nsim  number of simulations (20) 
#'
#' @return        a power estimate
#' 
#' @export
est_pow = function(n,p,alpha=0.05,x,nsim=20){
  power = sum(sapply(1:nsim,function(y) as.numeric(I(dip.test( c(rnorm(round(n*p),0,1),rnorm(round(n*abs((1-p))),x,1)))$p.value<alpha))))/nsim
  return(round(power,4))
}
