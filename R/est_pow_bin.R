#' est_pow_bin
#' 
#' @param   n     number of somethings
#' @param   p     pr(event)
#' @param   alpha default significance level (0.05)
#' @param   nsim  number of simulations (20) 
#'
#' @return        a power estimate
#' 
#' @export
est_pow_bin = function(n,p,alpha=0.05,nsim=20){
  power = sum(sapply(1:nsim,function(y) as.numeric(I(prop.test(table(factor(rbinom(n,1,p),levels=c(0,1))))$p.value<alpha))))/nsim
  return(round(power,4))
}
