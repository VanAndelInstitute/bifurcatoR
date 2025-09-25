#' est_pow_het
#' 
#' @param   n     number of [somethings] 
#' @param   p     pr(event)
#' @param   alpha default significance level (0.05)
#' @param   x     I have no idea what this does 
#' @param   nsim  number of simulations (20) 
#'
#' @return        a power estimate
#' 
#' @importFrom stats lm rnorm
#' 
#' @export
est_pow_het = function(n,p,alpha,x,nsim){
  power = sum(sapply(1:nsim,function(y) as.numeric((I(olsrr::ols_test_f(lm(val~g,data.frame(val = c(rnorm(round(n*p),0,1),rnorm(round(n*abs((1-p))),0,x)),g=c(rep("x",round(n*p)),rep("y",round(n*abs(1-p)))))))$p<alpha)))))/nsim
  return(round(power,4))
}
