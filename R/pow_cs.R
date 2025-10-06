#' calculate power several times
#' 
#' @param   ns    numbers of somethings
#' @param   p1    pr(event) in subgroup 1
#' @param   p2    pr(event) in subgroup 2
#' @param   shift scaled distance between means
#' @param   sel   column to select 
#' @param   CI    generate a CI (?)
#' @param   alpha default significance level (0.05)
#' @param   nsim  number of simulations (20) 
#'
#' @return        a data.frame 
#' 
#' @examples
#' pow_cs(ns=c(40,50), p1=.25, p2=.5, shift=15, sel='Bodyweight', CI=0)
#'
#' @import  dplyr
#' 
#' @export 
pow_cs <-  function(ns, p1, p2, shift, sel, CI, alpha=0.05, nsim=20) {

  return(rbindlist(lapply(seq(ns[1], ns[2], 5), function(y)
    calc_power(y, p1, p2, shift, sel, CI, alpha, nsim))))

}
