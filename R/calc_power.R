#' calculate power 
#' 
#' @param   n     number of [something] 
#' @param   p1    pr(event) in subgroup 1
#' @param   p2    pr(event) in subgroup 2
#' @param   shift scaled distance between means
#' @param   sel   column to select 
#' @param   CI    generate a CI (?)
#' @param   alpha default significance level (0.05)
#' @param   nsim  number of simulations (100) 
#' 
#' @import  dplyr
#' @importFrom stats ks.test
#' 
#' @export 
calc_power = function(n, p1, p2, shift, sel, CI, alpha=0.05, nsim=100) {
  Freq = pwr.2p.test(n = n, ES.h(p1 = p1, p2 = p2), sig.level = alpha)$power
  
  delta = case_when(
    sel == "Bodyweight" ~ (1 + shift / 100) * 1.313 - 1.313,
    
    sel == "Fat NNAT" ~ (1 + shift / 100) * 12.32 - 12.32,
    
    sel == "Fat Trim" ~ (1 + shift / 100) * 12.32 - 12.32
  )
  
  sd = case_when(
    sel == "Bodyweight" ~ (
      round(p1 * n, 0) * (0.08474 + qnorm(1 - (1 - CI / 100) / 2) * 0.0062) +
        round(p2 * n, 0) * (0.05196 + qnorm(1 - (1 - CI /
                                                   100) / 2) * 0.0139)
    ) / (round(p1 * n, 0) + round(p2 * n, 0)),
    
    sel == "Fat NNAT" ~ (
      round(p1 * n, 0) * (2 + qnorm(1 - (1 - CI / 100) / 2) * .9317) +
        round(p2 * n, 0) * (2 + qnorm(1 - (1 - CI / 100) /
                                        2) * 0.9524)
    ) / (round(p1 * n, 0) + round(p2 * n, 0)),
    
    sel == "Fat Trim" ~ (
      round(p1 * n, 0) * (2 + qnorm(1 - (1 - CI / 100) / 2) * .505) +
        round(p2 * n, 0) * (2.764 + qnorm(1 - (1 - CI /
                                                 100) / 2) * 1.231)
    ) / (round(p1 * n, 0) + round(p2 * n, 0))
  )
  Shift = pwr.t2n.test(
    n1 = max(floor(.95 * p1 * n), 2),
    n2 = max(floor(.95 * p2 * n), 2),
    d = delta / sd,
    sig.level = alpha
  )$power
  
  
  KS = sum(as.numeric(unlist(sapply(1:nsim, function(x) {
    tmp = rmixnorm(n, p1, p2, shift, sel, CI)
    I(ks.test(tmp$values[tmp$Group == "Reference"], tmp$values[tmp$Group == "Experimental"])$p.value < 0.05)
  })))) / nsim
  
  
  return(data.frame(
    N = n,
    Test = c("Frequency", "Shift", "KS"),
    Power = c(Freq, Shift, KS)
  ))
  
}
