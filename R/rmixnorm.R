#' generate a random mixture of normals 
#' 
#' @param   n     number of [something] 
#' @param   p1    pr(event) in subgroup 1
#' @param   p2    pr(event) in subgroup 2
#' @param   shift scaled distance between means
#' @param   sel   column name to select 
#' @param   CI    generate a CI (?)
#' 
#' @details       let's see how dplyr verbs work in production :-O
#' 
#' @return        a data.frame
#' 
#' @examples
#' pps = rmixnorm(20, p1=0.25, p2=0.5, shift=15, sel='Bodyweight', CI=0)
#' 
#' @import        dplyr
#' 
#' @export 
rmixnorm = function(n, p1, p2, shift, sel, CI) {

  #These were coded backwards, putting easy fix here
  p1 = 1 - p1
  p2 = 1 - p2
  ref =   case_when(
    sel == "Bodyweight" ~ c(
      rnorm(round(n * p1, 0), 1.053, 0.08474 + qnorm(1 - (1 - CI / 100) / 2) * 0.0062),
      rnorm(round(n * (1 - p1), 0), 1.313, 0.05196 + qnorm(1 - (1 - CI / 100) /
                                                             2) * 0.0139)
    ),
    
    sel == "Fat NNAT" ~ c(
      rnorm(round(n * p1, 0), 6.204, 2 + qnorm(1 - (1 - CI / 100) / 2) * .9317),
      rnorm(round(n * (1 - p1), 0), 12.32, 2 + qnorm(1 - (1 - CI / 100) / 2) * 0.9524)
    ),
    
    sel == "Fat Trim" ~ c(
      rnorm(round(n * p1, 0), 3.928, 2 + qnorm(1 - (1 - CI / 100) / 2) * .505),
      rnorm(round(n * (1 - p1), 0), 12.32, 2.764 + qnorm(1 - (1 - CI / 100) /
                                                           2) * 1.231)
    )
  )
  exp =   case_when(
    sel == "Bodyweight" ~ c(
      rnorm(round(n * p2, 0), 1.053, 0.08474 + qnorm(1 - (1 - CI / 100) / 2) * 0.0062),
      rnorm(
        round(n * (1 - p2), 0),
        (1 + shift / 100) * 1.313,
        0.05196 + qnorm(1 - (1 - CI / 100) / 2) * 0.0139
      )
    ),
    
    sel == "Fat NNAT" ~ c(
      rnorm(round(n * p2, 0), 6.204, 2 + qnorm(1 - (1 - CI / 100) / 2) * .9317),
      rnorm(round(n * (1 - p2), 0), (1 + shift / 100) * 12.32, 2 + qnorm(1 - (1 - CI /
                                                                                100) / 2) * 0.9524)
    ),
    
    sel == "Fat Trim" ~ c(
      rnorm(round(n * p2, 0), 3.928, 2 + qnorm(1 - (1 - CI / 100) / 2) * .505),
      rnorm(
        round(n * (1 - p2), 0),
        (1 + shift / 100) * 12.32,
        2.764 + qnorm(1 - (1 - CI / 100) / 2) * 1.231
      )
    )
  )
  return(data.frame(Group = c(
    rep("Reference", length(ref)), rep("Experimental", length(exp))
  ) , values = c(ref, exp)))
}


