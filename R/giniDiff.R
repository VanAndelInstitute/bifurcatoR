#' giniDiff
#' 
#' @param   y   Generic name for the outcome measures
#' @param   X   Generic name for the 2-level grouping variable 
#'
#' @return      Absolute value of the difference between group 1 and group 2 Gini's mean difference
#'  
#' @impot       Hmisc  
#'
#' @export

giniDiff <- function(y, X){
  abs((Hmisc::GiniMd(y[X == 1]) - Hmisc::GiniMd(y[X == 0])))
}