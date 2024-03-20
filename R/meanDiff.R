#' meanDiff
#'
#' @param   y   Generic name for the outcome measures
#' @param   X   Generic name for the 2-level grouping variable
#'
#' @return      Absolute value of the group 1 vs group 2 mean difference
#'
#' @export

meanDiff <- function(y, X){
  abs(mean(y[X == 1]) - mean(y[X == 0]))
}

