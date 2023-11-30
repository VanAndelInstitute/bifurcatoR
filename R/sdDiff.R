#' sdDiff
#'
#' @param   y   Generic name for the outcome measures
#' @param   X   Generic name for the 2-level grouping variable
#'
#' @return      Absolute value of the difference between group 1 and group 2 standard deviation
#'

#' @export

sdDiff <- function(y, X){
  abs((sd(y[X == 1]) - sd(y[X == 0])))
}

