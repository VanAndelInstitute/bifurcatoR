#' madDiff
#' 
#' @param   y   Generic name for the outcome measures
#' @param   X   Generic name for the 2-level grouping variable 
#'
#' @return      Absolute value of the difference between group 1 and group 2 median absolute deviation
#' 
#' @export

madDiff <- function(y, X){
  abs((mad(y[X == 1], constant = 1) - mad(y[X == 0], constant = 1)))
}

