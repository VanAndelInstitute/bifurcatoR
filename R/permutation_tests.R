#' permutation_tests
#'
#' @param   temp_df   Generic name for the data frame to be permuted columns are the group variable X and the value variable y
#' @param   nperm     The number of permutations to be performed
#' @param   fxn       The function name to call is [meanDiff] [madDiff] [giniDiff] [sdDiff]
#' @param   alpha     The confidence level
#'
#' @return      a 3 level list containingg: a p-value, the actual difference, and the 2.5% - 97.5% interval
#'
#' @export



permutation_tests = function(temp_df,nperm,fxn,alpha) {

  # Get actual absolute mean difference.
  actual_diff <- do.call(fxn, list(temp_df$y, temp_df$X))
  # Find shuffled mean differences.
  shuffled_diff <- sapply(1:nperm, function(p){
    shuffled_idx <- sample(1:nrow(temp_df))
    shuffled_X <- temp_df$X[shuffled_idx]
    return(do.call(fxn,list(temp_df$y, shuffled_X)))
  })

  percentile <- ecdf(shuffled_diff)
  p = 1 - percentile(actual_diff)
  return(list(p = p,diff = actual_diff, crit = quantile(shuffled_diff,p=c(alpha/2,(1-alpha/2)))))
}
