#' permutation_tests
#'
#' @param   temp_df   Generic name for the data frame to be permuted columns are the group variable X and the value variable y
#' @param   nperm     The number of permutations to be performed
#' @param   fxn       The function name to call is [meanDiff] [madDiff] [giniDiff] [sdDiff]
#' @param   alpha     The confidence level
#'
#' @return      a 3 level list containing: a p-value, the actual difference, and the 2.5% - 97.5% interval
#'
#' @importFrom stats ecdf quantile
#'
#' @export

permutation_tests <- function(y, X, nperm, fxn_names, alpha) {
  stopifnot(length(y) == length(X))
  stopifnot(is.numeric(nperm), nperm > 0)
  stopifnot(is.numeric(alpha), alpha > 0, alpha < 1)
  stopifnot(is.character(fxn_names), length(fxn_names) > 0)
  
  fxn_map <- list(
    "Permutations (Raw)"  = meanDiff,
    "Permutations (SD)"   = sdDiff,
    "Permutations (MAD)"  = madDiff,
    "Permutations (Gini)" = giniDiff
  )
  
  missing <- setdiff(fxn_names, names(fxn_map))
  if (length(missing) > 0) {
    stop("Unknown fxn_names: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  
  perm_fxns <- fxn_map[fxn_names]  # preserves requested order
  k <- length(perm_fxns)
  n <- length(y)
  
  actual_diff <- vapply(perm_fxns, function(f) f(y, X), numeric(1))
  names(actual_diff) <- fxn_names
  
  perm_vals <- matrix(NA_real_, nrow = nperm, ncol = k)
  colnames(perm_vals) <- fxn_names
  
  for (i in seq_len(nperm)) {
    Xp <- sample(X, n, replace = FALSE)
    perm_vals[i, ] <- vapply(perm_fxns, function(f) f(y, Xp), numeric(1))
  }
  
  pvals <- vapply(seq_len(k), function(j) {
    (sum(abs(perm_vals[, j]) >= abs(actual_diff[j])) + 1) / (nperm + 1)
  }, numeric(1))
  names(pvals) <- fxn_names
  
  crit <- lapply(seq_len(k), function(j) {
    stats::quantile(abs(perm_vals[, j]), probs = c(alpha/2, 1 - alpha/2), names = FALSE)
  })
  names(crit) <- fxn_names
  
  list(p = pvals, diff = actual_diff, crit = crit)
}
