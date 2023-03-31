meanDiff <- function(y, X){
  abs(mean(y[X == 1]) - mean(y[X == 0]))
}

madDiff <- function(y, X){
  abs((mad(y[X == 1], constant = 1) - mad(y[X == 0], constant = 1)))
}

giniDiff <- function(y, X){
  abs((Hmisc::GiniMd(y[X == 1]) - Hmisc::GiniMd(y[X == 0])))
}

sdDiff <- function(y, X){
  abs((sd(y[X == 1]) - sd(y[X == 0])))
}


perm = function(temp_df,nboot,fxn,alpha) {
  
  # Get actual absolute mean difference.
  actual_diff <- do.call(fxn, list(temp_df$y, temp_df$X))
  # Find shuffled mean differences.
  shuffled_diff <- sapply(1:nboot, function(p){
    shuffled_idx <- sample(1:nrow(temp_df))
    shuffled_X <- temp_df$X[shuffled_idx]
    return(do.call(fxn,list(temp_df$y, shuffled_X)))
  })
  
  percentile <- ecdf(shuffled_diff)
  p = 1 - percentile(actual_diff)
  return(list(p = p,diff = actual_diff, crit = quantile(shuffled_diff,p=c(alpha/2,(1-alpha/2)))))
}
