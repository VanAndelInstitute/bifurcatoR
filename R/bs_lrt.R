#' bs_lrt
#' bootstrap likelihood ratio tests for mixR fits
#'
#' The mixR package provides fast implementations for a number of 1-dimensional
#' mixture models, along with the bs.test function for performing bootstrap
#' likelihood ratio tests of component numbers (default is "one lump or two?").
#' Note that the fitting and bootstrapping of non-normal mixtures can be
#' substantially sped up by binning the data. See mixR::bin for details.
#'
#' @param   x         raw data for a mixR fit (vector or 3-column matrix)
#' @param   H0        the number of components in the null model (1)
#' @param   H1        the number of components in the alternative model (2)
#' @param   family    fit distribution ("normal","weibull","gamma","lnorm")

#' @param   nboot     number of bootstraps to perform (100)
#' @param   iter      maximum iterations for EM algorithm (1000)
#' @param   ...       additional arguments, passed to mixR::bs.test()
#'
#' @return  an object of class "bootEM" with three items (see mixR::bs.test)
#'
#' @seealso mixR::bin
#' @seealso mixR::mixfit
#' @seealso mixR::bs.test
#' @seealso mixR
#'
#' @import ppclust
#'
#' @export
bs_lrt <- function(x, H0=1, H1=2, family="normal", nboot=1e2, iter=1e3, ...){
  
  #Our own initz with fuzzy c means if kmeans fails
  initz2 <-  function(x, ncomp, init.method = c("kmeans", "hclust","fuzzy")) {
    
    init.method = match.arg(init.method)
    # check if 'x' is a matrix (from grouped data)
    if(is.matrix(x)) {
      x <- reinstate(x)
    }
    if(init.method == "kmeans") {
      a <- kmeans(x, centers = ncomp,nstart = 1)$cluster
      if(any(table(a) < length(x) *0.05)){
        a <- fpppcm(x,centers = ncomp)$cluster
      }
    } else {
      a <- cutree(hclust(dist(x)), ncomp)
      # a <- fpppcm(x,centers = ncomp)$cluster
    } 
    res <- list()
    for(i in 1:ncomp) {
      res[[i]] <- x[a == i]
    }
    count <- sapply(res, length)
    pi <- count / sum(count)
    mu <- sapply(res, mean)
    sd <- sapply(res, sd)
    if(any(is.na(sd))){sd[which(is.na(sd))] = sd[which(!is.na(sd))][1] }
    #If we still have a cluster of size 1, even after fuzzy c means, 
    #we'll give this cluster equal sd to cluster 1 and let the EM algorithm go
    
    
    
    order <- order(mu)
    
    pi <- pi[order]
    mu <- mu[order]
    sd <- sd[order]
    list(pi = pi, mu = mu, sd = sd)
  }
  
  # unlockBinding("initz", as.environment("package:mixR"))
  # assign("initz", initz2, "package:mixR")
  
  unlockBinding("initz",  getNamespace("mixR"))
  assign("initz", initz2,  getNamespace("mixR"))
  
  # check input
  if(!is.numeric(H0) || H0 < 1) stop("H0 must be a positive integer.")
  if(!is.numeric(H1) || H1 < H0) stop("H1 must be an integer greater than H0.")

  # perform the bootstrap test
  tst.p = mixR::bs.test(x, ncomp=c(H0, H1), family=family, B=nboot, max_iter=iter, ...)
  # If this test comes back NA, it usually means the EM algorithm failed to converge on two unequal modes.
  # Attempt with equal variances to get a p-value. An alternate option would be to set NA -> p =0.99999
  if(is.na(tst.p$pvalue)){
    tst.p = mixR::bs.test(x, ncomp=c(H0, H1), family=family, B=nboot, max_iter=iter,ev=T ,...)
  }
  
  return(tst.p)

}
