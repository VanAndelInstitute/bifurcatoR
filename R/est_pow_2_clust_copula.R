#' est_pow_2_clust_copula
#'
#' @param   n       number of [somethings] c(cluster 1, cluster 2)
#' @param   alpha   default significance level (0.05)
#' @param   nsim    number of simulations (20)
#' @param   dist    generating distribution
#' @param   params  parameters for the generating distribution
#' @param   tests   names of tests to run
#'
#' @return          a power estimate
#'
#' @import mvtnorm
#' @import mclust
#' @import sigclust
#'
#' @export

est_pow_2_clust_copula = function(n,alpha = 0.05,nsim = 20 ,dist = c("norm", "beta", "weibull", "gamma", "lnorm"),params,rho = c(0,0),tests){
  
  dist <- match.arg(dist, c("norm", "beta", "weibull", "gamma", "lnorm"))
  
  cluster <- factor(c(rep("A", n[1]), rep("B", n[2])))
  cluster.numeric <- as.integer(cluster) - 1L
  
  #https://rpubs.com/FJRubio/GC2
  get_copula <- function(qmarg1, qmarg2, n_cop, rho){
    R <- rbind(c(1,rho),c(rho,1))
    dat <- rmvnorm(n_cop, mean = c(0,0), sigma = R)
    dat[,1] <- qmarg1(pnorm(dat[,1]))
    dat[,2] <- qmarg2(pnorm(dat[,2]))
    return(dat)
  }
  
  draw_cop_norm  <- function(n_draw, p_list,rho_draw) {
    get_copula(
      qmarg1 = function(u) qnorm(u,p_list[[1]]$mean, p_list[[1]]$sd),
      qmarg2 = function(u) qnorm(u,p_list[[2]]$mean, p_list[[2]]$sd),
      n_cop = n_draw, rho = rho_draw
    )
  }
  
  draw_cop_weibull  <- function(n_draw, p_list,rho_draw){
    get_copula(
      qmarg1 = function(u) qweibull(u, p_list[[1]]$shape, p_list[[1]]$scale),
      qmarg2 = function(u) qweibull(u, p_list[[2]]$shape, p_list[[2]]$scale),
      n_cop = n_draw, rho = rho_draw
    )
  }
  
  draw_cop_gamma  <- function(n_draw, p_list,rho_draw){
    get_copula(
      qmarg1 = function(u) qgamma(u, p_list[[1]]$shape, p_list[[1]]$scale),
      qmarg2 = function(u) qgamma(u, p_list[[2]]$shape, p_list[[2]]$scale),
      n_cop = n_draw, rho = rho_draw
    )
  }
  
  draw_cop_lnorm  <- function(n_draw, p_list,rho_draw){
    get_copula(
      qmarg1 = function(u) qlnorm(u,p_list[[1]]$meanlog, p_list[[1]]$sdlog),
      qmarg2 = function(u) qlnorm(u,p_list[[2]]$meanlog, p_list[[2]]$sdlog),
      n_cop = n_draw, rho = rho_draw
    )
  }
  
  # Only called once as beta can be bimodal based on shape params
  draw_cop_beta <- function(n_draw, p_list,rho_draw){
    get_copula(
      qmarg1 = function(u) qbeta(u,p_list[[1]]$shape1, p_list[[1]]$shape2),
      qmarg2 = function(u) qbeta(u,p_list[[2]]$shape1, p_list[[2]]$shape2),
      n_cop = n_draw, rho = rho_draw
    )
  }
  
  test_fns <- list()
  
  # modetest family: avoid repeating boilerplate
  add_mclusttest <- function(key, method) {
    if (key %in% tests) {
      force(method)
      test_fns[[key]] <<- function(x){mclustBootstrapLRT(x, modelName=method,
                                                         verbose=FALSE, maxG=1, nboot=500)$p.value < alpha}
    }
  }
  
  add_mclusttest("mclust_EII", "EII")
  add_mclusttest("mclust_VII", "VII")
  add_mclusttest("mclust_EEI", "EEI")
  add_mclusttest("mclust_VVI", "VVI")
  add_mclusttest("mclust_EEE", "EEE")
  add_mclusttest("mclust_VVV", "VVV")
  
  if("sigclust" %in% tests){
    test_fns$sigclust <- function(x){sigclust(x, nsim = 1000)@pval < alpha}
  }
  
  test_names <- names(test_fns)
  
  rej_alt  <- setNames(integer(length(test_fns)), test_names)
  rej_null <- rej_alt
  
  par.alt <- list(
    cluster1 = convert_params(dist, params$cluster1),
    cluster2 = convert_params(dist, params$cluster2)
  )
  
  #clusterX[[1]] is the x axis and clusterY[[2]] is the y-axis, so cluster1 is a list containing the x-axis means and SDs
  null.params <- list(
    null_cluster1 = list(params$cluster1[[1]], params$cluster2[[1]]),
    null_cluster2 = list(params$cluster1[[2]], params$cluster2[[2]])
  )
  
  ## Pool the x-axis means/SDs and y-axis means/SDS
  pool.cluster1 = pool_params(n, null.params$null_cluster1)
  pool.cluster2 = pool_params(n, null.params$null_cluster2)
  
  ## since the null is two completely overlapping clusters, we just duplicate them
  par.null <- list(
    cluster1 = convert_params(dist, list(pool.cluster1,pool.cluster1)),
    cluster2 = convert_params(dist, list(pool.cluster2,pool.cluster2))
  )
  
  make_generator <- function(par) {
    function() {
      switch(
        dist,
        norm  = rbind(draw_cop_norm(n[1], par$cluster1,rho[1]),  draw_cop_norm(n[2], par$cluster2,rho[2])),
        weibull  = rbind(draw_cop_weibull(n[1], par$cluster1,rho[1]),  draw_cop_weibull(n[2], par$cluster2,rho[2])),
        gamma = rbind(draw_cop_gamma(n[1], par$cluster1,rho[1]), draw_cop_gamma(n[2], par$cluster2,rho[2])),
        lnorm = rbind(draw_cop_lnorm(n[1], par$cluster1,rho[1]), draw_cop_lnorm(n[2], par$cluster2,rho[2])),
        beta  = rbind(draw_cop_beta(n, par,rho[1]))
      )
    }
  }
  
  get_alt  <- make_generator(par.alt)
  get_null <- make_generator(par.null)
  
  # --- simulation loop ---
  for (sim in seq_len(nsim)) {
    xa <- get_alt()
    x0 <- get_null()
    
    for (nm in test_names) {
      f <- test_fns[[nm]]
      rej_alt[nm]  <- rej_alt[nm]  + isTRUE(f(xa))
      rej_null[nm] <- rej_null[nm] + isTRUE(f(x0))
    }
  }
  
  nm <- names(test_fns)
  
  test_labels <- c(
    mclust_EII = "Spherical, equal volume",
    mclust_VII = "Spherical, unequal volume",
    mclust_EEI = "Diagonal, equal volume",
    mclust_VVI = "Diagonal, varying volume, varying shape",
    mclust_EEE = "Same volume, shape, orientation",
    mclust_VVV = "Varying volume, shape, and volume",
    sigclust = "sigclust"
  )
  
  pwr.df <- data.frame(
    N     = paste0(n, collapse = ", "),
    Test  = unname(test_labels[nm]),   # pretty label
    power = rej_alt[nm]  / nsim,
    FP    = rej_null[nm] / nsim,
    row.names = NULL,
    check.names = FALSE
  )
  
}

