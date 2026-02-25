#' est_pow_bimodal
#'
#' @param   n       number of samples c(mode 1, mode 2)
#' @param   alpha   default significance level (0.05)
#' @param   nsim    number of simulations (20)
#' @param   dist    generating distribution
#' @param   params  parameters for the generating distribution
#' @param   tests   names of tests to run
#'
#' @return          a power estimate
#'
#' @import          mclust
#' @import          diptest
#' @import          mousetrap
#' @import          LaplacesDemon
#' @import          multimode
#' @import          Hmisc
#' @import          twosamples
#' @import          mixR
#'
#' @export
est_pow_bimodal = function(n,alpha = 0.05,nsim = 20,dist =c("norm", "beta", "weibull", "gamma", "lnorm") ,params,tests){
  dist <- match.arg(dist, c("norm","beta","weibull","gamma","lnorm"))
  if (!is.numeric(n) || length(n) != 2 || n <= 0) stop("n must be length 2")
  if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1) stop("alpha must be in (0,1).")
  if (!is.numeric(nsim) || nsim <= 0) stop("nsim must be > 0.")

  if (missing(tests) || is.null(tests)) tests <- character()
  if (!is.character(tests)) stop("tests must be a character vector.", call. = FALSE)
  
  test_fns <- list()
  
  if ("mclust_E" %in% tests) {
    test_fns$mclust_E <- function(x) {
      mclustBootstrapLRT(data.frame(x=x), modelName="E",
                                 verbose=FALSE, maxG=1, nboot=500)$p.value < alpha
    }
  }
  
  if ("mclust_V" %in% tests) {
    test_fns$mclust_V <- function(x) {
      mclustBootstrapLRT(data.frame(x=x), modelName="V",
                         verbose=FALSE, maxG=1, nboot=500)$p.value < alpha
    }
  }
  
  if ("mt" %in% tests) {
    test_fns$mt <- function(x) {
      mt_check_bimodality(data.frame(x = x),method="BC")$BC > (5/9)
    }
  }
  # modetest family: avoid repeating boilerplate
  add_modetest <- function(key, method) {
    if (key %in% tests) {
      test_fns[[key]] <<- function(x) modetest(x, mod0 = 1, method = method, B = 500)$p.value < alpha
    }
  }
  
  add_modetest("dip", "HH")
  add_modetest("SI",  "SI")
  add_modetest("CH",  "CH")
  add_modetest("HY",  "HY")
  add_modetest("ACR", "ACR")
  add_modetest("FM",  "FM")
  
  add_mixR <- function(key, family) {
    if (key %in% tests) {
      test_fns[[key]] <<- function(x) {
        pval <- try(bs_lrt(x, H0 = 1, H1 = 2, family = family, nboot = 500)$pvalue, silent = TRUE)
        if (inherits(pval, "try-error")) FALSE else (pval < alpha)
      }
    }
  }
  
  add_mixR("WmixR",  "weibull")
  add_mixR("NmixR",  "normal")
  add_mixR("GmixR",  "gamma")
  add_mixR("LNmixR", "lnorm")
  
  #### add any future tests starting here ####
  
  test_names <- names(test_fns)
  
  rej_alt  <- setNames(integer(length(test_fns)), test_names)
  rej_null <- rej_alt
  
  par.alt <- convert_params(dist,params)
  pool <- pool_params(c(n[1],n[2]),par.alt)
  par.null <- list(pool,pool) 
  
  # --- generators ---
  draw2_norm  <- function(n1, n2, p_list) c(
    rnorm(n1, p_list[[1]]$mean, p_list[[1]]$sd),
    rnorm(n2, p_list[[2]]$mean, p_list[[2]]$sd)
  )
  draw2_weibull  <- function(n1, n2, p_list) c(
    rweibull(n1, p_list[[1]]$shape, p_list[[1]]$scale),
    rweibull(n2, p_list[[2]]$shape, p_list[[2]]$scale)
  )
  draw2_gamma <- function(n1, n2, p_list) c(
    rgamma(n1, p_list[[1]]$shape, p_list[[1]]$scale),
    rgamma(n2, p_list[[2]]$shape, p_list[[2]]$scale)
  )
  draw2_lnorm <- function(n1, n2, p_list) c(
    rlnorm(n1, p_list[[1]]$meanlog, p_list[[1]]$sdlog),
    rlnorm(n2, p_list[[2]]$meanlog, p_list[[2]]$sdlog)
  )
  draw_beta <- function(n, p_list) rbeta(n, p_list[[1]]$shape1, p_list[[1]]$shape2)
  
  get_alt <- function() {
    switch(
      dist,
      norm  = draw2_norm(n[1], n[2], par.alt),
      weibull  = draw2_weibull(n[1], n[2], par.alt),
      gamma = draw2_gamma(n[1], n[2], par.alt),
      lnorm = draw2_lnorm(n[1], n[2], par.alt),
      beta  = draw_beta(n, par.alt)
    )
  }
  
  n.tot <- sum(n)
  
  get_null <- function() {
    switch(
      dist,
      norm  = rnorm(n.tot,  par.null$mean,   par.null$sd),
      weibull  = rweibull(n.tot, par.null$shape, par.null$scale),
      gamma = rgamma(n.tot,  par.null$shape, par.null$scale),
      lnorm = rlnorm(n.tot,  par.null$meanlog, par.null$sdlog),
      beta  = rbeta(n.tot,   par.null$shape1, par.null$shape2)
    )
  }
  
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
  
  test_labels <- c(
    mclust_E = "Mclust equal var",
    mclust_V = "Mclust unequal var",
    WmixR  = "Weibull mixR",
    NmixR  = "Normal mixR",
    GmixR =  "Gamma mixR",
    LNmixR = "Log normal mixR",
    mt     = "Bimodality Coefficient",
    dip    = "Hartigans' dip test",
    SI     = "Silverman Bandwidth test",
    HY     = "Hall and York Bandwidth test",
    CH     = "Cheng and Hall Excess Mass",
    ACR    = "Ameijeiras-Alonso et al. Excess Mass",
    FM     = "Fisher and Marron Cramer-von Mises"
  )
  
  nm <- names(test_fns)
  
  pwr.df <- data.frame(
    N     = paste0(n,collapse = ", "),
    Test  = unname(test_labels[nm]),   # pretty label
    power = rej_alt[nm]  / nsim,
    FP    = rej_null[nm] / nsim,
    row.names = NULL,
    check.names = FALSE
  )
  
  return(pwr.df)
  
}
