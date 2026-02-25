#' est_pow_2_unimodes
#'
#' @param   n      number of samples in c(group 1, group 2)
#' @param   alpha   default significance level (0.05)
#' @param   nsim    number of simulations (20)
#' @param   modes   modes of the two groups
#' @param   dist    generating distributions
#' @param   params  parameters for the generating distributions
#' @param   tests   names of tests to run between the two groups
#'
#' @return          a power estimate
#'
#' @import          twosamples
#' @import          car
#' @import          data.table
#'
#' @export

est_pow_2_unimodes <- function(n, 
                               alpha = 0.05, 
                               nsim = 20,
                               dist = c("norm", "beta", "weibull", "gamma", "lnorm"),
                               params,
                               tests){
  dist <- match.arg(dist, c("norm","beta","weibull","gamma","lnorm"))
  
  ## error checks ##
  stopifnot(is.numeric(n), length(n) == 2, all(is.finite(n)), all(n > 0))
  n <- as.integer(n)
  stopifnot(is.numeric(alpha), length(alpha) == 1, is.finite(alpha), alpha > 0, alpha < 1)
  stopifnot(is.numeric(nsim), length(nsim) == 1, is.finite(nsim), nsim > 0)
  nsim <- as.integer(nsim)
  
  if (missing(tests) || is.null(tests)) tests <- character()
  stopifnot(is.character(tests))
  
  group <- factor(c(rep("A", n[1]), rep("B", n[2])))
  group.numeric <- as.integer(group) - 1L
  
  # permutation label set & requested subset
  perm_labels <- c("Permutations (Raw)","Permutations (SD)","Permutations (MAD)","Permutations (Gini)")
  fxn_names <- intersect(perm_labels, tests)
  need_perm <- length(fxn_names) > 0
  
  # --- test functions: all return TRUE/FALSE ---
  test_fns <- list()
  
  if ("levene" %in% tests) {
    test_fns$levene <- function(x) leveneTest(c(x[[1]], x[[2]]), group)$"Pr(>F)"[1] < alpha
  }
  if ("ANOVA" %in% tests) {
    test_fns$ANOVA <- function(x) anova(lm(c(x[[1]], x[[2]]) ~ group))$"Pr(>F)"[1] < alpha
  }
  if ("Non-parametric ANOVA" %in% tests) {
    test_fns$Non_p_ANOVA <- function(x) anova(lm(rank(c(x[[1]], x[[2]])) ~ group))$"Pr(>F)"[1] < alpha
  }
  
  # permutation tests: store as separate keys, but will consume p-vector, not x
  if ("Permutations (Raw)"  %in% fxn_names) test_fns$perm_raw  <- function(pvec) pvec[["Permutations (Raw)"]]  < alpha
  if ("Permutations (SD)"   %in% fxn_names) test_fns$perm_sd   <- function(pvec) pvec[["Permutations (SD)"]]   < alpha
  if ("Permutations (MAD)"  %in% fxn_names) test_fns$perm_mad  <- function(pvec) pvec[["Permutations (MAD)"]]  < alpha
  if ("Permutations (Gini)" %in% fxn_names) test_fns$perm_gini <- function(pvec) pvec[["Permutations (Gini)"]] < alpha
  
  test_names <- names(test_fns)
  
  # identify which test keys are perm tests
  perm_keys <- c("perm_raw","perm_sd","perm_mad","perm_gini")
  is_perm_test <- test_names %in% perm_keys
  
  rej_alt  <- setNames(integer(length(test_fns)), test_names)
  rej_null <- rej_alt
  
  # --- parameter conversion (your existing logic) ---
  par.alt <-  convert_params(dist, params)
  
  pooled <- pool_params( c(n[1],n[2]), params)
  par.null <- convert_params(dist, list(pooled, pooled))

  # --- draw helpers ---
  draw_norm  <- function(n_vec, p_list) list(
    rnorm(n_vec[1], p_list[[1]]$mean, p_list[[1]]$sd),
    rnorm(n_vec[2], p_list[[2]]$mean, p_list[[2]]$sd)
  )
  
  draw_weibull  <- function(n_vec, p_list) list(
    rweibull(n_vec[1], p_list[[1]]$shape, p_list[[1]]$scale),
    rweibull(n_vec[2], p_list[[2]]$shape, p_list[[2]]$scale)
  )
  draw_gamma <- function(n_vec, p_list) list(
    rgamma(n_vec[1], p_list[[1]]$shape, p_list[[1]]$scale),
    rgamma(n_vec[2], p_list[[2]]$shape, p_list[[2]]$scale)
  )
  draw_lnorm <- function(n_vec, p_list) list(
    rlnorm(n_vec[1], p_list[[1]]$meanlog, p_list[[1]]$sdlog),
    rlnorm(n_vec[2], p_list[[2]]$meanlog, p_list[[2]]$sdlog)
  )
  draw_beta <- function(n_vec, p_list) list(
    rbeta(n_vec[1], p_list[[1]]$shape1, p_list[[1]]$shape2),
    rbeta(n_vec[2], p_list[[2]]$shape1, p_list[[2]]$shape2)
  )
  
  make_generator <- function(par) {
    function() {
      switch(
        dist,
        norm  = draw_norm(c(n[1],n[2]), par),
        weibull  = draw_weibull(c(n[1],n[2]), par),
        gamma = draw_gamma(c(n[1],n[2]), par),
        lnorm = draw_lnorm(c(n[1],n[2]), par),
        beta  = draw_beta(c(n[1],n[2]), par)
      )
    }
  }
  
  get_alt  <- make_generator(par.alt)
  get_null <- make_generator(par.null)
  
  # --- simulation loop ---
  for (sim in seq_len(nsim)) {
    xa <- get_alt()
    x0 <- get_null()
    
    if (need_perm) {
      xa_perm <- permutation_tests(y = c(xa[[1]], xa[[2]]), X = group.numeric, nperm = 500, fxn_names = fxn_names, alpha = alpha)$p
      x0_perm <- permutation_tests(y = c(x0[[1]], x0[[2]]), X = group.numeric, nperm = 500, fxn_names = fxn_names, alpha = alpha)$p
    }
    
    for (j in seq_along(test_names)) {
      nm <- test_names[j]
      f  <- test_fns[[nm]]
      
      if (is_perm_test[j]) {
        rej_alt[nm]  <- rej_alt[nm]  + isTRUE(f(xa_perm))
        rej_null[nm] <- rej_null[nm] + isTRUE(f(x0_perm))
      } else {
        rej_alt[nm]  <- rej_alt[nm]  + isTRUE(f(xa))
        rej_null[nm] <- rej_null[nm] + isTRUE(f(x0))
      }
    }
  }
  
  test_labels <- c(
    levene = "Levene's",
    ANOVA = "ANOVA",
    Non_p_ANOVA = "Non-parametric ANOVA",
    perm_raw = "Permutations (Raw)",
    perm_sd = "Permutations (SD)",
    perm_mad = "Permutations (MAD)",
    perm_gini = "Permutations (Gini)"
  )
  
  nm <- test_names
  data.frame(
    N     = n[1] + n[2],
    Test  = unname(test_labels[nm]),
    power = rej_alt[nm] / nsim,
    FP    = rej_null[nm] / nsim,
    row.names = NULL,
    check.names = FALSE
  )
}

