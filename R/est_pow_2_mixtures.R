#' est_pow_2_mixtures
#'
#' @param   n_group1      number of [somethings] in c(group 1 mode 1 , group 1 mode 2)
#' @param   n_group2      number of [somethings] in c(group 2 mode 1, group 2 mode 2)
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


est_pow_2_mixtures <- function(n_group1,n_group2, alpha = 0.05, nsim = 20, dist = c("norm", "beta", "weibull", "gamma", "lnorm"), params, tests) {
  # ---- validation ----
  dist <- match.arg(dist, c("norm","beta","weibull","gamma","lnorm"))
  
  stopifnot(is.numeric(n_group1), length(n_group1) == 2, all(is.finite(n_group1)), all(n_group1 >= 0))
  stopifnot(is.numeric(n_group2), length(n_group2) == 2, all(is.finite(n_group2)), all(n_group2 >= 0))
  n_group1 <- as.integer(n_group1)
  n_group2 <- as.integer(n_group2)
  
  n1 <- sum(n_group1)
  n2 <- sum(n_group2)
  
  stopifnot(n1 > 0, n2 > 0)
  
  stopifnot(is.numeric(alpha), length(alpha) == 1, is.finite(alpha), alpha > 0, alpha < 1)
  nsim <- as.integer(nsim);  stopifnot(nsim > 0)

  if (missing(tests) || is.null(tests)) tests <- character()
  stopifnot(is.character(tests))

  dist <- match.arg(dist)
  
  group <- factor(c(rep("A", n1), rep("B", n2)))
  group.numeric <- as.integer(group) - 1L
  
  # permutation label set & requested subset
  perm_labels <- c("Permutations (Raw)","Permutations (SD)","Permutations (MAD)","Permutations (Gini)")
  fxn_names <- intersect(perm_labels, tests)
  need_perm <- length(fxn_names) > 0
  
  # --- test functions: all return TRUE/FALSE ---
  test_fns <- list()
  
  if ("ks" %in% tests) test_fns$ks <- function(x) ks.test(x[[1]], x[[2]])$p.value < alpha
  if ("cvm" %in% tests) test_fns$cvm <- function(x) cvm_test(x[[1]], x[[2]], nboots = 500)[2] < alpha
  if ("dts" %in% tests) test_fns$dts <- function(x) dts_test(x[[1]], x[[2]], nboots = 500)[2] < alpha
  if ("ad"  %in% tests) test_fns$ad  <- function(x) ad_test(x[[1]], x[[2]], nboots = 500)[2] < alpha
  if ("wass" %in% tests) test_fns$wass <- function(x) wass_test(x[[1]], x[[2]], nboots = 500)[2] < alpha
  if ("kuiper" %in% tests) test_fns$kuiper <- function(x) kuiper_test(x[[1]], x[[2]], nboots = 500)[2] < alpha
  
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
  par.alt <- list(
    group1 = convert_params(dist, params$group1),
    group2 = convert_params(dist, params$group2)
  )
  
  null.params <- list(
    null_group1 = list(params$group1[[1]], params$group2[[1]]),
    null_group2 = list(params$group1[[2]], params$group2[[2]])
  )
  
  pool.group1 <- pool_params(n_group1, null.params$null_group1)
  pool.group2 <- pool_params(n_group2, null.params$null_group2)
  pooled.modes <- list(pool.group1,pool.group2) 
  
  par.null <- list(
    group1 = convert_params(dist, pooled.modes),
    group2 = convert_params(dist, pooled.modes)
  )
  
  # --- draw helpers ---
  draw2_norm  <- function(n_vec, p_list) c(
    rnorm(n_vec[1], p_list[[1]]$mean, p_list[[1]]$sd),
    rnorm(n_vec[2], p_list[[2]]$mean, p_list[[2]]$sd)
  )
  draw2_weibull  <- function(n_vec, p_list) c(
    rweibull(n_vec[1], p_list[[1]]$shape, p_list[[1]]$scale),
    rweibull(n_vec[2], p_list[[2]]$shape, p_list[[2]]$scale)
  )
  draw2_gamma <- function(n_vec, p_list) c(
    rgamma(n_vec[1], p_list[[1]]$shape, p_list[[1]]$scale),
    rgamma(n_vec[2], p_list[[2]]$shape, p_list[[2]]$scale)
  )
  draw2_lnorm <- function(n_vec, p_list) c(
    rlnorm(n_vec[1], p_list[[1]]$meanlog, p_list[[1]]$sdlog),
    rlnorm(n_vec[2], p_list[[2]]$meanlog, p_list[[2]]$sdlog)
  )
  draw_beta <- function(n, p_list) rbeta(n, p_list[[1]]$shape1, p_list[[1]]$shape2)
  
  make_generator <- function(par) {
    function() {
      switch(
        dist,
        norm  = list(draw2_norm(n_group1, par$group1),  draw2_norm(n_group2, par$group2)),
        weibull  = list(draw2_weibull(n_group1, par$group1),  draw2_weibull(n_group2, par$group2)),
        gamma = list(draw2_gamma(n_group1, par$group1), draw2_gamma(n_group2, par$group2)),
        lnorm = list(draw2_lnorm(n_group1, par$group1), draw2_lnorm(n_group2, par$group2)),
        beta  = list(draw_beta(n1, par$group1),         draw_beta(n2, par$group2))
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
  
  # labels (fix perm_gini key)
  test_labels <- c(
    ks = "Kolmogorov-Smirnov",
    cvm  = "Cramer-von Mises",
    dts  = "DTS",
    ad = "Anderson-Darling",
    wass =  "Wasserstein distance",
    kuiper = "Kuiper",
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
    N     = n1 + n2,
    Test  = unname(test_labels[nm]),
    power = rej_alt[nm] / nsim,
    FP    = rej_null[nm] / nsim,
    row.names = NULL,
    check.names = FALSE
  )
}

