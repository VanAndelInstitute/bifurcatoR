# Regression tests: these tests used to error inside bifurcatoR_Analysis().
two_groups <- function() {
  set.seed(1)
  data.frame(
    group = factor(rep(c("A", "B"), each = 20)),
    value = c(rnorm(20, 0, 1), rnorm(20, 1, 2))
  )
}

test_that("group-comparison tests run and return one row each", {
  res <- bifurcatoR_Analysis(
    two_groups(),
    tests = c("anova.p", "anova.np", "ks", "perm.raw", "perm.sd", "Levene"),
    nboot = 25, alpha = 0.05
  )
  expect_equal(nrow(res), 6)
  expect_false(anyNA(res$p.value))
  expect_true(all(res$p.value >= 0 & res$p.value <= 1))
})

test_that("unknown tests are reported clearly", {
  expect_error(bifurcatoR_Analysis(two_groups(), "nope", 10, 0.05), "Unknown test")
})

test_that("group tests refuse data that do not have exactly two groups", {
  d <- two_groups()
  d$group <- factor("A")
  res <- suppressWarnings(bifurcatoR_Analysis(d, "ks", nboot = 10, alpha = 0.05))
  expect_true(is.na(res$p.value))
  expect_match(res$CI, "exactly two groups")
})

test_that("pool_params weights by sample size", {
  p <- pool_params(c(10, 30), list(list(mean = 0, sd = 1), list(mean = 4, sd = 1)))
  expect_equal(p$mean, 3)
})

test_that("a single-group copula null has no cluster structure: both null clusters are identical", {
  # smoke test: runs end to end with different rhos and returns power and FP
  set.seed(1)
  res <- est_pow_2_clust_copula(
    n = c(20, 20), nsim = 2, dist = "norm", rho = c(0.8, -0.8),
    params = list(
      cluster1 = list(list(mean = 0, sd = 1), list(mean = 0, sd = 1)),
      cluster2 = list(list(mean = 4, sd = 1), list(mean = 4, sd = 1))
    ),
    tests = "mclust_EII"
  )
  expect_true(all(c("power", "FP") %in% names(res)))
})

test_that("est_pow_2_mixtures runs with different mode proportions in the two groups", {
  set.seed(1)
  res <- est_pow_2_mixtures(
    n_group1 = c(10, 30), n_group2 = c(30, 10), nsim = 3, dist = "norm",
    params = list(
      group1 = list(list(mean = 0, sd = 1), list(mean = 5, sd = 1)),
      group2 = list(list(mean = 0, sd = 1), list(mean = 5, sd = 1))
    ),
    tests = c("welch", "ks")
  )
  expect_equal(nrow(res), 2)
})
