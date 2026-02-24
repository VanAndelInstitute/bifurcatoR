test_that("est_pow_2_mixtures: returns expected columns", {

  set.seed(1)
  params <- list(
    group1 = list(list(mean = 1, sd = 1), list(mean = 2, sd = 1)),
    group2 = list(list(mean = 1, sd = 2), list(mean = 2, sd = 2))
  )
  
  out <- est_pow_2_mixtures(
    n_group1 = c(10, 10),
    n_group2 = c(10, 10),
    alpha = 0.05,
    nsim = 3,
    dist = "norm",
    params = params,
    tests = c("ANOVA", "levene", "Permutations (Raw)", "cvm")
  )
  
  expect_s3_class(out, "data.frame")
  expect_true(all(c("N", "Test", "power", "FP") %in% names(out)))
  expect_equal(unique(out$N), 40)
  expect_true(all(out$power >= 0 & out$power <= 1))
  expect_true(all(out$FP >= 0 & out$FP <= 1))
  
})


test_that("input validation: requires 2-length mixture vectors and alpha in (0,1)", {
  params <- list(
    group1 = list(list(mean = 1, sd = 1), list(mean = 2, sd = 1)),
    group2 = list(list(mean = 1, sd = 2), list(mean = 2, sd = 2))
  )
  
  expect_error(est_pow_2_mixtures(c(10,10,10), c(10,10), dist="norm", params=params, tests="ANOVA"))
  expect_error(est_pow_2_mixtures(c(10,10), c(10,10), alpha=0, dist="norm", params=params, tests="ANOVA"))
  expect_error(est_pow_2_mixtures(c(0,0), c(10,10), dist="norm", params=params, tests="ANOVA"))
  
})
