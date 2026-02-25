test_that("est_pow_2_unimodes runs and returns expected columns", {
  skip_if_not_installed("car")
  
  set.seed(123)
  out <- est_pow_2_unimodes(
    n = c(20, 20),
    alpha = 0.05,
    nsim = 3,
    dist = "norm",
    params = list(list(mean = 1, sd = 1), list(mean = 1, sd = 2)),
    tests = c("ANOVA", "levene", "Permutations (Raw)")
  )
  
  expect_s3_class(out, "data.frame")
  expect_true(all(c("N", "Test", "power", "FP") %in% names(out)))
  expect_equal(unique(out$N), 40)
  expect_true(all(out$power >= 0 & out$power <= 1))
  expect_true(all(out$FP >= 0 & out$FP <= 1))
  
  
})


test_that("est_pow_2_unimodes normality tests are working correctly", {
  skip_if_not_installed("car")
  
  set.seed(777)
  out <- est_pow_2_unimodes(
    n = c(20, 20),
    alpha = 0.05,
    nsim = 10,
    dist = "norm",
    params = list(list(mean = 1, sd = 1), list(mean = 4, sd = 1)),
    tests = c("ANOVA",
              "Non-parametric ANOVA",
              "levene",
              "Permutations (Raw)",
              "Permutations (SD)",
              "Permutations (MAD)",
              "Permutations (Gini)")
  )
  
  expect_equal(out$power, c(0,1,1,1,0,0,0))
  expect_equal(out$FP, c(0.1, 0.0, 0.0, 0.0, 0.0, 0.1, 0.0))
  
})


test_that("est_pow_2_unimodes weibull tests are working correctly", {
  skip_if_not_installed("car")
  
  set.seed(777)
  out <- est_pow_2_unimodes(
    n = c(20, 20),
    alpha = 0.05,
    nsim = 10,
    dist = "weib",
    params = list(list(mean = 1, sd = 1), list(mean = 4, sd = 1)),
    tests = c("ANOVA",
              "Non-parametric ANOVA",
              "levene",
              "Permutations (Raw)",
              "Permutations (SD)",
              "Permutations (MAD)",
              "Permutations (Gini)")
  )
  
  expect_equal(out$power, c(0.1, 1.0, 1.0, 1.0, 0.2, 0.0, 0.1))
  expect_equal(out$FP, c(0.1, 0.0, 0.0, 0.1, 0.0, 0.2, 0.0))
  
})


test_that("est_pow_2_unimodes lnorm tests are working correctly", {
  skip_if_not_installed("car")
  
  set.seed(777)
  out <- est_pow_2_unimodes(
    n = c(20, 20),
    alpha = 0.05,
    nsim = 10,
    dist = "lnorm",
    params = list(list(mean = 1, sd = 1), list(mean = 4, sd = 1)),
    tests = c("ANOVA",
              "Non-parametric ANOVA",
              "levene",
              "Permutations (Raw)",
              "Permutations (SD)",
              "Permutations (MAD)",
              "Permutations (Gini)")
  )
  
  expect_equal(out$power, c(0.3, 1.0, 1.0, 1.0, 0.4, 0.0, 0.3))
  expect_equal(out$FP, c(0.2, 0.0, 0.0, 0.0, 0.1, 0.1, 0.1))
  
})

test_that("est_pow_2_unimodes gamma tests are working correctly", {
  skip_if_not_installed("car")
  
  set.seed(777)
  out <- est_pow_2_unimodes(
    n = c(20, 20),
    alpha = 0.05,
    nsim = 10,
    dist = "gamma",
    params = list(list(mean = 1, sd = 1), list(mean = 4, sd = 1)),
    tests = c("ANOVA",
              "Non-parametric ANOVA",
              "levene",
              "Permutations (Raw)",
              "Permutations (SD)",
              "Permutations (MAD)",
              "Permutations (Gini)")
  )
  
  expect_equal(out$power, c(1, 1, 1, 1, 1, 0, 1))
  expect_equal(out$FP, c(0, 0, 0, 0, 0, 0, 0))
  
})

test_that("est_pow_2_unimodes beta tests are working correctly", {
  skip_if_not_installed("car")
  
  set.seed(777)
  out <- est_pow_2_unimodes(
    n = c(20, 20),
    alpha = 0.05,
    nsim = 10,
    dist = "beta",
    params = list(list(mean = 0.1, sd = 0.1), list(mean = 0.8, sd = 0.1)),
    tests = c("ANOVA",
              "Non-parametric ANOVA",
              "levene",
              "Permutations (Raw)",
              "Permutations (SD)",
              "Permutations (MAD)",
              "Permutations (Gini)")
  )
  
  expect_equal(out$power, c(0 1 1 1 0 0 0))
  expect_equal(out$FP, c(0.0, 0.0, 0.1, 0.0, 0.0, 0.0, 0.0))
  
})

