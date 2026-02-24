test_that("est_pow_bimodal validates inputs", {
  params <- list(list(mean = 1, sd = 1), list(mean = 2, sd = 1))
  
  expect_error(est_pow_bimodal(n = 10, dist = "norm", params = params, tests = "dip"))
  expect_error(est_pow_bimodal(n = c(10, -1), dist = "norm", params = params, tests = "dip"))
  expect_error(est_pow_bimodal(n = c(10, 10), alpha = 0, dist = "norm", params = params, tests = "dip"))
  expect_error(est_pow_bimodal(n = c(10, 10), nsim = 0, dist = "norm", params = params, tests = "dip"))
  expect_error(est_pow_bimodal(n = c(10, 10), nboot = 0, dist = "norm", params = params, tests = "dip"))
})

test_that("est_pow_bimodal errors if no recognized tests requested", {
  params <- list(list(mean = 1, sd = 1), list(mean = 2, sd = 1))
  expect_error(
    est_pow_bimodal(n = c(10, 10), dist = "norm", params = params, tests = c("NOT_A_TEST")),
    "No recognized tests requested"
  )
})