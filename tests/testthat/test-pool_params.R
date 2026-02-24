test_that("pool_params errors for too-small n", {
  params <- list(list(mean = 1, sd = 1), list(mean = 1, sd = 2))
  expect_error(pool_params(c(1, 5), params))
  expect_error(pool_params(c(2, 1), params))
})

test_that("pool_params returns expected mean and pooled sd", {
  params <- list(list(mean = 1, sd = 1), list(mean = 3, sd = 2))
  out <- pool_params(c(10, 10), params)
  
  expect_true(is.list(out))
  expect_true(all(c("mean", "sd") %in% names(out)))
  expect_equal(out$mean, 2)  # (10*1 + 10*3)/20
  expect_true(out$sd > 0)
})