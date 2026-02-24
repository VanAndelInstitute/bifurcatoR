test_that("convert_params: beta requires mean in (0,1) and feasible sd", {
  params_bad_mean <- list(list(mean = 1, sd = 0.1), list(mean = 0.5, sd = 0.1))
  expect_error(convert_params("beta", params_bad_mean), "means must be in")
  
  params_bad_sd <- list(list(mean = 0.5, sd = 0.6), list(mean = 0.5, sd = 0.1))
  # since sd^2 >= m(1-m)=0.25 here
  expect_error(convert_params("beta", params_bad_sd), "sd\\^2 must be <")
})

test_that("convert_params: lnorm requires mean > 0", {
  params <- list(list(mean = -1, sd = 1), list(mean = 1, sd = 1))
  expect_error(convert_params("lnorm", params), "means must be > 0")
})

test_that("convert_params: norm passes through", {
  params <- list(list(mean = 1, sd = 1), list(mean = 2, sd = 3))
  out <- convert_params("norm", params)
  expect_equal(out, params)
})

