test_that("est_pow_2_clust_copula: returns expected columns", {
  
  set.seed(1)
  params <- list(
    cluster1 = list(list(mean = 1, sd = 1), list(mean = 2, sd = 1)),
    cluster2 = list(list(mean = 10, sd = 2), list(mean = 10, sd = 2))
  )
  
  out <- est_pow_2_clust_copula(
    n = c(50,25),
    alpha = 0.05,
    nsim = 3,
    dist = "norm",
    params = params,
    tests = c("mclust_EEE","mclust_VVV")
  )
  
  expect_s3_class(out, "data.frame")
  expect_true(all(c("N", "Test", "power", "FP") %in% names(out)))
  expect_equal(unique(out$N), "50, 25")
  expect_true(all(out$power >= 0 & out$power <= 1))
  expect_true(all(out$FP >= 0 & out$FP <= 1))
  
})


