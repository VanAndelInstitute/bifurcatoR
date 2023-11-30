test_that("Checking dxsmall", {
            data("dxsmall", package="bifurcatoR")
            expect_true(is.factor(dxsmall@colData$fusion))
            expect_true(is.numeric(dxsmall@colData$OS))
          })
