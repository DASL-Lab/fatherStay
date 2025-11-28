test_that("linear models work", {
  x <- c(1:100)
  y <- EuStockMarkets[,2][1:100]
  df <- data.frame(x,y)
  expect_equal(fit_basicModels(y ~ x, model = "linear", df)$coef[[1]], 1712.976, tolerance = 0.001)
})

test_that("linear models work 2", {
  x <- c(1:100)
  y <- EuStockMarkets[,2][1:100]
  df <- data.frame(x,y)
  expect_equal(fit_basicModels(y ~ x, model = "linear", df)$coef[[2]], -0.1818, tolerance = 0.001)
})

test_that("AR models work", {
  x <- c(1:100)
  y <- EuStockMarkets[,2][1:100]
  df <- data.frame(x,y)
  expect_equal(fit_basicModels(y, model = "AR", df)$coef[[1]], 0.762926, tolerance = 0.001)
})

test_that("AR models work 2", {
  x <- c(1:100)
  y <- EuStockMarkets[,2][1:100]
  df <- data.frame(x,y)
  expect_equal(fit_basicModels(y, model = "AR", df)$coef[[2]], 1703.930612, tolerance = 0.001)
})