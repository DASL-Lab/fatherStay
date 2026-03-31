test_that("fit simple model works", {
  testY <- seq(1, 10, 1)
  testX <- seq(1, 20, 2)
  testNow <- c(1, 2, 3)
  test_mod <- fit_lm(testY, testX, testNow)
  expect_equal(test_mod[[1]]$coefficients[[1]], 0.5)
  expect_equal(test_mod[[1]]$coefficients[[2]], 0.5)
  expect_equal(test_mod$prediction$prediction[[1]], 1)
  expect_equal(test_mod$prediction$prediction[[2]], 1.5)
  expect_equal(test_mod$prediction$prediction[[3]], 2)
})

test_that("fitting simple model with multiple explanatory variables works", {
  Y_train <- seq(1, 10, 1)
  X_train <- data.frame(
    x1 = c(1, -1.5, 2, 4, 5, 5.8, 6.4, 7.1, 8, 8.5),
    x2 = c(1.838, 0.382, -0.306, 2.185, 0.458, -2.366, -0.959, -1.014, -0.158, -2.133)
  )
  X_nowcast <- data.frame(c(1, 2, 3), c(4, 5, 6))
  test_mod <- fit_lm(Y_train, X_train, X_nowcast)
  expect_equal(test_mod[[1]]$coefficients[[1]], 1.8348, tolerance = 0.001)
  expect_equal(test_mod[[1]]$coefficients[[2]], 0.7736, tolerance = 0.001)
  expect_equal(test_mod[[1]]$coefficients[[3]], -0.4033, tolerance = 0.001)
  expect_equal(test_mod$prediction$prediction[[1]], 0.9950741, tolerance = 0.001)
  expect_equal(test_mod$prediction$prediction[[2]], 1.3653039, tolerance = 0.001)
  expect_equal(test_mod$prediction$prediction[[3]], 1.7355338, tolerance = 0.001)
})