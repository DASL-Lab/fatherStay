test_that("Kalman Filter works", {
  #Testing stuff for Kalman Filter
  
  testY <- seq(1, 10, 1)
  testX <- data.frame(x1 = seq(1, 20, 2), x2 = seq(4,40,4)+rnorm(10), x3 = rep(2,10))
  Y_train <- seq(1, 10, 1)
  X_train <- data.frame(x1 = seq(1, 20, 2), x2 = seq(4,40,4)+rnorm(10), x3 = rep(2,10))
  CovMatrix <- diag(1,4,4)
  X_nowcast <- data.frame(x1 = c(1, 2, 3), x2 = c(2,3,1), x3 = c(6,7,3))
  data <- data.frame(testY,testX)
  
  suppressWarnings(KFMod <- fit_kf(testY, testX, X_nowcast))

  
  expect_equal(KFMod$prediction[[1]], c(1.5,1.8,1), tolerance = 0.1)
})
