test_that("linear models work1", {
  x <- c(1:105)
  y <- append(EuStockMarkets[, 2][1:100], rep(NA, 5))
  df <- data.frame(x, y)
  pred <- now_simple_models(y ~ x, model = "lm", df)
  preds <- c(pred[[1]], pred[[2]], pred[[3]], pred[[4]], pred[[5]])
  expect_equal(preds, c(1694.610, 1694.428, 1694.246, 1694.064, 1693.883),
    tolerance = 0.01
  )
})

test_that("ar models work1", {
  x <- c(1:105)
  y <- append(EuStockMarkets[, 2][1:100], rep(NA, 5))
  df <- data.frame(x, y)
  pred <- now_simple_models(y ~ x, model = "ar", df, order = 1)
  preds <- c(pred$pred[1], pred$pred[2], pred$pred[3], pred$pred[4], pred$pred[5])
  expect_equal(preds, c(1726.098, 1726.042, 1725.985, 1725.928, 1725.871),
    tolerance = 0.01
  )
})

test_that("NA's handled correctly", {
  x <- c(1, 1:105)
  y <- append(NA, append(EuStockMarkets[, 2][1:100], rep(NA, 5)))
  df <- data.frame(x, y)
  pred <- now_simple_models(y ~ x, model = "ar", df, order = 1)
  preds <- c(pred$pred[1], pred$pred[2], pred$pred[3], pred$pred[4], pred$pred[5])
  expect_equal(preds, c(1726.098, 1720.011, 1715.376, 1711.845, 1709.150),
    tolerance = 0.01
  )
})