test_that("XGBoost predictions work", {
  set.seed(12345)
  
  data("iris")
  Y_train2 <- iris$Sepal.Length[1:140]
  X_train2 <- iris[1:140,-c(1,5)]
  X_nowcast2 <- iris[141:150,-c(1,5)]
  
  irisxgb <- fit_xgboost(Y_train = Y_train2, X_train = X_train2, X_nowcast = X_nowcast2)
  
  expect_equal(irisxgb$prediction[[1]], 
               c(6.430758,6.271862,5.803501,7.115215,6.381705,5.951983,5.732964,
                 6.266992,6.351661,6.234945),
               tolerance = 0.0001)
})
