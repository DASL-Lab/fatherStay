test_that("Random Forest works", {
  set.seed(12345)
  
  data("iris")
  Y_train <- iris$Sepal.Length[1:140]
  X_train <- iris[1:140,-c(1,5)]
  X_nowcast <- iris[141:150,-c(1,5)]

  testPred <- fit_RF(Y_train, X_train, X_nowcast)

  model <- randomForest::randomForest(
    Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
    data = train_data, ntree = 500,weights = NULL,
    replace = TRUE, maxnodes = NULL,
    mtry = 1
  )
  
  set.seed(12345)
  preds <- predict(model, newdata = X_nowcast)

  expect_equal(testPred$prediction, preds, tolerance = 1)
})
