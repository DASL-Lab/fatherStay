#' Fit an eXtreme Gradient Boost model model on given data and make predictions for a given set of data
#'
#' @param X_Train Training data for the explanatory variables in the model
#' @param Y_Train Training data for the response variable
#' @param X_Nowcast Data to make predictions bases on
#' @param params A named list containing options for the parameters used by `xgboost::xgb.train`
#'
#' @returns XGBoost object and predictions
#' @export

fit_XGBoost <- function(Y_train, X_train = NULL, X_nowcast = NULL,
                   params = list(nrounds = 100, evals = list(), 
                                 objective = NULL, verbose = 1,
                                 XGBparams = list())) {
  
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    warning("Package \"xgboost\" must be installed to use this function.")
    return(list(model = NULL, prediction = NULL))
  }
  
  Y_train <- data.frame(Y_train)
  X_train <- data.frame(X_train)
  dMatrixTrain <- xgboost::xgb.DMatrix(X_train, label = as.matrix(Y_train))
  
  if (!"nrounds" %in% names(params)) {
    nrounds <- 100
  } else {
    nrounds <- params$nrounds
  }
  
  if (!"evals" %in% names(params)) {
    evals <- list()
  } else {
    evals <- params$evals
  }
  
  if (!"objective" %in% names(params)) {
    objective <- NULL
  } else {
    objective <- params$objective
  }
  
  if (!"verbose" %in% names(params)) {
    verbose <- 1
  } else {
    verbose <- params$verbose
  }
  
  if (!"XGBparams" %in% names(params)) {
    xgbParams2 = xgboost::xgb.params()
  } else {
    xgbParams2 <- params$XGBparams
  }

  # internal cross validation to tune the nrounds of the model
  XGBCV <- xgboost::xgb.cv(params = xgbParams2, data = dMatrixTrain, nrounds = nrounds, nfold = 5, verbose = 0)
    
  nrounds2 <- which(XGBCV$evaluation_log$test_rmse_mean == min(XGBCV$evaluation_log$test_rmse_mean))
  
  print(nrounds2)
  # traing the model
  XGBModel <- xgboost::xgb.train(
    data = dMatrixTrain, params = xgbParams2, nrounds = nrounds2, evals = evals, 
    objective = objective, verbose = verbose)
  
  # create the nowcasting data in the proper form
  yNow <- as.matrix(rep(NA,length(data.frame(X_nowcast)[,1])))
  
  X_nowcast <- as.matrix(X_nowcast)
  
  dMatrixPred <- xgboost::xgb.DMatrix(data = X_nowcast, label = yNow)
  
  predictions <- predict(XGBModel, newdata = dMatrixPred)
  
  list(model = XGBModel, prediction = predictions)
}