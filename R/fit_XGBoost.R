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
                   params = list(nrounds = 100, verbose = 1,
                                 XGBparams = list())) {
  
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    warning("Package \"xgboost\" must be installed to use this function.")
    return(list(model = NULL, prediction = NULL))
  }
  
  Y_train <- data.frame(Y_train)
  X_train <- data.frame(X_train)
  dMatrixTrain <- xgboost::xgb.DMatrix(X_train, label = as.matrix(Y_train))
  
  # only does cross validation for nrounds if the number of rounds is not specified
  if (!"nrounds" %in% names(params)) {
    # internal cross validation to tune the nrounds of the model
    XGBCV <- xgboost::xgb.cv(params = list(max_depth = 2), data = dMatrixTrain, 
                             nrounds = 100, nfold = 5, verbose = 0, 
                             early_stopping_rounds = 10)
    
    nrounds2 <- XGBCV$early_stop$best_iteration
  } else {
    nrounds2 <- params$nrounds
  }
  
  if (!"verbose" %in% names(params)) {
    verbose <- 1
  } else {
    verbose <- params$verbose
  }
  
  if (!"XGBparams" %in% names(params)) {
    depths <- seq(2,10)
    
    cvDepth <- c()
    
    for (i in depths) {
      cv <- xgboost::xgb.cv(params = list(max_depth = i), data = dMatrixTrain, 
                            nrounds = nrounds2, nfold = 5, verbose = 0,
                            early_stopping_rounds = 10)
      cvDepth[i] <- cv$early_stop$best_score
    }
    xgbParams2 = xgboost::xgb.params(max_depth = which.min(cvDepth))
  } else {
    xgbParams2 <- params$XGBparams
  }
  
  # traing the model
  XGBModel <- xgboost::xgb.train(
    data = dMatrixTrain, params = xgbParams2, nrounds = nrounds2, 
    verbose = verbose)
  
  # create the nowcasting data in the proper form
  yNow <- as.matrix(rep(NA,length(data.frame(X_nowcast)[,1])))
  
  X_nowcast <- as.matrix(X_nowcast)
  
  dMatrixPred <- xgboost::xgb.DMatrix(data = X_nowcast, label = yNow)
  
  predictions <- predict(XGBModel, newdata = dMatrixPred)
  
  list(model = XGBModel, prediction = predictions)
}