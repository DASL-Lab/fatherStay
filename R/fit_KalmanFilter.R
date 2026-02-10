#' Fit a Kalman Filter model on given data and make predictions for a given set of data
#'
#' @param X_Train Training data for the explanatory variables in the model
#' @param Y_Train Training data for the response variable
#' @param X_Nowcast Data to make predictions bases on3
#' @param degree For trend component, integer defining the degree of the polynomial trend
#' @param CovMatrix Covariance matrix or array of disturbance terms $\epsilon_t$ of observation equation
#'
#' @returns Kalman Filter model object and predictions
#' @export

fit_KalmanFilter <- function(Y_train, X_train = NULL, X_nowcast = NULL,
                             degree = 1, CovMatrix = NULL) {
  
  if (!requireNamespace("KFAS", quietly = TRUE)) {
    paste("Package \"KFAS\" must be installed to use this function.")
    return(list(model = NULL, prediction = NULL))
  }
  
  # to ensure the data is in a consistent format
  Y_train <- ts(data.frame(Y_train))
  X_train <- data.frame(X_train)
  data <- data.frame(X_train, Y_train)
  
  # creating the formula
  formulaToUse <- "~"
  for (i in seq(ncol(X_train))) {
    formulaToUse <- paste0(formulaToUse, colnames(X_train)[i], "+")
  }
  formulaToUse <- substr(formulaToUse, 1, (nchar(formulaToUse) - 1))
  formulaToUse <- as.formula(formulaToUse)
  YFormula <- colnames(Y_train)
  
  # fitting the model
  # for some reason using KFAS::SSMtrend() and KFAS::SSMregression() makes this break
  SSMtrend <- KFAS::SSMtrend
  SSMregression <- KFAS::SSMregression
  SMod <- KFAS::SSModel(Y_train ~ SSMtrend(degree = 1,  Q = list(matrix(NA))) 
                        + SSMregression(~ x1 + x2, data = data))
  
  # this finds the estimates for the unknown parameters
  FitMod <- KFAS::fitSSM(SMod, inits = c(1,1,1), method = "BFGS")$model
  
  prediction <- predict(FitMod, interval = "prediction", level = 0.9)
  
  list(model = FitMod, prediction = prediction)
}
