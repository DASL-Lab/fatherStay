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
                             params = list(degree = 1)) {
  
  if (!requireNamespace("KFAS", quietly = TRUE)) {
    paste("Package \"KFAS\" must be installed to use this function.")
    return(list(model = NULL, prediction = NULL))
  }
  
  if (is.null(params$degree)) {
    degree <- 1
  } else {
    degree <- params$degree
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
                        + SSMregression(formulaToUse, data = data))
  
  # this finds the estimates for the unknown parameters
  fitMod <- KFAS::fitSSM(SMod, inits = c(1,1,1), method = "BFGS")$model
  
  # new data wrangling!
  newn <- length(X_nowcast[,1])
  
  newY <- rep(NA, newn)
  
  # create a new SMod object for the new data
  newMod <- KFAS::SSModel(newY ~ SMregression(formulaToUse, Q = fitMod$Q), H = fitMod$H)
  
  prediction <- predict(FitMod, newdata = newMod)
  
  list(model = FitMod, prediction = prediction)
}
