#' Fit a spline model on given data and make predictions for a given set of data
#' 
#' @param XTrain Training data for the explanatory variables in the model
#' @param YTrain Training data for the response variable
#' @param XNowcast Data to make predictions bases on
#'
#' @returns GAM model object and predictions
#' @export

fit_GAM <- function(Y_train, X_train = NULL, X_nowcast = NULL, knots = 3, family = gaussian) {
  full_data <- as.data.frame(cbind(Y_train, X_train))
  
  fitted_GAM <- gam(Y_train ~ ., data = full_data, family = family)
  
  XNowcast <- as.data.frame(X_nowcast)
  
  colnames(XNowcast) <- colnames(full_data)[-1]
  
  predicted_GAM <- predict(fitted_GAM, newdata = XNowcast)
  
  list(model = fitted_GAM, prediction = predicted_GAM)
}