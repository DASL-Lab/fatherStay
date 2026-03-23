#' Fit an ARIMA model on given data and make predictions for a given set of data with options for using an ARIMA or ARIMAX model
#'
#' @param X_Train Training data for the explanatory variables in the model
#' @param Y_Train Training data for the response variable
#' @param X_Nowcast Data to make predictions bases on
#' @param p Integer indicating the number of parameters for the AR model
#' @param d Integer indicating the number of differences
#' @param q integer indicating the degree of the MA model
#'
#' @returns ARIMA model object and predictions

fit_arima <- function(Y_train, X_train = NULL, X_nowcast = NULL, params = list(p = 1, d = 0, q = 1)) {
  fit_ar(Y_train, X_train, X_nowcast, params)
}