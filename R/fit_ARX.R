#' Fit an AR model on given data and make predictions for a given set of data with options for using an AR or ARX model
#'
#' @param XTrain Training data for the explanatory variables in the model
#' @param YTrain Training data for the response variable
#' @param XNowcast Data to make predictions bases on
#' @param p Integer indicating the number of parameters for the AR model
#'
#' @returns Linear model object and predictions
#' @export

fit_ARX <- function(Y_train, X_train = NULL, X_nowcast = NULL, p = 1) {
  fit_AR(Y_train, X_train, X_nowcast, p)
}