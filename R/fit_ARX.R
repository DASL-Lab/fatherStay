#' Fit an ARX model on given data and make predictions for a given set of data
#'
#' @param X_Train Training data for the explanatory variables in the model
#' @param Y_Train Training data for the response variable
#' @param X_Nowcast Data to make predictions bases on
#' @param p Integer indicating the number of parameters for the AR model
#'
#' @returns Linear model object and predictions
#' @export

fit_ARX <- function(Y_train, X_train = NULL, X_nowcast = NULL, params = list(p = 1)) {
  fit_AR(Y_train, X_train, X_nowcast, params)
}