#' Fit an ARIMA model on given data and make predictions for a given set of data with options for using an ARIMA or ARIMAX model
#'
#' @param X_Train Training data for the explanatory variables in the model
#' @param Y_Train Training data for the response variable
#' @param X_Nowcast Current data of X_train for which there is no Y_train data, used to make nowcasts
#' @param params Named list containing additional parameters: `p` Integer indicating the number of parameters for the AR model, `d` Integer indicating the number of differences, `q` integer indicating the degree of the MA model
#'
#' @returns ARIMA model object, predictions, and fitted values of the model on the training data

fit_arima <- function(Y_train, X_train = NULL, X_nowcast = NULL, params = list(p = 1, d = 0, q = 1)) {
  # this is just a wrapper function that calls the fit_AR model with the proper parameters
  fit_ar(Y_train, X_train, X_nowcast, params)
}