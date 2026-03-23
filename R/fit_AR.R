#' Fit an AR model on given data and make predictions for a given set of data with options for using an AR, ARX, or ARIMA model
#'
#' @param X_Train Training data for the explanatory variables in the model
#' @param Y_Train Training data for the response variable
#' @param X_Nowcast Data to make predictions bases on
#' @param params A named list containing additional parameters: `p` Integer indicating the degree of the AR model, and `n.ahead` integer indicating the number of predictions to be make
#'
#' @returns Arima object and predictions

fit_ar <- function(Y_train, X_train = NULL, X_nowcast = NULL, params = list(p = 1, d = 0, q = 0, n.ahead = 1)) {
  # retrieve the number of future predictions to make, if not specified, then will be based on the number of rows in the nowcast data
  if (!"n.ahead" %in% names(params)) {
    n <- nrow(data.frame(X_nowcast))
  } else {
    if (params$n.ahead >= nrow(data.frame(X_nowcast))) {
      n <- params$n.ahead
    } else {
      n <- nrow(data.frame(X_nowcast))
    }
  }

  # retrieve the values for `p`, `d`, and `q`, if not specified then they will be 1, 0, and 0 respectively
  if (!"p" %in% names(params)) {
    p <- 0
  } else {
    p <- params$p
  }

  if (!"d" %in% names(params)) {
    d <- 0
  } else {
    d <- params$d
  }

  if (!"q" %in% names(params)) {
    q <- 0
  } else {
    q <- params$q
  }

  # If the X_train and X_nowcast are NULL, then passing them into the ARIMA
  #  function doesn't effect it at all, allowing us to use the same function for
  #  AR and ARX models!
  
  # create the AR model
  AR_mod <- arima(Y_train, order = c(p, d, q), xreg = X_train)

  # create the predictions
  preds <- predict(AR_mod, n, X_nowcast)

  # put the predictions into a data frame and add 95% confidence bands
  predictions <- data.frame(prediction = preds$pred, lower = preds$pred - 1.96 * preds$se, upper = preds$pred + 1.96 * preds$se)

  # find the fitted values of the model
  fitVals <- Y_train - AR_mod$residuals

  # assemble the output in the correct format
  list(model = AR_mod, prediction = predictions, fitted_values = fitVals)
}
