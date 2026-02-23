#' Fit an AR model on given data and make predictions for a given set of data with options for using an AR or ARX model
#'
#' @param X_Train Training data for the explanatory variables in the model
#' @param Y_Train Training data for the response variable
#' @param X_Nowcast Data to make predictions bases on
#' @param p Integer indicating the number of parameters for the AR model
#' @param n.ahead Integer indicating the number of predictions to be make
#'
#' @returns Linear model object and predictions
#' @export

fit_AR <- function(Y_train, X_train = NULL, X_nowcast = NULL, params = list(p = 1, d = 0, q = 0, n.ahead = 1)) {
  if (is.null(params$n.ahead)) {
    n <- nrow(data.frame(X_nowcast))
  } else {
    if (params$n.ahead >= nrow(data.frame(X_nowcast))) {
      n <- params$n.ahead
    } else {
      n <- nrow(data.frame(X_nowcast))
    }
  }
  
  if (is.null(params$p)) {
    p <- 0
  } else {
    p <- params$p
  }
  
  if (is.null(params$d)) {
    d <- 0
  } else {
    d <- params$d
  }
  
  if (is.null(params$q)) {
    q <- 0
  } else {
    q <- params$p
  }

  # If the X_train and X_nowcast are NULL, then passing them into the ARIMA
  #  function doesn't effect it at all, allowing us to use the same function for
  #  AR and ARX models!
  AR_mod <- arima(Y_train, order = c(p, d, q), xreg = X_train)

  predictions <- as.numeric(predict(AR_mod, n, X_nowcast)$pred)

  list(model = AR_mod, prediction = predictions)
}