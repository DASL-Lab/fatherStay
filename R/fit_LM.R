#' Fit a linear model on given data and make predictions for a given set of data
#'
#' @param X_Train Training data for the explanatory variables in the model
#' @param Y_Train Training data for the response variable
#' @param X_Nowcast Data to make predictions bases on
#'
#' @returns Linear model object and predictions

fit_LM <- function(Y_train, X_train, X_nowcast, params = NULL) {
  full_data <- as.data.frame(cbind(Y_train, X_train))

  fitted_LM <- lm(Y_train ~ ., data = full_data)

  XNowcast <- as.data.frame(X_nowcast)

  colnames(XNowcast) <- colnames(full_data)[-1]

  preds <- predict(
    fitted_LM, newdata = XNowcast, se.fit = TRUE, interval = "prediction"
  )$fit |>
    as.data.frame()
  colnames(preds) <- c("prediction", "lower", "upper")

  list(model = fitted_LM, prediction = preds, fitted_values = fitted_LM$fitted.values)
}