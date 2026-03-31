#' Fit a linear model on given data and make predictions for a given set of data
#'
#' @param X_Train Training data for the explanatory variables in the model
#' @param Y_Train Training data for the response variable
#' @param X_Nowcast Data to make predictions bases on
#'
#' @returns Linear model object and predictions

fit_lm <- function(Y_train, X_train, X_nowcast, params = NULL) {
  Y_train <- as.data.frame(Y_train)
  X_train <- as.data.frame(X_train)
  X_nowcast <- as.data.frame(X_nowcast)
  
  full_data <- as.data.frame(cbind(Y_train, X_train))

  fitted_LM <- lm(Y_train ~ ., data = full_data)

  XNowcast <- as.data.frame(X_nowcast)

  colnames(XNowcast) <- colnames(full_data)[-1]

  preds <- predict(
    fitted_LM, newdata = XNowcast, se.fit = TRUE, interval = "prediction"
  )$fit |>
    as.data.frame()
  colnames(preds) <- c("prediction", "lower", "upper")

  fitted <- predict(fitted_LM, newdata = X_train)

  list(model = fitted_LM, prediction = preds, fitted_values = fitted)
}