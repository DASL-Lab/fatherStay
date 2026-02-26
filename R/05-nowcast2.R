#' Nowcast various models from a prepared data frame
#'
#' @param formula A formula, e.g. y ~ x, y ~ lag(x1, 1) + lag(x2, 3)
#' @param data A data frame. Must contain the variables specified in the formula and in `date_col`. Trailing NA values in `y` will be nowcasted.
#' @param model The model to use for nowcasting. Currently implemented: "lm", "ar". Can be a vector, in which case the model is trained for each model in the vector.
#' @param test_size The proportion of the data to use for testing. If NULL, the data are not split. Defaults to 10% of the data.
#' @param params The parameters to use for the model. Must be a named list.
#' @param date_col Name of the column containing date information. If NULL, the date information attempted to be inferred. If there's a single datetime column then it is used. If the data are a ts or mts or zoo object, the dates are esxtracted.
#' @param eval A character vector of evaluation metrics to use. Currently implemented: "rmse", "mae", and "mre" (mean relative error).
#'
#' @returns An object of class "`dadnow`".
#'
#' @export
nowcast_one <- function(
    formula, data, model, test_size = 0.1, params = NULL, date_col = NULL
  ) {
  
  prepped_data <- prep_data(
    formula, data, model, test_size, date_col = date_col
  )
  
  model_id <- make_model_id(as.character(substitute(model)), params)

  x_train <- prepped_data$X_train
  y_train <- prepped_data$y_train
  x_test <- prepped_data$X_test
  y_test <- prepped_data$y_test
  x_now <- prepped_data$X_nowcast
  y_now <- prepped_data$y_nowcast

  # Fit to training, evaluate on test
  prepped_data$eval <- fit_model_test(
    model, params, x_train, y_train, x_test, y_test
  )

  # Fit to all training, create nowcast
  nowcast <- dispatch_model(
    model = model, 
    x_train = rbind(x_train, x_test),
    y_train = c(y_train, y_test),
    x_nowcast = x_now,
    params = params
  )
  nowcast$params <- params

  prepped_data[model_id] <- list(nowcast)

  return(prepped_data)
}

fit_model_test <- function(model, params, x_train, y_train, x_test, y_test) {
  
  model_id <- make_model_id(model, params)
  
  test_preds <- dispatch_model(model, x_train, y_train, x_test, params)
  eval <- data.frame(
    "rmse" = sqrt(mean((y_test - test_preds$prediction)^2)),
    "mae" = mean(abs(y_test - test_preds$prediction)),
    "mre" = mean(((y_test - test_preds$prediction) / (y_test + 0.1))^2)
  )
  rownames(eval) <- model_id
  eval
}

make_model_id <- function(model, params) {
  model_id <- paste0("nowcast_", model, "_",
    paste0(names(params), params, collapse = "_")
  )
  gsub("_$", "", model_id)
}

dispatch_model <- function(model, x_train, y_train, x_nowcast, params) {
  switch(model,
    "lm" = fit_LM(X_train = x_train,  Y_train = y_train, X_nowcast = x_nowcast, params = params),
    "ar" = fit_AR(X_train = x_train,  Y_train = y_train, X_nowcast = x_nowcast, params = params),
    "arx" = fit_ARX(X_train = x_train,  Y_train = y_train, X_nowcast = x_nowcast, params = params),
    "arima" = fit_ARIMA(X_train = x_train,  Y_train = y_train, X_nowcast = x_nowcast, params = params),
    "gam" = fit_GAM(X_train = x_train,  Y_train = y_train, X_nowcast = x_nowcast, params = params),
    "kf" = fit_KalmanFilter(X_train = x_train,  Y_train = y_train, X_nowcast = x_nowcast, params = params),
    "rf" = fit_RF(X_train = x_train,  Y_train = y_train, X_nowcast = x_nowcast, params = params),
    model(X_train = x_train,  Y_train = y_train, X_nowcast = x_nowcast, params = params)
  )
}