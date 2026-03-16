#' Nowcast various models from a prepared data frame
#'
#' @param formula A formula, e.g. y ~ x, y ~ lag(x1, 1) + lag(x2, 3)
#' @param data A data frame. Must contain the variables specified in the formula and in `date_col`. Trailing NA values in `y` will be nowcasted.
#' @param model The model to use for nowcasting. Currently implemented: "lm", "ar". Can be a vector, in which case the model is trained for each model in the vector.
#' @param params The parameters to use for the model. Must be a named list.
#' @param date_col Name of the column containing date information. If NULL, the date information attempted to be inferred. If there's a single datetime column then it is used. If the data are a ts or mts or zoo object, the dates are esxtracted.
#' @param eval A character vector of evaluation metrics to use. Currently implemented: "rmse", "mae", and "mre" (mean relative error).
#'
#' @returns An object of class "`dadnow`".
#'
#' @export
nowcast_one <- function(
    formula, data, model, batches = 40, train_window = NULL, level = 0.95, params = NULL, date_col = NULL
  ) {

  prepped_data <- prep_data(
    formula, data, model, date_col = date_col
  )
  
  model_id <- make_model_id(model, params)

  x_train <- prepped_data$X_train
  y_train <- prepped_data$y_train
  x_now <- prepped_data$X_nowcast
  y_now <- prepped_data$y_nowcast

  evals <- cross_val_error(x_train, y_train, prepped_data$cross_val_indices, model, params)

  enbpi <- enbpi(X_train = x_train, y_train = y_train, model = model, params = params, k = nrow(x_now), batches = batches, train_window = train_window, level = level)

  # Fit to all training, create nowcast
  nowcast <- dispatch_model(model)(
    X_train = x_train,
    Y_train = y_train,
    X_nowcast = x_now,
    params = params
  )

  dadnow_obj <- list(
    date_col = date_col,
    data = as.data.frame(data),
    models = list(
      list(
        model_id = model_id,
        formula = formula,
        prepped_data = prepped_data,
        model = nowcast$model,
        predictions = nowcast$prediction,
        evals = evals,
        params = params
      )
    )
  )
  names(dadnow_obj$models)[1] <- model_id
  class(dadnow_obj) <- "multidadnow"

  dadnow_obj
}

make_model_id <- function(model, params) {
  if (!is.character(model)) model <- as.character(substitute(model))
  model_id <- paste0(model, "_",
    paste0(names(params), params, collapse = "_")
  )
  gsub("_$", "", model_id)
}

dispatch_model <- function(model, x_train, y_train, x_nowcast, params) {
  switch(model,
    "lm" = fit_LM,
    "ar" = fit_AR,
    "arx" = fit_ARX,
    "arima" = fit_ARIMA,
    "gam" = fit_GAM,
    "kf" = fit_KalmanFilter,
    "rf" = fit_RF,
    "xgboost" = fit_XGBoost,
    model
  )
}