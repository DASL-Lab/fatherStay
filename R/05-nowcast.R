#' Nowcast various models from a prepared data frame
#'
#' @param formula A formula, e.g. y ~ x, y ~ lag(x1, 1) + lag(x2, 3)
#' @param data A data frame. Must contain the variables specified in the formula and in `date_col`. Trailing NA values in `y` will be nowcasted.
#' @param model The model to use for nowcasting. See `vignette("Model_Details", package = "fatherStay")` and `vignette("Mechanistic_Model", package = "fatherStay")` for more information about currently implemented models, and `vignette("Write_fit_XX_functions", package = "fatherStay")` for how to write your own model for use in this package.
#' @param batches The number of batches to use for training in the EnbPI calculation (akin to the number of folds for k-fold cross validation).
#' @param train_window The number of days to use for training. Defaults to 60% of the training data, which allows for a large training set for each batch while also allowing for a reasonable amount of variation in the test sets.
#' @param level The prediction interval level.
#' @param params The parameters to use for the model. Must be a named list.
#' @param date_col Name of the column containing date information. If NULL, the date information attempted to be inferred. If there's a single datetime column then it is used. If the data are a ts or mts or zoo object, the dates are esxtracted.
#'
#' @returns An object of class "`dadnow`".
#'
#' @export
nowcast <- function(
    formula, data, model, params = NULL, date_col = NULL,
    batches = 40, train_window = NULL, level = 0.95
  ) {

  cat("Model:", model, "\n")
  
  prepped_data <- prep_data(
    formula, data, model, date_col = date_col
  )

  
  response <- all.vars(formula)[1]
  terms <- all.vars(formula)[-1]
  if (model == "mechanistic") {
    cat(
      paste0(
        "I see the formula \"", deparse(formula), "\"\n",
        "Assuming that \"", response, "\" contains DAD data, \"", 
        terms[1], "\" is CNISP, \"", terms[2], "\" is PTSOS, and \"",
        terms[3], "\" is RVDSS.\n"
      )
    )
  }

  x_train <- prepped_data$X_train
  y_train <- prepped_data$y_train
  x_now <- prepped_data$X_nowcast
  y_now <- prepped_data$y_nowcast
  
  enbpi <- enbpi(
    X_train = x_train,
    y_train = y_train,
    formula = prepped_data$formula,
    model = model,
    params = params,
    k = nrow(x_now),
    batches = batches,
    train_window = train_window,
    level = level
  )

  # Fit to all training, create nowcast
  nowcast <- dispatch_model(model)(
    X_train = x_train,
    Y_train = y_train,
    X_nowcast = x_now,
    params = params
  )

  aug_data <- as.data.frame(data)
  for (covariate in prepped_data$covariates) {
    aug_data[[covariate]] <- impute_linear(dates = aug_data[, prepped_data$date_col], x = aug_data[[covariate]])
  }
  aug_data <- aug_data[order(aug_data[, prepped_data$date_col]), ]
  
  nowcasted_data <- aug_data[(nrow(x_train) + 1):nrow(aug_data), ]
  nowcasted_data[, prepped_data$response] <- nowcast$prediction$prediction
  nowcasted_data$model <- ifelse(
    model == "mechanistic", yes = paste0("mech_", params$method), no =model
  )
  nowcasted_data$params <- paste0(names(params), params, collapse = "_")
  nowcasted_data$pi_lower <- nowcast$prediction$prediction +
    qnorm(1 - (1 - level)/2) * enbpi$se
  nowcasted_data$pi_upper <- nowcast$prediction$prediction +
    qnorm((1 - level)/2) * enbpi$se
  nowcasted_data$formula <- deparse(formula)

  aug_data$model <- "Training"
  aug_data$params <- "None"
  aug_data$pi_lower <- NA
  aug_data$pi_upper <- NA
  aug_data$formula <- NA
  aug_data <- rbind(aug_data, nowcasted_data)

  dadnow <- list(
    date_col = date_col,
    data = aug_data,
    evals = enbpi$evals,
    response = response,
    batches = batches,
    train_window = train_window,
    level = level,
    models = list(
      list(
        model_id = make_model_id(enbpi$evals),
        formula = formula,
        prepped_data = prepped_data,
        model = nowcast$model,
        predictions = nowcast$prediction,
        evals = enbpi$evals,
        enbpi = enbpi$enbpi,
        params = params
      )
    )
  )
  model_ids <- make_model_id(enbpi$evals)
  names(dadnow$models) <- model_ids
  for (i in seq_along(dadnow$models)) {
    dadnow$models[[i]]$model_id <- model_ids[i]
  }

  class(dadnow) <- "multidadnow"
  dadnow
}

make_model_id <- function(evals) {

  all_formulas <- paste0("f", match(evals$formula, unique(evals$formula)))
  all_models <- evals$model#paste0("m", match(evals$model, unique(evals$model)))
  model_ids <- paste0(all_formulas, "_",  all_models)

  param_set <- ave(
    seq_len(nrow(evals)),
    all_models, 
    all_formulas,
    FUN = seq_along
  )
  
  suffix <- ifelse(param_set > 1, letters[param_set], "")
  model_ids <- paste0(model_ids, suffix)

  model_ids
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
    "mechanistic" = fit_mechanistic,
    model
  )
}
