#' Nowcast various models from a prepared data frame
#'
#' @param formula A formula, e.g. y ~ x, y ~ lag(x1, 1) + lag(x2, 3)
#' @param data A data frame. Must contain the variables specified in the formula and in `date_col`. Trailing NA values in `y` will be nowcasted.
#' @param model The model to use for nowcasting. See `vignette("Model_Details", package = "dadnowcast")` and `vignette("Mechanistic_Model", package = "dadnowcast")` for more information about currently implemented models, and `vignette("Write_fit_XX_functions", package = "dadnowcast")` for how to write your own model for use in this package.
#' @param batches The number of batches to use for training in the EnbPI calculation (akin to the number of folds for k-fold cross validation).
#' @param train_window The number of days to use for training. Defaults to 60% of the training data, which allows for a large training set for each batch while also allowing for a reasonable amount of variation in the test sets.
#' @param level The prediction interval level.
#' @param params The parameters to use for the model. Must be a named list.
#' @param date_col Name of the column containing date information. If NULL, the date information attempted to be inferred. If there's a single datetime column then it is used. If the data are a ts or mts or zoo object, the dates are esxtracted.
#' @param se Should the standard errors and evaluation metrics be calculated by ensemble-based batch prediction intervals (EnbPI)? If FALSE, the function will return NAs for the standard errors and evaluation metrics.
#'
#' @details
#' A short overview of the specific models used here is given below, more details can be found in the vignette `Model_Details` and the individual help files (`?fit_*`, where `*` is the name of the model).
#' - "lm", a standard linear model, which takes no additional parameters.
#' - "ar", an autoregressive model, it has the parameter `p` which is the degree of the AR model.
#' - "arx", an autoregressive model with exogenous variables, it is an AR model that adds the capacity to consider explanatory variables, it has the same parameters as an ar model.
#' - "arima", an ARIMA model combines an AR model with a moving average (MA) model, it has parameters `p`, which specifies the degree of the AR component, `d` which specifies the degree of differencing in the model, and `q` which is the degree of the moving average model.
#' - "gam", a generalized additive model, it has parameters `family` to specify the family used, `smooths` which is smoothing parameters for each variable, and `bs` which specifies basis smoothing functions.
#' - "kf", a Kalman filter model, which is a continuously updating model with a prediction and update portion, it takes additional parameters `degree` which specifies the degree of the trend, and `CovMatrix` which is a covariance matrix for the disturbance terms.
#' - "rf", a random forest model, a tree based model that grows many trees and combines the output to pick a prediction, it takes parameters `ntree` the number of trees to grow, `mtry` the number of variables used as candidates at each split, `weights` weights the sample data, `replace` should sampling be done with or without replacement, `maxnodes` limits the number of terminal nodes, `nodesize` sets the minimum size of the terminal nodes.
#' - "xgboost", a extreme gradient boosting model, which is a tree based model in which subsequent trees learn from previous trees, it takes parameters `nrounds` which is the number of boosting iteration to do, and `XGBparams` a list of more parameters used in the model the most important of which is `max_depth` both `nrounds` and `max_depth` are tuned if their values are not specified, it also takes `verbose` should output be silent (0) or not (1).
#' - "mechanistic", creates a mechanistic model, `sc`, `sp`, and `method` specifies the family.
#' - See `vignette("Write_fit_XX_functions", package = "dadnowcast")` for how to write your own model for use in this package.
#'
#'
#' @returns An object of class "`dadnow`".
#'
#' @export
nowcast <- function(
    formula, data, model, params = NULL, date_col = NULL,
    batches = 40, train_window = NULL, level = 0.95, se = TRUE
  ) {

  cat("Model:", model, "\n")

  # Retuns a list with all of the neceassry data.
  # This ends up duplicating the data within each model, but it makes it
  # easier to deal with models that have different formulas (and thus have different
  # training data sets).
  prepped_data <- prep_data(
    formula, data, model, date_col = date_col
  )

  # If mechanistic, warn that the formula needs a particular format.
  response <- prepped_data$response
  terms <- prepped_data$covariates
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

  X_train <- prepped_data$X_train
  y_train <- prepped_data$y_train
  X_now <- prepped_data$X_nowcast
  y_now <- prepped_data$y_nowcast

  # Returns a list with the k-step ahead prediction standard errors and the evaluations of the models for the k-step ahead predictions.
  # The evaluations include all metrics used for comparison.
  enbpi <- enbpi(
    X_train = X_train,
    y_train = y_train,
    formula = prepped_data$formula,
    model = model,
    params = params,
    k = nrow(X_now),
    batches = batches,
    train_window = train_window,
    se = se
  )

  # Fit to all training, create nowcast
  # Returns the raw model object, a data frame called prediction with a column labelled prediction, and a data frame called fitted_values with the fitted values.
  nowcast <- dispatch_model(model)(
    X_train = X_train,
    Y_train = y_train,
    X_nowcast = X_now,
    params = params
  )

  # Take the training data and add the nowcasted data to it.
  # Format it so that it's just the columns used in the formula.
  aug_data <- as.data.frame(data)
  # FIXME: Different models have different covariates; imputation will miss some.
  # Later, when models are added, the covariate rows will not be duplicated. 
  for (covariate in prepped_data$covariates) {
    aug_data[[covariate]] <- impute_linear(dates = aug_data[, prepped_data$date_col], x = aug_data[[covariate]])
  }
  aug_data <- aug_data[order(aug_data[, prepped_data$date_col]), ]

  nowcasted_data <- aug_data[(nrow(X_train) + 1):nrow(aug_data), ]
  nowcasted_data[, prepped_data$response] <- nowcast$prediction$prediction
  nowcasted_data$model <- ifelse(
    model == "mechanistic", yes = paste0("mech_", params$method), no = model
  )
  nowcasted_data$params <- paste0(names(params), params, collapse = "_")
  nowcasted_data$pi_lower <- nowcast$prediction$prediction +
    qnorm((1 - level)/2) * enbpi$se
  nowcasted_data$pi_upper <- nowcast$prediction$prediction +
    qnorm(1 - (1 - level)/2) * enbpi$se
  nowcasted_data$formula <- deparse(formula)

  aug_data$model <- "Training"
  aug_data$params <- "None"
  aug_data$pi_lower <- NA
  aug_data$pi_upper <- NA
  aug_data$formula <- NA
  aug_data <- rbind(aug_data, nowcasted_data)

  # Return the dadnow object
  # This started with much fewer entries, but adding things here
  # made it easier to add new features elsewhere.
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
        model_name = nowcasted_data$model[1],
        formula = formula,
        prepped_data = prepped_data,
        nowcasted_data = nowcasted_data,
        model = nowcast$model,
        predictions = nowcast$prediction,
        fitted_values = nowcast$fitted_values,
        evals = enbpi$evals,
        enbpi_se = enbpi$se,
        params = params
      )
    )
  )

  # Model ids are based on the evaluations table.
  # Note that model ids can change as things are added to the dadnow object.
  model_ids <- make_model_id(enbpi$evals)
  names(dadnow$models) <- model_ids
  # Ensure the internal model ids are updated.
  for (i in seq_along(dadnow$models)) {
    dadnow$models[[i]]$model_id <- model_ids[i]
  }

  class(dadnow) <- "multidadnow"
  dadnow
}

make_model_id <- function(evals) {

  all_formulas <- paste0("f", match(evals$formula, unique(evals$formula)))
  all_models <- evals$model
  model_ids <- paste0(all_models, "_", all_formulas)

  param_set <- ave(
    seq_len(nrow(evals)),
    all_models,
    all_formulas,
    FUN = seq_along
  )

  # If a model/formula is repeated, add "a", "b", etc. to the end of the model id.
  suffix <- ifelse(param_set > 1, letters[param_set], "")
  model_ids <- paste0(model_ids, suffix)

  model_ids
}

#' Choose a model to use for nowcasting
#'
#' @param model The model to use for nowcasting. If not specified, the function attempts to find a function called `model`. See `vignette("Write_fit_XX_functions", package = "dadnowcast")` for how to write your own model for use in this package.
dispatch_model <- function(model) {
  switch(model,
    "lm" = fit_lm,
    "ar" = fit_ar,
    "arx" = fit_arx,
    "arima" = fit_arima,
    "gam" = fit_gam,
    "kf" = fit_kf,
    "rf" = fit_rf,
    "xgboost" = fit_xgboost,
    "mechanistic" = fit_mechanistic,
    model
  )
}
