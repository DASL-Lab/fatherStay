#' Get the underlying model from a dadnow object
#'
#' @param dadnow A dadnow object.
#'
#' @returns The underlying model, in it's original form.
#' @export
get_model <- function(dadnow, model) {
  if (length(model) != 1) {
    stop("Only one model can be extracted from a dadnow object at a time.")
  }
  dadnow$models[[model]]$model
}

#' Get the predictions from a dadnow object
#'
#' @param dadnow A dadnow object.
#' @param models A model name or vector of model names. Must correspond to the names of the models seen in the evaluations table. Note that the model names can change depending on the other models added to the dadnow object, so always double check.
#'
#' @returns A matrix of predictions, with rows as dates and columns as models.
#' @export
get_predictions <- function(dadnow, models = NULL) {
  if (!is.null(models)) {
    dadnow <- extract(dadnow, models)
  }

  preds <- lapply(dadnow$models, function(x) {
    df <- data.frame(
      date = x$prepped_data$dates_nowcast,
      prediction = as.numeric(x$prediction$prediction),
      lower = x$prediction$lower,
      upper = x$prediction$upper,
      model_id = x$model_id
    )
    rownames(df) <- NULL
    df
  })
  preds <- do.call(rbind, preds)
  rownames(preds) <- dadnow$data$dates_nowcast

  preds
}

#' Get the residuals from each model in a dadnow object
#' 
#' @param dadnow A dadnow object.
#' 
#' @returns A data frame with the fitted values and residuals.
#' @export
get_residuals <- function(dadnow) {
  
  residuals <- lapply(dadnow$models, function(x) {
    df <- data.frame(
      date = x$prepped_data$dates_train,
      actual_value = as.numeric(x$prepped_data$y_train),
      fitted_value = as.numeric(x$fitted_values),
      model_id = x$model_id
    )
    df$residual <- df$actual_value - df$fitted_value
    rownames(df) <- NULL
    df
  })
  residuals <- do.call(rbind, residuals)
  rownames(residuals) <- NULL

  
  residuals
}

#' Get the data with predictions from a dadnow object
#'
#' @param dadnow A dadnow object.
#' @param training A logical indicating whether to return the training data. If FALSE, then only the nowcasts are returned.
#' 
#' @returns A data frame with the predictions.
#' @export
get_data <- function(dadnow, include_training = TRUE) {
  if (include_training) {
    df <- dadnow$data
  } else {
    df <- dadnow$data[dadnow$data$model != "Training", ]
  }

  df$model_id <- NULL
  for (model in dadnow$models) {
    param_string <- paste0(names(model$params), model$params, collapse = "_")
    condition <- df$model == model$model_name & df$formula == model$formula & df$params == param_string
    if (any(condition)) {
      df[condition, "model_id"] <- model$model_id
    }
  }
  df
}
  