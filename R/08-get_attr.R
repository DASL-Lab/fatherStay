#' Get the underlying model from a dadnow object
#'
#' @param dadnow A dadnow object.
#' @param model_id The model id to use for the model.
#'
#' @returns The underlying model, in it's original form.
#' @export
get_model <- function(dadnow, model_id) {
  if (length(model_id) != 1) {
    stop("Only one model can be extracted from a dadnow object at a time.")
  }
  dadnow$models[[model_id]]$model
}

#' Get the formula used in the underlying model from a dadnow object
#'
#' @param dadnow A dadnow object.
#' @param model_id The model id to use for the model.
#'
#' @returns The underlying model, in it's original form.
#' @export
get_formula <- function(dadnow, model_id) {
  if (length(model_id) != 1) {
    stop("Only one model can be extracted from a dadnow object at a time.")
  }
  dadnow$models[[model_id]]$formula
}


#' Get the predictions from a dadnow object
#'
#' @param dadnow A dadnow object.
#' @param model_ids A model name or vector of model names. Must correspond to the model ids seen in the evaluations table. Note that the model names can change depending on the other models added to the dadnow object, so always double check.
#'
#' @returns A matrix of predictions, with rows as dates and columns as models.
#' @export
get_predictions <- function(dadnow, model_ids = NULL) {
  if (!is.null(model_ids)) {
    dadnow <- extract(dadnow, model_ids)
  }

  get_data(dadnow, include_training = FALSE)
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

  nowcasted_data <- lapply(dadnow$models, function(x) {
    df <- x$nowcasted_data
    df$model_id <- x$model_id
    df
})
  nowcasted_data <- do.call(rbind, nowcasted_data)
  if (include_training) {
    train <- dadnow$data[1:length(dadnow2$models[[1]]$prepped_data$dates_train), ]
    train$model_id = NA
    nowcasted_data <- rbind(train, nowcasted_data)
  }
  nowcasted_data
  }

#' Get the evaluation metrics from a dadnow object
#'
#' @param dadnow A dadnow object.
#' @param sort The metric to sort by. Defaults to "rmse",but can also be "mae" or "mre" or NULL.
#'
#' @returns A data frame with the evaluation metrics.
#' @export
get_evals <- function(dadnow, sort = "rmse") {
  evals <- dadnow$evals
  evals$model_id <- rownames(evals)
  if (!is.null(sort)) {
    return(evals[order(evals[, sort]), ])
  } else {
    return(evals)
  }
}
