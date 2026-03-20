#' Get the underlying model from a dadnow object
#' 
#' @param dadnow A dadnow object.
#' 
#' @returns The underlying model, in it's original form.
#' @export
get_model <- function(dadnow, model) {
  if (length(model) != 1) {
    stop("Only one model can be extracted from a dadnow object.")
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
      lower = x$enbpi[, 1],
      upper = x$enbpi[, 2],
      model_id = x$model_id
    )
    rownames(df) <- NULL
    df
  })
  preds <- do.call(rbind, preds)
  rownames(preds) <- dadnow$data$dates_nowcast

  preds
}
