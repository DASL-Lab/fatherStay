#' Extract a model (or models) from a dadnow object
#' 
#' @param dadnow A dadnow object.
#' @param models A model name or vector of model names. Must correspond to the names of the models seen in the evaluations table. Note that the model names can change depending on the other models added to the dadnow object, so always double check.
#' 
#' @returns A dadnow object with the specified models.
#' @export
extract <- function(dadnow, models) {
  if (!all(models %in% names(dadnow$models))) {
    print(names(dadnow$models))
    stop(paste0("Model(s) ", paste(models[!models %in% names(dadnow$models)], collapse = ", "), " not found in dadnow object."))
  }

  if (length(models) == 1) {
    dadnow$models <- list(dadnow$models[[models]])
  } else {
    dadnow$models <- dadnow$models[models]
  }
  dadnow$evals <- dadnow$evals[rownames(dadnow$evals) %in% models, ]
  names(dadnow$models) <- models

  dadnow
}

#' Extract the best model (or models) from a dadnow object
#' 
#' @param dadnow A dadnow object.
#' @param metric The metric to use for evaluation. Defaults to "rmse".
#' @param top_n The number of top models to return. Defaults to 1.
#' 
#' @returns A dadnow object with the specified models.
#' @export
extract_best <- function(dadnow, metric = "rmse", top_n = 1) {
    
  evals <- dadnow$evals
  
  if (metric == "rmse") {
    evals <- evals[order(evals$rmse), ]
  } else if (metric == "mae") {
    evals <- evals[order(evals$mae), ]
  } else if (metric == "mre") {
    evals <- evals[order(evals$mre), ]
  } else {
    stop("Metric must be \"rmse\", \"mae\", or \"mre\".")
  }
  
  if (top_n > nrow(evals)) {
    top_n <- nrow(evals)
  }

  best_models <- rownames(evals)[1:top_n]
  dadnow <- extract(dadnow, best_models)
  dadnow
}