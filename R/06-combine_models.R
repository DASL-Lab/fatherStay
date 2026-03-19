
#' Add a dadnow to a multidadnow object, or combine two multidadnow objects
#'
#' @param dadnow1,dadnow2 A dadnow or multidadnow object.
#'
#' @returns A single multidadnow object.
#' @export
combine_dadnow <- function(dadnow1, dadnow2) {
  data <- rbind(dadnow1$data, dadnow2$data)
  data <- data[!duplicated(data), ]

  evals1 <- dadnow1$evals
  evals2 <- dadnow2$evals
  rownames(evals1) <- NULL
  rownames(evals2) <- NULL
  evals <- rbind(evals1, evals2)
  rownames(evals) <- make_model_id(evals)

  if (inherits(dadnow1, "dadnow") && inherits(dadnow2, "dadnow")) {
    return_value <- list(
      date_col = dadnow1$date_col,
      data = data,
      evals = evals,
      models = list(dadnow1, dadnow2)
    )
    names(return_value$models) <- c(dadnow1$model_id, dadnow2$model_id)
    class(return_value) <- "multidadnow"
  } else if (inherits(dadnow1, "multidadnow") && inherits(dadnow2, "dadnow")) {
    models <- c(dadnow1$models, list(dadnow2))
    names(models)[length(models)] <- dadnow2$model_id
    return_value <- list(
      date_col = dadnow1$date_col,
      data = data,
      evals = evals,
      models = models
    )
    class(return_value) <- "multidadnow"
  } else if (inherits(dadnow1, "dadnow") && inherits(dadnow2, "multidadnow")) {
    models <- c(list(dadnow1), dadnow2$models)
    names(models)[1] <- dadnow1$model_id
    return_value <- list(
      date_col = dadnow2$date_col,
      data = data,
      evals = evals,
      models = models
    )
    class(return_value) <- "multidadnow"
  } else if (inherits(dadnow1, "multidadnow") && inherits(dadnow2, "multidadnow")) {
    models <- c(dadnow1$models, dadnow2$models)
    names(models) <- c(names(dadnow1$models), names(dadnow2$models))
    return_value <- list(
      date_col = dadnow1$date_col,
      data = data,
      evals = evals,
      models = models
    )
    class(return_value) <- "multidadnow"
  } else {
    stop("Both inputs must be of class 'dadnow' or 'multidadnow'.")
  }

  for (i in seq_along(return_value$models)) {
    return_value$models[[i]]$model_id <- rownames(return_value$evals)[i]
  }
  
  return(return_value)
}


