
#' Add a dadnow to a multidadnow object, or combine two multidadnow objects
#'
#' @param dadnow1,dadnow2 A dadnow or multidadnow object.
#'
#' @returns A single multidadnow object.
#' @export
combine_nowcasts <- function(dadnow1, dadnow2) {
  data <- rbind(dadnow1$data, dadnow2$data)
  data <- data[!duplicated(data), ]

  evals1 <- dadnow1$evals
  evals2 <- dadnow2$evals
  rownames(evals1) <- NULL
  rownames(evals2) <- NULL
  evals <- rbind(evals1, evals2)
  model_ids <- make_model_id(evals)
  rownames(evals) <- model_ids

  models <- c(dadnow1$models, dadnow2$models)

  return_value <- list(
    date_col = dadnow1$date_col,
    data = data,
    evals = evals,
    models = models
  )
  class(return_value) <- "multidadnow"

  names(return_value$models) <- model_ids
  for (i in seq_along(return_value$models)) {
    return_value$models[[i]]$model_id <- model_ids[i]
  }
  
  return(return_value)
}


