#' Print a dadnow object
#'
#' @param dadnow A dadnow object.
#'
#' @returns A dadnow object invisibly.
#' @export
print.dadnow <- function(dadnow) {
  cat("Date column:", dadnow$date_col, "\n")
  cat(
    "Training date range:",
    format(min(lubridate::ymd(dadnow$prepped_data$dates_train)), "%Y-%m-%d"), "to",
    format(max(lubridate::ymd(dadnow$prepped_data$dates_train)), "%Y-%m-%d"), "\n"
  )

  cat("\nTest set evaluation metrics:\n")
  print(dadnow$evals)
  if (any(dadnow$data$y_test == 0)) {
    cat("Note: There are zeros in the test set, which affects the mean relative error.\n")
  }
  
  #trained_models <- names(dadnow)[grepl("nowcast_", names(dadnow))]
  #if (length(trained_models) == 0) trained_models <- "None"
  #cat("Nowcasted models:", gsub("nowcast_", "", trained_models), "\n")

  #for (model in trained_models) {
  #  cat(paste0(model, ":"))
  #  print(dadnow[[model]])
  #  cat("\n")
  #}

  invisible(dadnow)
}

#' Print a dadnow object
#'
#' @param dadnow A dadnow object.
#'
#' @returns A dadnow object invisibly.
#' @export
print.multidadnow <- function(dadnow) {
  cat("Date column:", dadnow$date_col, "\n")
  cat(
    "Training date range:",
    format(min(lubridate::ymd(dadnow$data[, dadnow$date_col])), "%Y-%m-%d"), "to",
    format(max(lubridate::ymd(dadnow$data[, dadnow$date_col])), "%Y-%m-%d"), "\n\n"
  )

  cat("\nTest set evaluation metrics:\n")
  all_evals <- do.call(rbind, lapply(dadnow$models, function(x) x$evals))
  rownames(all_evals) <- NULL
  print(all_evals[order(all_evals$rmse), ])
  if (any(dadnow$models[[1]]$prepped_data$y_test == 0)) {
    cat("Note: There are zeros in the test set, which affects the mean relative error.\n")
  }

  invisible(dadnow)
}

#' Summarize a dadnow object
#' 
#' Currently a placeholder.
#'
#' @param dadnow A dadnow object.
#'
#' @returns A dadnow object invisibly.
#' @export
summary.dadnow <- function(dadnow) {
  cat(names(dadnow))

  invisible(dadnow)
}
