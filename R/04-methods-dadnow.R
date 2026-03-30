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
  print(get_evals(dadnow, sort = "rmse"))
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
