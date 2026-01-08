#' Print a dadnow object
#'
#' @param dadnow A dadnow object.
#'
#' @returns A dadnow object invisibly.
#' @export
print.dadnow <- function(dadnow) {
  cat("Formula:", deparse(dadnow$formula), "\n")
  cat("Model:", dadnow$model, "\n")
  cat("Date column:", dadnow$date_col, "\n")
  cat("AR Order:", dadnow$order, "\n")
  cat("Require imputation:", dadnow$require_imputation, "\n")
  cat(
    ifelse(all(is.na(dadnow$y_nowcast)),
      "Nowcasting has not been performed.\n",
      "Nowcasting has been performed.\n"
    )
  )
  trained_models <- names(dadnow)[grepl("trained_", names(dadnow))]
  if (length(trained_models) == 0) trained_models <- "None"
  cat("Trained models:", gsub("trained_", "", trained_models), "\n")
  invisible(dadnow)
}

#' Summairze a dadnow object
#' 
#' Currently a placeholder.
#'
#' @param dadnow A dadnow object.
#'
#' @returns A dadnow object invisibly.
#' @export
summary.dadnow <- function(dadnow) {
  cat("dadnow object\n")
  cat("Formula:", deparse(dadnow$formula), "\n")
  cat("Model:", dadnow$model, "\n")
  cat("Date column:", dadnow$date_col, "\n")
  cat("Order:", dadnow$order, "\n")
  cat("Require imputation:", dadnow$require_imputation, "\n")

  invisible(dadnow)
}
