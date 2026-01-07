#' Train a model for nowcasting
#'
#' @param dadnow A dadnow object.
#' @param quiet If TRUE, suppresses output.
#'
#' @returns A dadnow object with the model trained.
train_model <- function(dadnow, quiet = FALSE) {
  # Check the model is valid
  if (!all(dadnow$model %in% c(
    "lm", "ar"
  ))) {
    stop("Invalid model.")
  }

  for (model in dadnow$model) {
    res <- switch(model,
      "lm" = train_lm(dadnow$X_train, dadnow$y_train),
      "ar" = train_ar(dadnow$X_train, dadnow$y_train, dadnow$order)
    )
    dadnow[[paste0("trained_", model)]] <- res
  }
}

train_lm <- function(X, y) {
  formula <- y ~ X
  lm(formula)
}

train_ar <- function(X, y, order) {
  data <- cbind(y, X)
  arima(y, order = c(order, 0, 0), xreg = X)
}
