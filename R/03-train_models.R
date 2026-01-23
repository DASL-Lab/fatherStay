#' Train a model for nowcasting
#'
#' @param dadnow A dadnow object.
#' @param quiet If TRUE, suppresses output.
#'
#' @returns A dadnow object with the model trained.
#' @export
train_model <- function(dadnow, quiet = FALSE) {
  # Check the model is valid
  if (!all(dadnow$model %in% c(
    "lm", "ar"
  ))) {
    stop("Invalid model.")
  }

  for (model in dadnow$model) {
    res <- switch(model,
      "lm" = fit_LM(X_train = as.matrix(dadnow$X_train), Y_train = dadnow$y_train, X_nowcast = dadnow$X_nowcast),
      "ar" = fit_ARX(X_train = as.matrix(dadnow$X_train), Y_train = dadnow$y_train, X_nowcast = dadnow$X_nowcast, p = dadnow$order)
    )
    dadnow[[paste0("trained_", model)]] <- res$model
    dadnow[[paste0("nowcast_", model)]] <- res$prediction
    # TODO: Standard errors / prediction intervals
  }

  dadnow
}

train_lm <- function(X, y) {
  newdf <- data.frame(y = y, X)
  lm(y ~ ., data = newdf)
}

train_ar <- function(X, y, order) {
  arima(y, order = c(order, 0, 0), xreg = X)
}
