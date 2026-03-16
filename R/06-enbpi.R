#' Ensemble batch prediction intervals and prediction metrics
#' 
#' @param dadnow A dadnow object.
#' @param k The number of steps ahead to predict.
#' @param train_window The number of days to use for training.
#' @param level The prediction interval level.
#' @param batches The number of batches to use for training.
#'
#' @returns A dadnow object with the ensemble predictions and intervals added.
#' @export
enbpi <- function(X_train, y_train, model, formula, params, k, batches = 40, train_window = NULL, level = 0.95) {

  if (is.null(train_window)) train_window <- floor(0.6 * length(y_train))
  
  train_indices <- sample(1:(length(y_train) - k - train_window), batches, replace = FALSE)

  preds <- vector(mode = "list", length = batches)
  rmse <- vector(mode = "numeric", length = batches)
  mae <- vector(mode = "numeric", length = batches)
  mre <- vector(mode = "numeric", length = batches)
  for (i in 1:batches) {
    # Get the training data
    X_train_k <- X_train[train_indices[i]:(train_indices[i] + train_window - 1), ]
    y_train_k <- y_train[train_indices[i]:(train_indices[i] + train_window - 1)]
    
    # Get the test data
    X_test_k <- X_train[(train_indices[i] + train_window):(train_indices[i] + k + train_window), ]
    y_test_k <- y_train[(train_indices[i] + train_window):(train_indices[i] + k + train_window)]

    # Fit the model
    preds[[i]] <- dispatch_model(model)(X_train = X_train_k, Y_train = y_train_k, X_nowcast = X_test_k, params = params)$prediction
    rmse[i] <- sqrt(mean((y_test_k - preds[[i]])^2))
    mae[i] <- mean(abs(y_test_k - preds[[i]]))
    mre[i] <- mean(((y_test_k - preds[[i]]) / (y_test_k + 0.1))^2)
  }

  preds_mat <- matrix(unlist(preds), nrow = k, byrow = FALSE)
  preds_ci <- apply(preds_mat, 1, function(x) quantile(x, c((1 - level)/2, 1 - (1 - level)/2)))

  evals = data.frame(
    "formula" = deparse(formula),
    "model" = model,
    "params" = paste0(names(params), params, collapse = "_"),
    "rmse" = mean(rmse),
    "mae" = mean(mae),
    "mre" = mean(mre)
  )

  list(enbpi = t(preds_ci), evals = evals)
}
