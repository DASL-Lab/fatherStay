#' Ensemble batch prediction intervals and prediction metrics
#' 
#' @param X_train A data frame of training data.
#' @param y_train A vector of training data.
#' @param model The model to use for nowcasting.
#' @param formula A formula object.
#' @param params The parameters to use for the model.
#' @param k The number of steps ahead to predict. If NULL, the number of steps is determined by the amount of data to be nowcast.
#' @param batches The number of batches to use for training (akin to the number of folds for k-fold cross validation).
#' @param train_window The number of days to use for training. Defaults to 60% of the training data, which allows for a large training set for each batch while also allowing for a reasonable amount of variation in the test sets.
#' @param level The prediction interval level.
#'
#' @returns A dadnow object with the ensemble predictions and intervals added.
enbpi <- function(X_train, y_train, model, formula, params, k, batches = 40, train_window = NULL, level = 0.95) {

  if (is.null(train_window)) train_window <- floor(0.6 * length(y_train))
  

  train_indices <- sample(1:(length(y_train) - k - train_window), batches, replace = TRUE)

  preds <- vector(mode = "list", length = batches)
  rmse <- vector(mode = "numeric", length = batches)
  mae <- vector(mode = "numeric", length = batches)
  mre <- vector(mode = "numeric", length = batches)
  for (i in 1:batches) {
    # Get the training data
    X_train_k <- X_train[train_indices[i]:(train_indices[i] + train_window), ]
    y_train_k <- y_train[train_indices[i]:(train_indices[i] + train_window)]
    
    # Get the test data
    X_test_k <- X_train[(train_indices[i] + train_window + 1):(train_indices[i] + k + train_window), ]
    y_test_k <- y_train[(train_indices[i] + train_window + 1):(train_indices[i] + k + train_window)]

    # Fit the model
    preds[[i]] <- dispatch_model(model)(X_train = X_train_k, Y_train = y_train_k, X_nowcast = X_test_k, params = params)$prediction$prediction
    
    rmse[i] <- sqrt(mean((y_test_k - preds[[i]])^2))
    mae[i] <- mean(abs(y_test_k - preds[[i]]))
    mre[i] <- mean(((y_test_k - preds[[i]]) / (y_test_k + 0.1))^2)
  }

  preds_mat <- matrix(unlist(preds), nrow = k, byrow = FALSE)
  preds_ci <- apply(preds_mat, 1, function(x) quantile(x, c((1 - level)/2, 1 - (1 - level)/2)))

  if (model %in% c("ar", "arx")) {
    model <- paste0(model, params$order)
  } else if (model == "mechanistic") {
    model <- paste0("mech_", params$method)
  }

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
