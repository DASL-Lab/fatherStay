#' Ensemble batch prediction intervals and prediction metrics
#'
#' EnbPI takes `batches` of training data, fits a model to each batch, and then uses the residuals to calculate prediction intervals and prediction metrics. This targets the exact standard errors that we're looking for. If we're doing 8-step ahead nowcasting, we need an estimate of the standard error for the eighth step ahead, not just the general standard error for predictions. This is a model-agnostic approach to calculating prediction intervals.
#'
#' @param X_train A data frame of training data.
#' @param y_train A vector of training data.
#' @param model The model to use for nowcasting.
#' @param formula A formula object.
#' @param params The parameters to use for the model.
#' @param k The number of steps ahead to predict. If NULL, the number of steps is determined by the amount of data to be nowcast.
#' @param batches The number of batches to use for training (akin to the number of folds for k-fold cross validation).
#' @param train_window The number of days to use for training. Defaults to 60% of the training data, which allows for a large training set for each batch while also allowing for a reasonable amount of variation in the test sets.
#'
#' @returns A list containing the k-step ahead prediction standard errors and the evaluations of the models for the k-step ahead predictions.
enbpi <- function(X_train, y_train, model, formula, params, k, batches = 40, train_window = NULL, se = TRUE) {

  # Return NAs if enbpi is not used.
  if (!se) {
    
    evals = data.frame(
      "formula" = deparse(formula),
      "model" = model,
      "params" = paste0(names(params), params, collapse = "_"),
      "rmse" = NA,
      "mae" = NA,
      "mre" = NA
    )

    return(list(se = rep(NA, k), evals = evals))
  }

  # If the train_window is not specified, set it to 60% of the training data
  if (is.null(train_window)) train_window <- floor(0.6 * length(y_train))
  
  # There needs to be at least 5 possible indices to start a window on.
  if (length(y_train) - k - train_window < 5) {
    train_window <- length(y_train) - k - 5
    warning(
      paste0(
        "There were fewer than 5 possible indices to start an ensemble batch prediction interval.",
        "\n  Setting train_window to:",
        "\n     length_of_training_data - number_of_steps_to_predict - 5 = ",
        train_window, "\n  Resultant prediction intervals and evaluation metrics may be unreliable."
      )
    )
  }

  # Indices must be sampled such that the test data is still within the training data
  train_indices <- sample(1:(length(y_train) - k - train_window), batches, replace = TRUE)

  if (any(train_indices < 1)) {
    stop(
      paste0(
        "The training data minus the k-steps minus the size of the training window is ",
        length(y_train), " - ", k, " - ", train_window, " = ",
        length(y_train) - k - train_window, ".",
        "Set train_window to a smaller value or set enbpi to FALSE."
      )
    )
  }

  preds <- vector(mode = "list", length = batches)
  resids <- vector(mode = "list", length = batches)
  rmse <- vector(mode = "numeric", length = batches)
  mae <- vector(mode = "numeric", length = batches)
  mre <- vector(mode = "numeric", length = batches)
  for (i in 1:batches) {
    # Get the training data
    to_train <- train_indices[i]:(train_indices[i] + train_window)
    X_train_k <- X_train[to_train, , drop = FALSE]
    y_train_k <- y_train[to_train]

    # Get the test data
    to_test <- (train_indices[i] + train_window + 1):(train_indices[i] + k + train_window)
    X_test_k <- X_train[to_test, , drop = FALSE]
    y_test_k <- y_train[to_test]

    # Fit the model and get residuals
    preds[[i]] <- dispatch_model(model)(X_train = X_train_k, Y_train = y_train_k, X_nowcast = X_test_k, params = params)$prediction$prediction
    resids[[i]] <- y_test_k - preds[[i]]

    rmse[i] <- sqrt(mean((resids[[i]])^2))
    mae[i] <- mean(abs(resids[[i]]))
    mre[i] <- mean(((resids[[i]]) / (y_test_k + 0.1))^2)
  }

  # Get the standard deviation of the residuals for each step ahead.
  resids_mat <- matrix(unlist(resids), nrow = k, byrow = FALSE)
  resids_se <- apply(resids_mat, 1, function(x) sd(x))

  # Dealing with the model name.
  # For AR, and "AR with order 1" is an AR(1) (similar for ARX)
  # For mechanistic, it's important to note the method used.
  # This is done here because the rows of the evals table are used to tell models apart elsewhere.
  if (model == "mechanistic") {
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

  list(se = resids_se, evals = evals)
}
