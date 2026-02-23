#' Fit a Random Forest model on given data and make predictions for a given set of data
#'
#' @param X_Train Training data for the explanatory variables in the model
#' @param Y_Train Training data for the response variable
#' @param X_Nowcast Data to make predictions bases on
#' @param ntree Integer indicating the number of trees in the random forest, default is 500
#' @param mtry Indicates the number of features to try in each node of the Random Forest. Default is the default for randomForest
#' @param weights A vector of length same as y that are positive weights used only in sampling data to grow each tree (not used in any other calculation)
#' @param replace Should sampling of cases be done with or without replacement?
#' @param maxnodes Maximum number of terminal nodes trees in the forest can have.
#'
#' @returns Random Forest object and predictions
#' @export

fit_RF <- function(Y_train, X_train = NULL, X_nowcast = NULL,
                   params = list(ntree = 500, mtry = NULL, weights = NULL, replace = TRUE, maxnodes = NULL)) {
  if (!requireNamespace("randomForest", quietly = TRUE)) {
    paste("Package \"randomForest\" must be installed to use this function.")
    return(list(model = NULL, prediction = NULL))
  }

  Y_train <- data.frame(Y_train)
  X_train <- data.frame(X_train)

  formulaToUse <- paste0(colnames(Y_train), "~")
  for (i in seq_len(ncol(X_train))) {
    formulaToUse <- paste0(formulaToUse, colnames(X_train)[i], "+")
  }
  formulaToUse <- substr(formulaToUse, 1, (nchar(formulaToUse) - 1))
  formulaToUse <- as.formula(formulaToUse)

  data <- data.frame(X_train, Y_train)

  if (is.null(params$ntree)) {
    ntree <- 500
  } else {
    ntree <- params$ntree
  }

  if (is.null(params$mtry)) {
    mtry <- if(!is.null(Y_train) && !is.factor(Y_train)) {
      max(floor(ncol(X_train) / 3), 1)
    } else {
      floor(sqrt(ncol(X_train)))
    }
  } else {
    mtry <- params$mtry
  }

  if (is.null(params$weights)) {
    weights <- NULL
  } else {
    weights <- params$weights
  }

  if (is.null(params$replace)) {
    replace <- TRUE
  } else {
    replace <- params$replace
  }

  if (is.null(params$maxnodes)) {
    maxnodes <- NULL
  } else {
    maxnodes <- params$maxnodes
  }

  RFModel <- randomForest::randomForest(formulaToUse,
    data = data, ntree = ntree, mtry = mtry, weights = weights,
    replace = replace, maxnodes = maxnodes,
    na.action = na.omit
  )

  predictions <- predict(RFModel, newdata = X_nowcast)

  list(model = RFModel, prediction = predictions)
}
