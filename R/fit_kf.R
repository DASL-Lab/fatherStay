#' Fit a Kalman Filter model on given data and make predictions for a given set of data
#'
#' @param X_Train Training data for the explanatory variables in the model
#' @param Y_Train Training data for the response variable
#' @param X_Nowcast Current data of X_train for which there is no Y_train data, used to make nowcasts
#' @param params Named list containing additional parameters: `degree` For trend component, integer defining the degree of the polynomial trend
#'
#' @returns A named list containing: Kalman Filter model object, predictions, and fitted values

fit_kf <- function(Y_train, X_train = NULL, X_nowcast = NULL,
                   params = list(degree = 1)) {
  # check if the package "KFAS" is installed and stops if it isn't
  if (!requireNamespace("KFAS", quietly = TRUE)) {
    stop("Package \"KFAS\" must be installed to use this function.")
    return(list(model = NULL, prediction = NULL))
  }

  # retrieves the variables from `params` and if they are missing uses default values
  if (!"degree" %in% names(params)) {
    degree <- 1
  } else {
    degree <- params$degree
  }

  # to ensure the data is in a consistent format
  Y_train <- ts(data.frame(Y_train))
  X_train <- data.frame(X_train)
  data <- data.frame(X_train, Y_train)

  # creating the formula
  formulaToUse <- "~"
  for (i in seq(ncol(X_train))) {
    formulaToUse <- paste0(formulaToUse, colnames(X_train)[i], "+")
  }
  formulaToUse <- substr(formulaToUse, 1, (nchar(formulaToUse) - 1))
  formulaToUse <- as.formula(formulaToUse)
  YFormula <- colnames(Y_train)

  # fitting the model
  # for some reason using KFAS::SSMtrend() and KFAS::SSMregression() makes this break
  SSMtrend <- KFAS::SSMtrend
  SSMregression <- KFAS::SSMregression
  SMod <- KFAS::SSModel(Y_train ~ SSMtrend(degree = 1, Q = list(matrix(NA)))
    + SSMregression(formulaToUse, data = data))

  # this finds the estimates for the unknown parameters
  fitMod <- KFAS::fitSSM(SMod, inits = c(1, 1, 1), method = "BFGS")$model

  # new data wrangling!
  newn <- length(X_nowcast[, 1])

  newY <- rep(NA, newn)

  newData <- cbind(X_nowcast, newY)

  # create a new SMod object for the new data for predictions
  newMod <- KFAS::SSModel(
    newY ~ SSMregression(formulaToUse,
      Q = fitMod$Q,
      data = newData
    ),
    H = fitMod$H
  )

  # a SMod object of the training data that will be used to get the fitted values
  oldMod <- KFAS::SSModel(
    Y_train ~ SSMregression(formulaToUse,
      Q = fitMod$Q,
      data = data
    ),
    H = fitMod$H
  )

  # generate the predictions
  prediction <- data.frame(prediction = as.numeric(predict(fitMod, newdata = newMod)))

  # generate the fitted values
  fits <- predict(fitMod, newdata = oldMod)

  # ensure output is in the correct named list format
  list(model = fitMod, prediction = prediction, fitted_values = fits)
}