#' Nowcast from a simple linear model (internal)
#'
#' @param NewX The known x values to be nowcast on.
#' @param model The model to use for predictions.
now_simple_lm <- function(newX, model) {
  predict.lm(model, newdata = list(x = newX))
}

#' Nowcast from a simple AR model (internal)
#'
#' @param NewX The known x values to be nowcast on.
#' @param model The model to use for predictions.
now_simple_arx <- function(newX, model) {
  predict(model, newxreg = newX)
}

#' Return the number of NA's at the end of a vector (internal)
#'
#' @param y A vector of values with some number of NA's at the end of it.
find_nas <- function(y) {
  which(rev(y) != "NA") |>
    min() - 1
}

#' Perform nowcasting for the specified response variable
#'
#' @param formula y ~ x
#' @param data A data frame with columns used in the formula.
#' @param model Currently implemented: "lm" and "ar"
#' @param order The AR order to be used in `arima()`.
#'
#' @returns Predictions for the missing data in the response variable

now_simple_models <- function(
  formula, data,
  model = c("lm", "ar")[1],
  order = NULL
) {
  y <- data[, all.vars(formula[[2]])]
  x <- data[, all.vars(formula[[3]])]

  # find the number of NA values at the end of the response variable
  naCount <- find_nas(y)

  # find the length of the non-NA values in the response variable
  numNonNa <- (length(y) - naCount)

  # get a data frame for the non-NA response values
  shortData <- data[1:numNonNa, ]

  # create the model to do predictions from
  modToUse <- fit_simple_model(formula, shortData, model, order)

  # get a data frame for the covariates of when the response variable is NA
  newX <- x[(numNonNa + 1):length(x)]

  # perform the nowcasting
  switch(model,
    "lm" = now_simple_lm(newX, modToUse),
    "ar" = now_simple_arx(newX, modToUse)
  )
}