#' Fit a basic indicated model
#'
#' @param formula A formula indicating which variables to use in the model
#' @param model A string indicating the type of model to be fit to the data
#' @param data A data frame containg at least the data to be used
#'
#' @return The specified model based on the given formula
#'
#' @export
#'
#' @examples
#' y <- rnorm(100)
#' x <- seq(1,100)
#' df <- data.frame(x,y)
#' fit-basicModels(y ~ x, model = "AR", df)

fit_basicModels <- function(formula, model, data) {
  # obtain variables for the left and righthand side of the given formula
  lhs <- formula[[2]]
  rhs <- formula[[3]]

  # this works to detect if the left hand side value is in the data frame!
  if (as.character(lhs) %in% colnames(data)) {
    lhs_data <- data[, which(colnames(data) == lhs)]
  } else {
    paste("Variable", lhs, "not found")
  }

  # determine the number of variables in the righthand side of the formula
  rhsVars <- all.vars(rhs)
  numRhsVars <- length(rhsVars)
  
  # detect if all variables used in the righthand side are in the given data
  rhs_data <- data.frame(matrix(nrow = length(lhs_data), ncol = numRhsVars))
  
  for (i in seq(1:numRhsVars)) {
    if (as.character(rhsVars[i]) %in% colnames(data)) {
      rhs_data[,i] <- data[, which(colnames(data) == rhsVars[i])]
    } else {
      paste("Variable", lhs, "not found")
    }
  }
  
  
}
