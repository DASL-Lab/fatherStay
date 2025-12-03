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
  
  naCount <- sum(is.na(y))
  shortData <- data[1:(length(y)-naCount),]
  
  modToUse <- fit_basic_models(formula, shortData, model,order)
  
  # create predict part here that predicts naCount ahead
}