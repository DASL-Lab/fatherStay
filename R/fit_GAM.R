#' Fit a spline model on given data and make predictions for a given set of data
#'
#' @param X_Train Training data for the explanatory variables in the model
#' @param Y_Train Training data for the response variable
#' @param X_Nowcast Data to make predictions bases on
#' @param family Model family, as used in `mgcv::gam()`. Defaults to `gaussian` and accepts any family that is accepted by `mgcv::gam()`.
#' @param smooths Named list of smoothing parameters for each variable, e.g. `list("x1" = 10, "x2")` will result in a formula `s(x1, bs = "tp", k = 10) + s(x2, bs = "tp", k = -1) + ...`. If a smoothing parameter is not specified, it will default to -1, which will result `mgcv::gam()` choosing the smoothing parameter.
#' @param bs Basis smoothing function, as used in `mgcv::gam()`. Default `tp`.
#'
#' @returns GAM model object and predictions
#' @export

fit_GAM <- function(
  Y_train, X_train = NULL, X_nowcast = NULL,
  family = gaussian, smooths = list(), bs = "tp"
) {
  full_data <- as.data.frame(cbind(Y_train, X_train))

  if (length(smooths) > 0) {
    formula_terms <- vector(mode = "list", length = length(smooths))
    for (smooth in names(smooths)) {
      if (!smooth %in% colnames(full_data)) {
        stop(paste0("Smoothing variable ", smooth, " not found in data."))
      }
      if (is.null(smooths[[smooth]])) smooths[[smooth]] <- -1
      formula_term <- paste0(
        "s(", smooth, ", bs = '", bs, "', k = ", smooths[[smooth]], ")"
      )
      formula_terms[[smooth]] <- formula_term

      formula <- paste0("Y_train ~", paste(formula_terms, collapse = " + "))
    }

    for (term in colnames(full_data)[-1]) {
      if (!term %in% names(smooths)) {
        formula <- paste0(formula, "+", term)
      }
    }
  } else {
    formula <- paste0(
      "Y_train ~",
      paste0(colnames(full_data)[-1], collapse = " + ")
    )
  }

  formula <- as.formula(formula)

  fitted_GAM <- mgcv::gam(formula, data = full_data, family = family)

  XNowcast <- as.data.frame(X_nowcast)

  colnames(XNowcast) <- colnames(full_data)[-1]

  predicted_GAM <- predict(fitted_GAM, newdata = XNowcast)

  list(model = fitted_GAM, prediction = predicted_GAM)
}
