#' Prepare the data for analysis, returning an object ready for further analysis steps.
#' 
#' @param formula A formula object.
#' @param data A data frame.
#' @param model The model to use for nowcasting. Currently implemented: "lm", "ar". Can be a vector, in which case the model is trained for each model in the vector.
#' @param order The AR order of the model to be used in `arima()`. Must be a vector of length 1.
#' @param date_col Name of the column containing date information. If NULL, the date information attempted to be inferred. If there's a single datetime column then it is used. If the data are a ts or mts or zoo object, the dates are esxtracted.
#' 
#' @returns Object of class dadnow
#' 
prep_data <- function(
  formula, data, model, date_col = NULL,
  quiet = FALSE
) {
  # Get the dates
  dates <- data[, date_col]
  if (length(dates) != length(unique(dates))) {
    stop("There are repeated dates.")
  }
  if (!inherits(dates, "Date")) {
    dates <- tryCatch(
      parse_date_time(dates, c("ymd", "dmy", "mdy", "mdy")),
      error = function(e) e
    )
    if (inherits(dates, "error")) stop("Date formats did not parse. Convert to date and try again.")
  }
  if (!quiet) {
    cat(
      "Date range:",
      format(min(lubridate::ymd(dates)), "%Y-%m-%d"), "to",
      format(max(lubridate::ymd(dates)), "%Y-%m-%d"), "\n")
  }
  data <- data[order(data[, date_col]), ]
  diffs <- diff(data[, date_col])
  diffs <- diffs[!is.na(diffs)]
  require_imputation <- length(unique(diffs)) != 1
  if (require_imputation) {
    cat("Note: data are not sampled at regular intervals, imputation is required for some models.\n")
  }

  # X and y
  X <- model.matrix(formula, data)
  y <- data[, all.vars(formula)[1]]

  # Determine the training and nowcasting data
  trailing_nas <- find_nas(y)
  num_non_na <- length(y) - trailing_nas
  X_train <- X[1:num_non_na, ]
  X_nowcast <- X[(num_non_na + 1):length(X[, 1]), ]
  y_train <- y[1:num_non_na]
  y_nowcast <- y[(num_non_na + 1):length(y)]

  # Create dadnow object
  return_value <- list(
    formula = formula,
    data = data,
    model = model,
    date_col = date_col,
    order = order,
    X_train = X_train,
    y_train = y_train,
    X_nowcast = X_nowcast,
    y_nowcast = y_nowcast,
    dates = dates,
    require_imputation = require_imputation
  )
  class(return_value) <- "dadnow"
  return_value
}
