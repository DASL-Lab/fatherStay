#' Prepare the data for analysis, returning an object ready for further analysis steps.
#' 
#' @param formula A formula object.
#' @param data A data frame.
#' @param model The model to use for nowcasting. Currently implemented: "lm", "ar". Can be a vector, in which case the model is trained for each model in the vector.
#' @param order The AR order of the model to be used in `arima()`. Must be a vector of length 1.
#' @param date_col Name of the column containing date information. If NULL, the date information attempted to be inferred. If there's a single datetime column then it is used. If the data are a ts or mts or zoo object, the dates are esxtracted.
#' 
#' @returns Object of class dadnow
#' @export
prep_data <- function(
  formula, data, model, date_col = NULL,
  quiet = FALSE, order = 1
) {
  data <- as.data.frame(data) |> 
    parse_dates(date_col = date_col, quiet = quiet)
  
  diffs <- diff(data[, date_col])
  diffs <- diffs[!is.na(diffs)]
  require_imputation <- length(unique(diffs)) != 1
  if (require_imputation) {
    cat("Note: data are not sampled at regular intervals, imputation is required for some models.\n")
  }

  # Training and nowcasting data
  response <- all.vars(formula)[1]
  covariates <- all.vars(formula)[-1]
  stopifnot(all(covariates %in% colnames(data)))

  y <- data[, response]
  trailing_nas <- find_nas(y)
  stopifnot(trailing_nas > 0)
  num_non_na <- length(y) - trailing_nas

  model_matrix <- parse_lag_formula(formula, data)

  X_train <- model_matrix[1:num_non_na, , drop = FALSE]
  X_nowcast <- model_matrix[(num_non_na + 1):nrow(data), , drop = FALSE]
  y_train <- y[1:num_non_na]
  # I don't know why this is here - it's all NAs anyway.
  y_nowcast <- y[(num_non_na + 1):nrow(data)]

  # TODO: Allow for lag terms. There are two options:
  # 1. allow the user to specify the lags in the formula (y ~ lag(x, 1)).
    # - Fraught with regex, but convenient for the user.
  # 2. include an argument lags = list("x" = 2)
    # Less convenient for the user but way easier for me.
  # Also note that lags of the response are somewhat reasonable for some models.

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
    dates = data[, date_col],
    require_imputation = require_imputation
  )
  class(return_value) <- "dadnow"
  return_value
}


parse_dates <- function(data, date_col, quiet) {
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
    if (inherits(dates, "error")) stop("Date formats did not parse. Please manually convert to 'Date' and try again.")
  }
  if (!quiet) {
    cat(
      "Date range:",
      format(min(lubridate::ymd(dates)), "%Y-%m-%d"), "to",
      format(max(lubridate::ymd(dates)), "%Y-%m-%d"), "\n"
    )
  }
  data <- data[order(data[, date_col]), ]
  data
}

parse_lag_formula <- function(formula, data) {
  # Expand the formula to handle shorthand like (x + z)^2
  tms <- terms(formula, data = data)
  # Get the labels (e.g., "lag(x, 2)", "lag(z, 3)")
  term_labels <- attr(tms, "term.labels")
  
  # Initialize a list to store our expanded columns
  model_cols <- list()
  
  for (label in term_labels) {
    # Convert the string label back into a language object (call)
    call_obj <- parse(text = label)[[1]]
    
    # Check if the term is a 'lag' function
    if (is.call(call_obj) && call_obj[[1]] == quote(lag)) {
      var_name <- as.character(call_obj[[2]])
      max_lag  <- as.numeric(call_obj[[3]])
      
      # Extract the original vector from the data
      original_vec <- data[[var_name]]
      
      # Create lags 0 through max_lag
      for (i in 0:max_lag) {
        col_name <- paste0(var_name, "_lag", i)
        # Using a simple shifting logic for the lag
        model_cols[[col_name]] <- c(rep(NA, i), head(original_vec, length(original_vec) - i))
      }
    } else {
      # Handle non-lagged terms (like a standard intercept or x)
      model_cols[[label]] <- data[[label]]
    }
  }
  
  # Return as a data frame/matrix
  as.data.frame(model_cols)
}