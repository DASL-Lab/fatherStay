#' Add a model to an existing dadnow or multidadnow object
#' 
#' @param x A dadnow or multidadnow object.
#' @param formula A formula object.
#' @param model The model to use for nowcasting. Currently implemented: "lm", "ar". Can be a vector, in which case the model is trained for each model in the vector.
#' @param params The parameters to use for the model. Must be a named list.
#'
#' @returns A dadnow or multidadnow object with the model added.
#' @export
add_model <- function(x, ...) {
  UseMethod("add_model")
}

#' Add a model to an existing dadnow object
#'
#' @param dadnow A dadnow object.
#' @param model The model to add.
#' @param params The parameters to use for the model. Must be a named list.
#'
#' @returns A dadnow object with the model added.
#' @export
add_model.dadnow <- function(dadnow, formula = NULL, model, params = NULL) {
  
  if (is.null(formula)) {
    formula <- dadnow$formula
  }

  prepped_data <- prep_data(
    formula = formula, data = dadnow$data, model = model, date_col = dadnow$date_col, cross_val_indices = dadnow$cross_val_indices
  )

  new_eval <- cross_val_error(
    X_train = prepped_data$X_train, y_train = prepped_data$y_train, folds = prepped_data$cross_val_indices,
    model = model, params = params
  )

  new_preds <- dispatch_model(model)(
    X_train = prepped_data$X_train,
    Y_train = prepped_data$y_train,
    X_nowcast = prepped_data$X_nowcast,
    params = params
  )

  dadnow_one <- list(
    model_id = dadnow$model_id,
    formula = dadnow$formula,
    date_col = dadnow$date_col,
    prepped_data = dadnow$prepped_data,
    model = dadnow$model,
    predictions = dadnow$predictions,
    evals = dadnow$evals,
    params = params
  )

  dadnow_two <- list(
    model_id = make_model_id(model, params),
    formula = formula,
    date_col = dadnow$date_col,
    prepped_data = prepped_data,
    model = new_preds$model,
    predictions = new_preds$prediction,
    evals = new_eval,
    params = params
  )

  multidadnow <- list(
    date_col = dadnow$date_col,
    data = dadnow$data,
    models = list(dadnow_one, dadnow_two)
  )
  names(multidadnow$models) <- c(dadnow_one$model_id, dadnow_two$model_id)
  class(multidadnow) <- "multidadnow"

  multidadnow
}

#' @rdname add_model.dadnow
#' @export
add_model.multidadnow <- function(multidadnow, formula = NULL, model, params = NULL) {
  
  if (is.null(formula)) {
    formula <- multidadnow$models[[1]]$formula
    message(paste0("Using formula from first registered model: ", deparse(formula), "\n"))
  }
  
  prepped_data <- prep_data(
    formula, multidadnow$data, model, date_col = multidadnow$date_col,
    cross_val_indices = multidadnow$cross_val_indices
  )

  enbpi <- enbpi(
    X_train = prepped_data$X_train,
    y_train = prepped_data$y_train,
    formula = prepped_data$formula,
    model = model,
    params = params,
    k = nrow(prepped_data$X_nowcast),
    batches = 40,
    train_window = NULL,
    level = 0.95
  )

  new_preds <- dispatch_model(model)(
    X_train = rbind(prepped_data$X_train, prepped_data$X_test),
    Y_train = c(prepped_data$y_train, prepped_data$y_test),
    X_nowcast = prepped_data$X_nowcast,
    params = params
  )
  
  multidadnow$models[[length(multidadnow$models) + 1]] <- list(
    model_id = make_model_id(model, params),
    formula = formula,
    date_col = multidadnow$date_col,
    prepped_data = prepped_data,
    model = new_preds$model,
    predictions = new_preds$prediction,
    evals = enbpi$evals,
    params = params
  )
  names(multidadnow$models)[length(multidadnow$models)] <- make_model_id(model, params)

  multidadnow
}


#' Add a mechanistic model to a dadnow or multidadnow object
#'
#' @param dadnow A dadnow or multidadnow object.
#' @param Dt,Ct,Pt,Rt The data correspoding to DAD, CNISP, PTSOS, and RVDSS, respectively.
#' @param Rt_nowcast The RVDSS data for the nowcast period. This is used to create the nowcast predictions for the mechanistic model.
#' @param sc,sp The scaling factors for the CNISP and PTSOS data.
#' @param method Either "normal", "poisson", or "negbinom".
#'
#' @returns A dadnow or multidadnow object with the mechanistic model added.
#' @export
add_mechanistic <- function(dadnow, formula, params = list(sc = 0.2, sp = 0.3, method = "poisson")) {
  dadnow_mech <- nowcast_mechanistic(
    formula, data = dadnow$data, 
    params = params,
    date_col = dadnow$date_col
  )
  
  dadnow <- combine_dadnow(dadnow, dadnow_mech)
  dadnow
}
