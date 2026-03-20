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


#' @rdname add_model
add_model.multidadnow <- function(multidadnow, formula = NULL, model, params = NULL) {
  
  
  if ("model" %in% names(multidadnow$data)) {
    model_data <- multidadnow$data[multidadnow$data$model == "Training", ]
  } else {
    model_data <- multidadnow$data
  }

  if (is.null(formula)) {
    formula <- multidadnow$models[[1]]$formula
    message(paste0("Using formula from first registered model: ", deparse(formula)))
  }
  
  prepped_data <- prep_data(
    formula, model_data, model, date_col = multidadnow$date_col,
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

  aug_data <- as.data.frame(multidadnow$data)

  nowcasted_data <- aug_data[(nrow(prepped_data$X_train) + 1):nrow(aug_data), ]
  nowcasted_data[, prepped_data$response] <- new_preds$prediction$prediction
  nowcasted_data$model <- model
  nowcasted_data$params <- paste0(names(params), params, collapse = "_")
  nowcasted_data$pi_lower <- enbpi$enbpi[, 1]
  nowcasted_data$pi_upper <- enbpi$enbpi[, 2]

  aug_data <- rbind(aug_data, nowcasted_data)
  multidadnow$data <- aug_data
  
  multidadnow$models[[length(multidadnow$models) + 1]] <- list(
    formula = formula,
    date_col = multidadnow$date_col,
    prepped_data = prepped_data,
    model = new_preds$model,
    predictions = new_preds$prediction$prediction,
    evals = enbpi$evals,
    params = params
  )

  all_evals <- do.call(rbind, lapply(multidadnow$models, function(x) x$evals))
  model_ids <- make_model_id(all_evals)

  rownames(all_evals) <- model_ids

  multidadnow$evals <- all_evals
  names(multidadnow$models) <- model_ids
  for (i in seq_along(multidadnow$models)) {
    multidadnow$models[[i]]$model_id <- model_ids[i]
  }

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
  
  
  if ("model" %in% names(dadnow$data)) {
    model_data <- dadnow$data[dadnow$data$model == "Training", ]
  } else {
    model_data <- dadnow$data
  }

  dadnow_mech <- nowcast_mechanistic(
    formula, data = model_data, 
    params = params,
    date_col = dadnow$date_col
  )
  
  dadnow <- combine_dadnow(dadnow, dadnow_mech)
  dadnow
}
