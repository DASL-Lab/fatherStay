#' Add a model to an existing dadnow object
#'
#' @param dadnow A dadnow object.
#' @param model The model to add.
#' @param params The parameters to use for the model. Must be a named list.
#'
#' @returns A dadnow object with the model added.
#' @export
add_model <- function(dadnow, model, params = NULL) {
  
  new_eval <- fit_model_test(
    model, params,
    dadnow$X_train,
    dadnow$y_train,
    dadnow$X_test,
    dadnow$y_test
  )
  dadnow$eval <- rbind(dadnow$eval, new_eval)

  new_preds <- dispatch_model(model)(
    X_train = rbind(dadnow$X_train, dadnow$X_test),
    Y_train = c(dadnow$y_train, dadnow$y_test),
    X_nowcast = dadnow$X_nowcast,
    params = params
  )
  dadnow[[paste0("nowcast_", model)]] <- new_preds

  dadnow
}