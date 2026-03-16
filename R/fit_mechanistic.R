nllik_normal <- function(mu, sigma, y) {
  sum(log(sigma) + ((y - mu) / sigma)^2)
}

optim_normal <- function(Dt, Rt, Ct, Pt, sc = 0.2, sp = 0.3) {
  nllik <- function(theta) {
    alpha <- theta[1]
    eta <- theta[2]
    sigma <- theta[3:5]
    nllik_normal(alpha + eta * Rt, sigma[1], Dt) +
      nllik_normal(alpha + eta * sc * Rt, sigma[2], Ct) +
      nllik_normal(alpha + eta * sp * Rt, sigma[3], Pt)
  }
  
  optim(
    par = c("alpha" = 1, "eta" = 1, "sigmaD" = 1, "sigmaC" = 1.5, "sigmaP" = 0.5),
    fn = nllik,
    method = "L-BFGS-B",
    lower = rep(1e-8, 4),
    control = list(maxit = 10000)
  )
}

nllik_poisson <- function(theta, y) {
  -sum(y * log(theta) - theta)
}


optim_poisson <- function(Dt, Rt, Ct, Pt, sc = 0.2, sp = 0.3) {
  nllik <- function(theta) {
    alpha <- theta[1]
    eta_Rt <- max(1e-8, theta[2] * Rt)
    nllik_poisson(alpha + eta_Rt, Dt) +
    nllik_poisson(alpha + eta_Rt * sc, Ct) +
    nllik_poisson(alpha + eta_Rt * sp, Pt)
  }
    
  optim(
    par = c("alpha" = 1, "eta" = 1),
    fn = nllik,
    control = list(maxit = 10000),
    method = "L-BFGS-B",
    lower = rep(1e-8, 2),
    upper = 10000
  )
}
nllik_negbinom <- function(mu, size, y) {
  -sum(lgamma(y + size) - lgamma(size) - lgamma(y + 1) +
       size * log(size / (size + mu)) + y * log(mu / (size + mu)))
}

optim_negbinom <- function(Dt, Rt, Ct, Pt, sc = 0.2, sp = 0.3) {
  nllik <- function(theta) {
    alpha <- theta[1]
    eta_Rt <- max(1e-8, theta[2] * Rt)
    sizeD <- theta[3]          # dispersion for Dt
    sizeC <- theta[4]          # dispersion for Ct
    sizeP <- theta[5]          # dispersion for Pt

    nllik_negbinom(alpha + eta_Rt, sizeD, Dt) +
      nllik_negbinom(alpha + eta_Rt * sc, sizeC, Ct) +
      nllik_negbinom(alpha + eta_Rt * sp, sizeP, Pt)
  }

  ## enforce positivity for all four parameters
  lower_bounds <- rep(1e-8, 4)

  optim(
    par = c("alpha" = 1, "eta" = 1, "thetaD" = 2, "thetaC" = 2.5, "thetaP" = 3),
    fn = nllik,
    method = "L-BFGS-B",
    lower = lower_bounds,
    control = list(maxit = 10000))
}


#' Fit a mechanistic model to the data
#' 
#' @param Dt,Ct,Pt,Rt The data correspoding to DAD, CNISP, PTSOS, and RVDSS, respectively.
#' @param Rt_nowcast The RVDSS data for the nowcast period. This is used to create the nowcast predictions for the mechanistic model.
#' @param sc,sp The scaling factors for the CNISP and PTSOS data.
#' @param method Either "normal", "poisson", or "negbinom".
#'
#' @returns A list with the parameter estimates and the negative log-likelihood.
#' 
#' @examples
#' sim_poisson_data <- function(eta, Rt, sc = 0.2, sp = 0.3) {
#'   data.frame(
#'     Dt = rpois(length(Rt), eta * Rt),
#'     Ct = rpois(length(Rt), eta * sc * Rt),
#'     Pt = rpois(length(Rt), eta * sp * Rt),
#'     Rt = Rt
#'   )
#' }
#' d2 <- sim_poisson_data(eta = 5, Rt = rpois(100, 100))
#' fit_mechanistic(d2$Dt, d2$Ct, d2$Pt, d2$Rt, Rt_nowcast = rpois(10, 100), method = "poisson")
#' @export
fit_mechanistic <- function(
  Y_train, X_train = NULL, X_nowcast = NULL,
  params = list(sc = 0.2, sp = 0.3, method = "normal")
) {
  Dt <- Y_train
  Ct <- X_train[, 1]
  Pt <- X_train[, 2]
  Rt <- X_train[, 3]
  Rt_nowcast <- X_nowcast[, 3]
  optim_res <- switch(method,
    "normal" = optim_normal(Dt, Rt, Ct, Pt, sc, sp),
    "poisson" = optim_poisson(Dt, Rt, Ct, Pt, sc, sp),
    "negbinom" = optim_negbinom(Dt, Rt, Ct, Pt, sc, sp)
  )
  if (optim_res$convergence != 0) warning("Model did not converge.")

  model <- c(optim_res$par, sc = sc, sp = sp, method = method, convergence = optim_res$convergence)

  list(model = model, predictions = optim_res$par[1] + optim_res$par[2] * Rt_nowcast)
}


#' Fit a mechanistic model to the data, returning a dadnow object
#' 
#' @param formula A formula object, *must* be of the form Dt ~ Ct + Pt + Rt.
#' @param data A data frame. Must contain the variables specified in the formula and in `date_col`. Trailing NA values in `y` will be nowcasted.
#' @param params The parameters to use for the model. Must be a named list containing sc and sp and method (normal, poisson, or negbinom).
#' @param date_col Name of the column containing date information. If NULL, the date information attempted to be inferred. If there's a single datetime column then it is used. If the data are a ts or mts or zoo object, the dates are esxtracted.
#'
#' @returns A dadnow object with the mechanistic model added.
#' @export
nowcast_mechanistic <- function(
  formula, data, batches = 40, train_window = NULL, level = 0.95, date_col = NULL,
  params = list(sc = 0.2, sp = 0.3, method = "normal")
) {

  prepped_data <- prep_data(
    formula, data, model = "mechanistic",, date_col = date_col
  )

  response <- all.vars(formula)[1]
  terms <- all.vars(formula)[-1]

  cat(
    paste0(
      "Assuming that \"", response, "\" contains DAD data, \"", 
      terms[1], "\" is CNISP, \"", terms[2], "\" is PTSOS, and \"",
      terms[3], "\" is RVDSS.\n"
    )
  )

  enbpi <- enbpi(
    X_train = prepped_data$X_train,
    y_train = prepped_data$y_train,
    formula = "mechanistic",
    model = "mechanistic",
    params = params,
    k = nrow(prepped_data$X_nowcast),
    batches = 40,
    train_window = floor(0.6 * nrow(prepped_data$X_train)),
    level = 0.95
  )

  dadnow_mech <- fit_mechanistic(
    Y_train = prepped_data$y_train,
    X_train = prepped_data$X_train,
    X_nowcast = prepped_data$X_nowcast,
    params = params
  )

  dadnow <- list(
    date_col = date_col,
    data = as.data.frame(data),
    models = list(
      list(
        model_id = paste0("mech_", params$method),
        formula = paste0("mech_", params$method),
        prepped_data = prepped_data,
        model = dadnow_mech$model,
        predictions = dadnow_mech$predictions,
        evals = enbpi$evals,
        enbpi = enbpi$enbpi,
        params = params
      )
    )
  )
  names(dadnow$models)[1] <- paste0("mech_", params$method)
  class(dadnow) <- "multidadnow"
  dadnow
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
