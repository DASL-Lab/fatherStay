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
#' @param X_train,Y_train,X_nowcast The data correspoding to DAD, CNISP, PTSOS, and RVDSS, respectively.
#' @param params The parameters to use for the model. Must be a named list containing sc and sp and method (normal, poisson, or negbinom).
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
#' fit_mechanistic(Y_train = d2$Dt, X_train = d2["Ct", "Pt", "Rt"], X_nowcast = rpois(10, 100), params = list(method = "poisson"))
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
  optim_res <- switch(params$method,
    "normal" = optim_normal(Dt, Rt, Ct, Pt, params$sc, params$sp),
    "poisson" = optim_poisson(Dt, Rt, Ct, Pt, params$sc, params$sp),
    "negbinom" = optim_negbinom(Dt, Rt, Ct, Pt, params$sc, params$sp)
  )
  if (optim_res$convergence != 0) warning("Model did not converge.")

  model <- c(optim_res$par, sc = params$sc, sp = params$sp, method = params$method, convergence = optim_res$convergence)

  list(model = model, predictions = optim_res$par[1] + optim_res$par[2] * Rt_nowcast)
}
