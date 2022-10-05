

#' Calculate the marginal likelihood for a Bayesian linear regression model
#'
#' @description with Normal-Inverse-Gamma conjugacy
#'
#' @param model List  with model information
#' @param data Observed data y
#' @param return_log (bool) should the log marginal likelihood be returned
#'
#' @return Marginal Likelihood (log depending on `return_log`)
#' @export
calculate_ml_bayesian_linear_regression <- function(model, data, return_log = FALSE){
  X = as.matrix(data[, model$predictors, drop=FALSE])
  y = as.matrix(data[, "y", drop=FALSE])
  Lambda_0 = model$prior$Lambda_0
  mu_0 = model$prior$mu_0
  a_0 = model$prior$a_0
  b_0 = model$prior$b_0

  N = nrow(X)
  beta_hat = solve((t(X) %*% X)) %*% t(X) %*% y

  Lambda_n = t(X) %*% X + Lambda_0
  mu_n = solve(Lambda_n) %*% (Lambda_0%*%mu_0 + t(X) %*% X %*% beta_hat)
  a_n = a_0 + N/2
  b_n = b_0 + 0.5 * (
    t(y) %*% y +
      t(mu_0)%*%Lambda_0%*%mu_0 -
      t(mu_n) %*% Lambda_n %*% mu_n
  )

  log_ml =
    log(1) - 0.5 * N * log(2*pi) +
    0.5 * (log(det(Lambda_0)) - log(det(Lambda_n))) +
    a_0 * log(b_0) - a_n * log(b_n)+
    log(gamma(a_n)) - log(gamma(a_0))

  if (return_log) {
    return(as.numeric(log_ml))
  } else{
    return(exp(as.numeric(log_ml)))
  }
}



#' Data generating process for Bayesian Linear Regression
#'
#' @description with Normal-Inverse-Gamma Conjugacy
#' @param model model details
#' @param N number of observations
#'
#' @return data `y`, variances `sigma_2`, weights `beta`
#' @export
simulate_data_bayesian_linear_regression <- function(model, N){

  n_predictors = nrow(model$prior$mu_0)
  sigma2 = invgamma::rinvgamma(n = 1, shape = model$prior$a_0, scale = model$prior$b_0)
  beta = MASS::mvrnorm(n = 1, mu = model$prior$mu_0, Sigma = sigma2 * solve(model$prior$Lambda_0))

  X = MASS::mvrnorm(n = N, mu = rep(0, times=5), Sigma = model$cov_X)
  colnames(X)=paste0("x", 1:5)
  y  = X[, model$predictors, drop=FALSE] %*% beta + stats::rnorm(N, 0, sqrt(sigma2))

  return(list(
    data = data.frame(X, y),
    sigma2 = sigma2,
    beta = beta
  ))
}
