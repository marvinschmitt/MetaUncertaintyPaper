#' Construct a logistic normal mixture distribution
#'
#' @param x data
#' @param theta mixture weights
#' @param mu_list (list of) location parameters
#' @param Sigma_list (list of) shape parameters
#'
#' @return
#' @export
#'
logistic_normal_mixture = function(x, theta, mu_list, Sigma_list){
  out = theta[1] * brms::dlogistic_normal(x, mu_list[[1]], Sigma_list[[1]])
  for (i in 2:length(theta)){
    out = out + theta[i] * brms::dlogistic_normal(x, mu_list[[i]], Sigma_list[[i]])
  }
  return(out)
}
