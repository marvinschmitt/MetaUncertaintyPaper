
#' Stable logsumexp(x)
#' @description A numerically stable implementation of log(sum(exp(x))).
#' @param x a numeric vector
#'
#' @return a numeric vector
#'
#' @export
log_sum_exp <- function (x) {
  xmax <- max(x)
  xsum <- sum(exp(x - xmax))
  xmax + log(xsum)
}


#' Stable logmeanexp(x)
#' @description A numerically stable implementation of log(mean(exp(x))).
#' @param x a numeric vector
#'
#' @return a numeric vector
#'
#' @export
log_mean_exp <- function(x){
  mx = max(x)
  return(mx+log(mean(exp(x-mx))))
}

#' Continuity correction
#' @description Truncates a vector to the interval (epsilon, 1-epsilon) and re-normalizes the values
#' @param x a numeric vector
#' @param epsilon threshold for truncating
#'
#' @return a numeric vector, corrected
#'
#' @export
continuity_correction <- function(x, epsilon = 0.0001) {
  x <- as.matrix(x)
  x <- ifelse(x < epsilon, epsilon, x)
  x <- ifelse(x > 1 - epsilon, 1 - epsilon, x)
  x / rowSums(x)
}
