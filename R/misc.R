#' Build symmetric matrix from flattened lower triangular
#'
#' @param lower_tri flattened lower triangular matrix
#' @param nrow number of rows (can be removed with clever deductions in future version)
#'
#' @return constructed symmetric matrix
#' @export
flat_lower_triag_to_symmetric = function(lower_tri, nrow=2){
  out = matrix(data = 0.0, nrow=2, ncol=2)
  out[lower.tri(out, diag=TRUE)] = lower_tri
  out = out + t(out) - diag(diag(out), nrow=nrow(out))
  return (out)
}
