#' Simulate from a basic discrete-time SIR model
#'
#' @param beta infection rate
#' @param gamma recovery rate
#' @param N population size
#' @param T_steps number of time steps to take
#' @param i0 number of initially infected
#'
#' @return I(t) - vector of infected inviduals over T_steps
#' @export
SIR = function(beta, gamma, N, T_steps, i0=1) {

  # Prepare vectors
  S = rep(NA, T_steps)
  I = rep(NA, T_steps)
  R = rep(NA, T_steps)

  # Initial conditions
  S[1] = N - i0
  I[1] = i0
  R[1] = 0

  # Run pandemic in discrete steps
  for (t in 2:T_steps) {

    # Newly infected
    I_new = beta*I[t-1]*S[t-1] / N

    # Newly recovered
    R_new = gamma*I[t-1]

    # Update compartments
    S[t] = S[t-1] - I_new
    I[t] = I[t-1] + I_new - R_new
    R[t] = R[t-1] + R_new
  }
  return(I)
}
