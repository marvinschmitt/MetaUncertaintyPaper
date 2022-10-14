#' Simulate from a basic discrete-time SIR model
#'
#' @param beta infection rate
#' @param eta rate E -> I
#' @param gamma recovery rate
#' @param N population size
#' @param T_steps number of time steps to take
#' @param e0 number of initially exposed
#' @param i0 number of initially infected
#'
#' @return I(t) - vector of infected inviduals over T_steps
#' @export
SEIR = function(beta, eta, gamma, N, T_steps, e0=1, i0 = 1) {

  # Prepare vectors
  S = rep(NA, T_steps)
  E = rep(NA, T_steps)
  I = rep(NA, T_steps)
  R = rep(NA, T_steps)

  # Initial conditions
  S[1] = N - e0 - i0
  E[1] = e0
  I[1] = i0
  R[1] = 0

  # Run pandemic in discrete steps
  for (t in 2:T_steps) {

    # Newly exposed
    E_new = beta*I[t-1]*S[t-1] / N

    # Newly infected
    I_new = eta*E[t-1]

    # Newly recovered
    R_new = gamma*I[t-1]

    # Update compartments
    S[t] = S[t-1] - E_new
    E[t] = E[t-1] + E_new - I_new
    I[t] = I[t-1] + I_new - R_new
    R[t] = R[t-1] + R_new
  }
  print(S)
  print(E)
  print(I)
  print(R)

  for (t in 1:T_steps){print(sum(c(S[t], E[t], I[t], R[t])))}
  return(I)
}
