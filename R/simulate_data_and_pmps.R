

#' Simulate set of data and posterior model probabilities
#'
#' @param M List of models
#' @param prior_model_prob Prior model probabilities, defaults to uniform
#' @param K number of simulated data sets
#' @param N number of observations per data set
#'
#' @return Data frames with posterior model probabilities and additional info, in wide and long format
#' @export
simulate_data_and_pmps <- function(M, prior_model_prob = NULL,
                          K = 1000, N = 20){
  J = length(M) # number of models

  # if no model prior is specified --> uniform
  if (is.null(prior_model_prob)){
    prior_model_prob = rep(1/J, times=J)
  }
  df_wide = data.frame(
    true_model_idx = sample(1:J, size=K, replace=TRUE, prob=prior_model_prob),
    # rep(1:J, each = K),
    pmp_1 = NA, pmp_2 = NA, pmp_3 = NA, #pmp_4 = NA,
    beta = NA, sigma2 = NA, N = NA
  )

  df_wide$true_model = factor(df_wide$true_model, levels = c(1,2,3),
                              labels = c(latex2exp::TeX("$M_*=M_1$"),
                                         latex2exp::TeX("$M_*=M_2$"),
                                         latex2exp::TeX("$M_*=M_3$")))

  for (k in 1:K){
    true_model_idx = df_wide$true_model_idx[k]
    true_model = M[[true_model_idx]]
    sim = simulate_data_bayesian_linear_regression(model=true_model, N=N)
    data = sim$data

    log_ml_1 = calculate_ml_bayesian_linear_regression(M[[1]], data, return_log = TRUE)
    log_ml_2 = calculate_ml_bayesian_linear_regression(M[[2]], data, return_log = TRUE)
    log_ml_3 = calculate_ml_bayesian_linear_regression(M[[3]], data, return_log = TRUE)
    #log_ml_4 = calculate_ml_bayesian_linear_regression(M[[4]], data, return_log = TRUE)
    log_ml_true_model = NA #calculate_marginal_likelihood(true_model, data, return_log = TRUE)

    log_pmp_normalization = log(
      prior_model_prob[1] * exp(log_ml_1) +
        prior_model_prob[2] * exp(log_ml_2) +
        prior_model_prob[3] * exp(log_ml_3) #+
        #prior_model_prob[4] * exp(log_ml_4)
    )
    log_pmp_1 = log_ml_1 + log(prior_model_prob[1]) - log_pmp_normalization
    log_pmp_2 = log_ml_2 + log(prior_model_prob[2]) - log_pmp_normalization
    log_pmp_3 = log_ml_3 + log(prior_model_prob[3]) - log_pmp_normalization
    #log_pmp_4 = log_ml_4 + log(prior_model_prob[4]) - log_pmp_normalization


    #df_wide$true_model[k] = true_model_idx

    df_wide$pmp_1[k] = exp(log_pmp_1)
    df_wide$pmp_2[k] = exp(log_pmp_2)
    df_wide$pmp_3[k] = exp(log_pmp_3)
    #df_wide$pmp_4[k] = exp(log_pmp_4)

    df_wide$beta[k] = list(sim$beta)
    df_wide$sigma2[k] = sim$sigma2
    df_wide$N[k] = N
  }



  # assert that PMPs of the three candidate models for each data set sum to one
  stopifnot(
    abs(df_wide$pmp_1 +
          df_wide$pmp_2 +
          df_wide$pmp_3 #+
          #df_wide$pmp_4
          - 1 ) < 1e-1
  )

  df_long = tidyr::pivot_longer(
    df_wide,
    c("pmp_1", "pmp_2", "pmp_3"),#, "pmp_4"),
    names_to = c(".value", "test_model"),
    names_pattern = "(.*)_(.)",
    values_to = c("pmp")
    )

  return(
    list(
      df_wide = df_wide,
      df_long = df_long
    )
  )
}
