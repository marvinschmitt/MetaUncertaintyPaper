#' Fit meta models and return posterior draws
#'
#' @param data simulated data
#' @param n_posterior_draws number of posterior draws after warm-up
#'
#' @return List of posterior draws for each model
#' @export
get_meta_model_posteriors <- function(data, n_posterior_draws) {
  fit1 <- brms::brm(
    pmp ~ 1,
    data = dplyr::filter(data, true_model_idx == 1),
    family = brms::brmsfamily("logistic_normal"),
    backend = "cmdstanr",
    chains = 2,
    cores = 2,
    iter = 1000 + n_posterior_draws,
    warmup = 1000
  )


  fit2 <- brms::brm(
    pmp ~ 1,
    data = dplyr::filter(data, true_model_idx == 2),
    family = brms::brmsfamily("logistic_normal"),
    backend = "cmdstanr",
    chains = 2,
    cores = 2,
    iter = 1000 + n_posterior_draws,
    warmup = 1000
  )


  fit3 <- brms::brm(
    pmp ~ 1,
    data = dplyr::filter(data, true_model_idx == 3),
    family = brms::brmsfamily("logistic_normal"),
    backend = "cmdstanr",
    chains = 2,
    cores = 2,
    iter = 1000 + n_posterior_draws,
    warmup = 1000
  )


  new_data = data.frame(x = c(1:3))

  prep_1 = get_prep(fit1, newdata = new_data)
  prep_2 = get_prep(fit2, newdata = new_data)
  prep_3 = get_prep(fit3, newdata = new_data)


  mu_post_1 = brms:::get_Mu(prep_1, 1)
  Sigma_post_1 = brms:::get_Sigma(prep_1, 1, cor_name = "lncor")

  mu_post_2 = brms:::get_Mu(prep_2, 1)
  Sigma_post_2 = brms:::get_Sigma(prep_2, 1, cor_name = "lncor")

  mu_post_3 = brms:::get_Mu(prep_3, 1)
  Sigma_post_3 = brms:::get_Sigma(prep_3, 1, cor_name = "lncor")


  meta_model_posteriors = list(
    "true_model_1" = list("mu" = mu_post_1,
                          "Sigma" = Sigma_post_1),
    "true_model_2" = list("mu" = mu_post_2,
                          "Sigma" = Sigma_post_2),
    "true_model_3" = list("mu" = mu_post_3,
                          "Sigma" = Sigma_post_3)
  )

  return(meta_model_posteriors)
}
