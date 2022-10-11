library(outbreaks)
library(tidyr)
library(magrittr)
library(dplyr)
library(rstan)
library(brms)
library(bridgesampling)
library(ggplot2)
library(latex2exp)
library(bayesplot)

c_light <- c("#fee0d2")
c_mid <- c("#fc9272")
c_dark <- c("#de2d26")
c_simu <- "chartreuse3"
c_posterior = "orange"
c_prior = "aquamarine2"

set.seed(1) # for reproductibility

# Settings
niter <- 5000
K = 200
J = 3
prior_model_prob = rep(1/J, J)


theme_set(theme_bw())
ggplot(data = influenza_england_1978_school) +
  geom_point(mapping = aes(x = date, y = in_bed)) +
  labs(y = "Number of students in bed")


# setup
cases <- influenza_england_1978_school$in_bed  # Number of students in bed
N <- 763;
n_days <- length(cases)
t <- seq(0, n_days, by = 1)
t0 = 0
t <- t[-1]
i0 <- 1
s0 <- N - i0
r0 <- 0
y0 = c(S = s0, I = i0, R = r0)
sir_data <- list(n_days = n_days, y0 = y0, t0 = t0, ts = t, N = N, cases = cases)

prior_params_1 = list(
           mu_beta = 1.7, sigma_beta = 0.1,
           mu_gamma = 0.5, sigma_gamma = 0.1,
           lambda_phi_inv = 1)

prior_params_2 = list(
           mu_beta = 1.7, sigma_beta = 0.1,
           mu_gamma = 0.5, sigma_gamma = 0.15,
           lambda_phi_inv = 1)

prior_params_3 = list(
           mu_beta = 1.7, sigma_beta = 0.1,
           mu_gamma = 0.5, sigma_gamma = 0.1,
           lambda_phi_inv = 3)
prior_params_list = list(prior_params_1, prior_params_2, prior_params_3)

# model <- stan_model("experiments/stan_models/models_influenza/model.stan")
# model_prior_predictive <- stan_model("experiments/stan_models/models_influenza/model_prior.stan")
#
# save(model, file="output/computations/experiment_2_model.Rdata")
# save(model_prior_predictive, file="output/computations/experiment_2_model_prior_predictive.Rdata")

load("output/computations/experiment_2_model.Rdata")
load("output/computations/experiment_2_model_prior_predictive.Rdata")


# model_1 <- stan_model("experiments/stan_models/models_influenza/M_1.stan")
# model_2 <- stan_model("experiments/stan_models/models_influenza/M_2.stan")
# model_3 <- stan_model("experiments/stan_models/models_influenza/M_3.stan")
#
# model_1_prior <- stan_model("experiments/stan_models/models_influenza/M_1_prior.stan")
# model_2_prior <- stan_model("experiments/stan_models/models_influenza/M_2_prior.stan")
# model_3_prior <- stan_model("experiments/stan_models/models_influenza/M_3_prior.stan")

# M = list(model_1, model_2, model_3)
# M_prior = list(model_1_prior, model_2_prior, model_3_prior)
#
# save(M, file = "output/computations/experiment_2_M.RData")
# save(M_prior, file = "output/computations/experiment_2_M_prior.RData")

# load("output/computations/experiment_2_M.RData")
# load("output/computations/experiment_2_M_prior.RData")

df_wide = data.frame(
  k = 1:K,
  true_model_idx = sample(1:J, size=K, replace=TRUE, prob=prior_model_prob),
  pmp1 = NA, pmp2 = NA, pmp3 = NA
)
df_wide$true_model = factor(df_wide$true_model_idx, levels = c(1,2,3),
                            labels = c(latex2exp::TeX("$M_*=M_1$"),
                                       latex2exp::TeX("$M_*=M_2$"),
                                       latex2exp::TeX("$M_*=M_3$")))

for (k in 1:K){
  print(paste0("----------------— Run k = ", k, " ----------------—"))
  true_model_idx = df_wide$true_model_idx[k]
  true_model_prior_params = prior_params_list[[true_model_idx]]
  print(paste0("True model: M", true_model_idx))

  true_model_prior_predictive_fit = sampling(model_prior_predictive,
                                  data = c(sir_data, true_model_prior_params), # contains prior
                                  chains = 1,
                                  iter=1000,
                                  seed = k,
                                  refresh = 0,
                                  show_messages=FALSE)


  #simulated_cases = as.vector(true_model_prior_predictive_fit$draws("pred_cases"))
  simulated_cases = rstan::extract(true_model_prior_predictive_fit)$pred_cases[1, ]
  #plot(simulated_cases[1, ], ylim=c(0,200))
  #for (i in 1:100) lines(simulated_cases[i, ])
  simulated_data = list(n_days  = n_days, y0 = y0, t0 = t0, ts = t, N=N, cases=simulated_cases)

  fit_1 = sampling(model, data = c(simulated_data, prior_params_list[[1]]), chains = 4, iter = niter, seed = 0, refresh = 0, show_messages=FALSE)
  fit_2 = sampling(model, data = c(simulated_data, prior_params_list[[2]]), chains = 4, iter = niter, seed = 0, refresh = 0, show_messages=FALSE)
  fit_3 = sampling(model, data = c(simulated_data, prior_params_list[[3]]), chains = 4, iter = niter, seed = 0, refresh = 0, show_messages=FALSE)


  logml_m1 = bridgesampling::bridge_sampler(fit_1, silent=TRUE)
  logml_m2 = bridgesampling::bridge_sampler(fit_2, silent=TRUE)
  logml_m3 = bridgesampling::bridge_sampler(fit_3, silent=TRUE)

  pmp = brms::post_prob(logml_m1, logml_m2, logml_m3)
  print(pmp %>% round(4))
  df_wide$pmp1[k] = pmp[1]
  df_wide$pmp2[k] = pmp[2]
  df_wide$pmp3[k] = pmp[3]

  ggplot(df_wide, aes(pmp = ggsimplex::make_list_column(pmp1, pmp2, pmp3))) +
    coord_fixed(ratio = 1, xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1)) +
    theme_void() +
    ggsimplex::geom_simplex_point(size=1) +
    ggsimplex::geom_simplex_canvas() +
    facet_grid(~ true_model, labeller = label_parsed) +
    theme(strip.text = element_text(size = 18)) +
    labs(title = paste0("k = ", k))

  ggsave("output/plots/experiment-2-level-2.pdf")
}



