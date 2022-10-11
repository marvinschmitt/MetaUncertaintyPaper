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
niter <- 1000
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
e0 <- 1
s0 <- N - i0 - e0
r0 <- 0
y0 = c(S = s0, E = e0, I = i0, R = r0)
sir_data <- list(n_days = n_days, y0 = y0, t0 = t0, ts = t, N = N, cases = cases)

model_1 <- stan_model("experiments/stan_models/models_influenza/M_1.stan", model_name = "M1")
model_2 <- stan_model("experiments/stan_models/models_influenza/M_2.stan", model_name = "M2")
model_3 <- stan_model("experiments/stan_models/models_influenza/M_3.stan", model_name = "M3")

M = list(model_1, model_2, model_3)

# save(M, file = "output/computations/experiment_2_M.RData")

# load("output/computations/experiment_2_M.RData")

df_wide = data.frame(
  k = 1:K,
  true_model_idx = sample(1:J, size=K, replace=TRUE, prob=prior_model_prob),
  pmp1 = NA, pmp2 = NA, pmp3 = NA
)
df_wide$true_model = factor(df_wide$true_model_idx, levels = c(1,2,3),
                            labels = c(latex2exp::TeX("$M_*=M_1$"),
                                       latex2exp::TeX("$M_*=M_2$"),
                                       latex2exp::TeX("$M_*=M_3$")
                                       )
                            )

for (k in 1:K){
  print(paste0("----------------— Run k = ", k, " ----------------—"))
  true_model_idx = df_wide$true_model_idx[k]
  print(paste0("True model: M", true_model_idx))
  true_model = M[[true_model_idx]]

  true_model_prior_predictive_fit = sampling(true_model,
                                  data = c(sir_data, prior_predictive=1),
                                  chains = 1,
                                  iter=1,
                                  seed = k,
                                  refresh = 0,
                                  show_messages=FALSE)

  simulated_cases = rstan::extract(true_model_prior_predictive_fit)$pred_cases[1, ]
  #plot(simulated_cases[1, ], ylim=c(0,200))
  #for (i in 1:100) lines(simulated_cases[i, ])
  simulated_data = list(n_days  = n_days, y0 = y0, t0 = t0, ts = t, N=N, cases=simulated_cases)

  fit_1 = sampling(model_1, data = c(simulated_data, prior_predictive=0), chains = 4, iter = niter, seed = 0, refresh = 0, show_messages=FALSE)
  fit_2 = sampling(model_2, data = c(simulated_data, prior_predictive=0), chains = 4, iter = niter, seed = 0, refresh = 0, show_messages=FALSE)
  fit_3 = sampling(model_3, data = c(simulated_data, prior_predictive=0), chains = 4, iter = niter, seed = 0, refresh = 0, show_messages=FALSE)


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



