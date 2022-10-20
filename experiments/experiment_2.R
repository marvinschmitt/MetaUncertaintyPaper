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
library(scales)
library(lubridate)
library(MetaUncertaintyPaper)

c_light <- c("#fee0d2")
c_mid <- c("#fc9272")
c_dark <- c("#de2d26")
c_simu <- "chartreuse3"
c_posterior = "#5DC863"
c_prior = "aquamarine2"
c_data = "#404688"

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
# cases = c(cases, rep(0, 100))
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

# model_1 <- stan_model("experiments/stan_models/models_influenza/M_1.stan", model_name = "M1")
# model_2 <- stan_model("experiments/stan_models/models_influenza/M_2.stan", model_name = "M2")
# model_3 <- stan_model("experiments/stan_models/models_influenza/M_3.stan", model_name = "M3")
# M = list(model_1, model_2, model_3)
# save(M, file = "output/computations/experiment-2-M.RData")

# Prepare data frame ----
# df_wide = data.frame(
#   k = 1:K,
#   true_model_idx = sample(1:J, size=K, replace=TRUE, prob=prior_model_prob),
#   pmp1 = NA, pmp2 = NA, pmp3 = NA
# )
# df_wide$true_model = factor(df_wide$true_model_idx, levels = c(1,2,3),
#                             labels = c(latex2exp::TeX("$M_*=M_1$"),
#                                        latex2exp::TeX("$M_*=M_2$"),
#                                        latex2exp::TeX("$M_*=M_3$")
#                                        )
#                             )


# simulate data ----

# simulated_data_matrix = matrix(data = NA, nrow=K, ncol=n_days)
# for (j in 1:J){
#   model = M[[j]]
#   n_simulations = unname(table(df_wide$true_model)[j])
#   prior_predictive_fit = sampling(model,
#                                   data = c(sir_data, prior_predictive=1),
#                                   chains = 1,
#                                   iter = 5000 + n_simulations,
#                                   warmup=5000,
#                                   refresh=0,
#                                   show_messages=FALSE)
#   simulated_cases = rstan::extract(prior_predictive_fit)$pred_cases
#
#   simulated_data_matrix[which(df_wide$true_model_idx == j), ] = simulated_cases
# }
# save(simulated_data_matrix, file="output/computations/experiment-2-simulated_data_matrix.RData")

#
# colors = c("red", "green", "blue")
# plot(simulated_data_matrix[1, ], col = colors[df_wide$true_model_idx[1]], type="l",
#      ylim=c(0,800))
# for (k in 2:K){
#   lines(simulated_data_matrix[k, ], col = colors[df_wide$true_model_idx[k]], type="l")
#
# }

# visualize simulated data ----
load("output/computations/experiment-2-M.RData")
load("output/computations/experiment-2-df_wide.RData")
load("output/computations/experiment-2-simulated_data_matrix.RData")
simulated_data_matrix = simulated_data_matrix[1:100, ]
d = cbind(df_wide %>% dplyr::select(k, true_model_idx),
      data.frame(simulated_data_matrix)) %>%
  tidyr::pivot_longer(., cols = starts_with("X"),
                      names_to = "day",
                      names_prefix = "X",
                      values_to = "cases") %>%
  dplyr::mutate(day = as.numeric(day)) %>%
  dplyr::mutate(date = ymd("1978-01-22")  + day-1) %>%
  dplyr::mutate(true_model = factor(true_model_idx, levels = c(1,2,3),
                                    labels = c(TeX("$M_*=M_1$"),
                                               TeX("$M_*=M_2$"),
                                               TeX("$M_*=M_3$"))))

# # full plot with facets
# ggplot() +
#   scale_color_viridis_d() +
#   labs(y = "# students in bed",
#        x = "Date") +
#   scale_x_date(labels = date_format("%b %d"),
#                breaks = c(as.Date("1978-01-22"),
#                           as.Date("1978-01-28"),
#                           as.Date("1978-02-04")),
#                minor_breaks = "1 day") +
#   scale_y_continuous(limits = c(0, 600), expand=c(0,0)) +
#   theme(text = element_text(size=20),
#         panel.grid.minor = element_line(),
#         panel.grid.major.y = element_line(),
#         axis.text.x = element_text(size=8)) +
#   geom_line(aes(x = date, y = cases, group=k), color="black", alpha=0.15) +
#   facet_grid(~true_model, labeller = label_parsed) +
#   geom_line(data = influenza_england_1978_school %>% dplyr::mutate(k=1), aes(x = date, y = in_bed), color = c_data, size=2)+
#   geom_point(data = influenza_england_1978_school%>% dplyr::mutate(k=1), aes(x = date, y = in_bed), color = c_data, size=5)
# ggsave("output/plots/experiment-2-simulated-observed-data.pdf", width=12, height=4)

for (j in 1:3){
  d %>% filter(true_model_idx==j) %>%
  ggplot() +
    scale_color_viridis_d() +
    labs(y = "# students in bed",
         x = "Date") +
    scale_x_date(labels = date_format("%b %d"),
                 breaks = c(as.Date("1978-01-22"),
                            as.Date("1978-01-28"),
                            as.Date("1978-02-04")),
                 minor_breaks = "1 day") +
    scale_y_continuous(limits = c(0, 600), expand=c(0,0)) +
    theme(text = element_text(size=20),
          panel.grid.minor = element_line(),
          panel.grid.major.y = element_line(),
          axis.text.x = element_text(size=8)) +
    geom_line(aes(x = date, y = cases, group=k), color="black", alpha=0.15) +
    geom_line(data = influenza_england_1978_school %>% dplyr::mutate(k=1), aes(x = date, y = in_bed), color = c_data, size=2)+
    geom_point(data = influenza_england_1978_school%>% dplyr::mutate(k=1), aes(x = date, y = in_bed), color = c_data, size=5)
  ggsave("output/plots/experiment-2-simulated-observed-data.pdf", width=12, height=4)
}



# Compute PMPs (level 2) ----

# for (k in 1:K){
#   print(paste0("----------------— Run k = ", k, " ----------------—"))
#   true_model_idx = df_wide$true_model_idx[k]
#   print(paste0("True model: M", true_model_idx))
#
#   simulated_cases = simulated_data_matrix[k, ]
#   simulated_data = list(n_days  = n_days, y0 = y0, t0 = t0, ts = t, N=N, cases=simulated_cases)
#
#   fit_1 = sampling(M[[1]], data = c(simulated_data, prior_predictive=0), chains = 4, cores=4, iter = niter, seed = 0, refresh = 0, show_messages=FALSE)
#   fit_2 = sampling(M[[2]], data = c(simulated_data, prior_predictive=0), chains = 4, cores=4, iter = niter, seed = 0, refresh = 0, show_messages=FALSE)
#   fit_3 = sampling(M[[3]], data = c(simulated_data, prior_predictive=0), chains = 4, cores=4, iter = niter, seed = 0, refresh = 0, show_messages=FALSE)
#
#
#   logml_m1 = bridgesampling::bridge_sampler(fit_1, silent=TRUE)
#   logml_m2 = bridgesampling::bridge_sampler(fit_2, silent=TRUE)
#   logml_m3 = bridgesampling::bridge_sampler(fit_3, silent=TRUE)
#
#   pmp = bridgesampling::post_prob(logml_m1, logml_m2, logml_m3)
#   print(pmp %>% round(4) %>% unname())
#   df_wide$pmp1[k] = pmp[1]
#   df_wide$pmp2[k] = pmp[2]
#   df_wide$pmp3[k] = pmp[3]
#
#   ggplot(df_wide, aes(pmp = ggsimplex::make_list_column(pmp1, pmp2, pmp3))) +
#     coord_fixed(ratio = 1, xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1)) +
#     theme_void() +
#     ggsimplex::geom_simplex_point(size=1) +
#     ggsimplex::geom_simplex_canvas() +
#     facet_grid(~ true_model, labeller = label_parsed) +
#     theme(strip.text = element_text(size = 18)) +
#     labs(title = paste0("k = ", k))
#
#   ggsave("output/plots/experiment-2-level-2.pdf")
#   save(df_wide, file="output/computations/experiment-2-df_wide.RData")
# }

# level 3 ----
df_wide = df_wide[1:100, ]
D = 1000
data = data.frame(
  true_model_idx = df_wide$true_model_idx,
  true_model = df_wide$true_model,
  pmp1 = df_wide$pmp1,
  pmp2 = df_wide$pmp2,
  pmp3 = df_wide$pmp3
)
data$pmp = with(data, cbind(pmp1, pmp2, pmp3))
meta_model_posteriors = get_meta_model_posteriors(data, n_posterior_draws = D)
df_level_3 = data.frame(
  true_model_idx = 1:3
) %>%
  dplyr::mutate(true_model = factor(true_model_idx, levels = c(1,2,3),
                                    labels = c(TeX("$M_*=M_1$"),
                                               TeX("$M_*=M_2$"),
                                               TeX("$M_*=M_3$"))))
df_level_3$mu = rep(list(NA), nrow(df_level_3))
df_level_3$Sigma = rep(list(NA), nrow(df_level_3))
for (j in 1:3){
  row = which(df_level_3$true_model_idx==j)
  mu = meta_model_posteriors[[paste0("true_model_", j)]]$mu
  Sigma = meta_model_posteriors[[paste0("true_model_", j)]]$Sigma
  df_level_3$mu[row] = list(apply(mu, 2, mean))
  df_level_3$Sigma[row] = list(apply(Sigma, c(2, 3), mean))
}

ggplot() +
  coord_fixed(ratio = 1, xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1)) +
  theme_void() +
  ggsimplex::geom_simplex_canvas(fontsize=11)  +
  ggsimplex::stat_simplex_density(
    data = df_level_3,
    fun = brms::dlogistic_normal,
    args = alist(mu = mu, Sigma = Sigma),
    col_scale = 4
  ) +
  ggsimplex::geom_simplex_point(data = data,
                                aes(pmp=ggsimplex::make_list_column(pmp1, pmp2, pmp3)),
                                alpha = 1.0) +
  facet_grid(cols=vars(true_model), labeller = label_parsed) +
  theme(strip.text = element_text(size = 18))
ggsave("output/plots/experiment-2-level-2-level-3.pdf", width=8, height=3)

# predictive mixture ----
fit_1_obs <- sampling(M[[1]], data = c(sir_data, prior_predictive=0), iter = niter, chains = 4, seed = 0, cores=4)
fit_2_obs <- sampling(M[[2]], data = c(sir_data, prior_predictive=0), iter = niter, chains = 4, seed = 0, cores=4)
fit_3_obs <- sampling(M[[3]], data = c(sir_data, prior_predictive=0), iter = niter, chains = 4, seed = 0, cores=4)

fit_obs_list = list(fit_1_obs, fit_2_obs, fit_3_obs)

for (j in 1:3){
  fit_obs = fit_obs_list[[j]]
  pred <- cbind(as.data.frame(summary(
    fit_obs, pars = "pred_cases", probs = c(0.055, 0.5, 0.945))$summary), t, cases, date=influenza_england_1978_school$date)
  colnames(pred) <- make.names(colnames(pred)) # to remove % in the col names
  ggplot() +
    # prior predictive
    geom_line(data = d %>% filter(true_model_idx==j), aes(x = date, y = cases, group=k), color="black", alpha=0.10) +
    # posterior predictive
    geom_ribbon(data=pred, mapping=aes(x=date, ymin = X5.5., ymax = X94.5.), fill = c_posterior, alpha = 0.35) +
    geom_line(data=pred, mapping=aes(x = date, y = X50.), color = c_posterior, size=2) +
    scale_x_date(labels = date_format("%b %d"),
                 breaks = c(as.Date("1978-01-22"),
                            as.Date("1978-01-28"),
                            as.Date("1978-02-04")),
                 minor_breaks = "1 day")+
    # data
    geom_point(data=pred, mapping=aes(x=date, y = cases), color = c_data, size=5) +
    geom_line(data=pred, mapping=aes(x=date, y = cases), color = c_data, size=1) +
    scale_y_continuous(limits = c(0, 600), expand=c(0,0)) +
    theme(text = element_text(size=20),
          panel.grid.minor = element_line(),
          panel.grid.major.y = element_line(),
          axis.text.x = element_text(size=8))+
    labs(x = "Date", y = "# students in bed")
  ggsave(paste0("output/plots/experiment-2-posterior-predictive-", j, ".pdf"), width=6, height=4)

}


d %>% filter(true_model_idx==j) %>%
  ggplot() +
  scale_color_viridis_d() +
  labs(y = "# students in bed",
       x = "Date") +
  scale_x_date(labels = date_format("%b %d"),
               breaks = c(as.Date("1978-01-22"),
                          as.Date("1978-01-28"),
                          as.Date("1978-02-04")),
               minor_breaks = "1 day") +
  scale_y_continuous(limits = c(0, 600), expand=c(0,0)) +
  theme(text = element_text(size=20),
        panel.grid.minor = element_line(),
        panel.grid.major.y = element_line(),
        axis.text.x = element_text(size=8)) +
  geom_line(aes(x = date, y = cases, group=k), color="black", alpha=0.15) +
  geom_line(data = influenza_england_1978_school %>% dplyr::mutate(k=1), aes(x = date, y = in_bed), color = c_data, size=2)+
  geom_point(data = influenza_england_1978_school%>% dplyr::mutate(k=1), aes(x = date, y = in_bed), color = c_data, size=5)



ml_1_obs = bridge_sampler(fit_1_obs)
ml_2_obs = bridge_sampler(fit_2_obs)
ml_3_obs = bridge_sampler(fit_3_obs)

pmp_obs = bridgesampling::post_prob(ml_1_obs, ml_2_obs, ml_3_obs)

print(round(pmp_obs, 2))

pmp_obs_df = data.frame(pmp1 = pmp_obs[1],
                        pmp2 = pmp_obs[2],
                        pmp3 = pmp_obs[3]
)

mixture_function = purrr::partial(logistic_normal_mixture,
                                  theta = pmp_obs,
                                  mu_list = list(
                                    df_level_3$mu[df_level_3$true_model_idx==1][[1]],
                                    df_level_3$mu[df_level_3$true_model_idx==2][[1]],
                                    df_level_3$mu[df_level_3$true_model_idx==3][[1]]
                                  ),
                                  Sigma_list = list(
                                    df_level_3$Sigma[df_level_3$true_model_idx==1][[1]],
                                    df_level_3$Sigma[df_level_3$true_model_idx==2][[1]],
                                    df_level_3$Sigma[df_level_3$true_model_idx==3][[1]]
                                  )
)

pmp_obs_cartesian = pmp_obs %*% matrix(c(0, 0, 1, 0, 0.5, sqrt(3)/2), byrow=TRUE, ncol=2)
ggplot() +
  coord_fixed(ratio = 1, xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1)) +
  theme_void() +
  ggsimplex::stat_simplex_density(
    data = data.frame(dummy=0),
    fun = mixture_function,
    args = alist(dummy=dummy),
    col_scale = 4
  ) +
  ggsimplex::geom_simplex_point(data = pmp_obs_df, aes(pmp = ggsimplex::make_list_column(pmp1, pmp2, pmp3)),
                                size = 1.7, shape=21, colour = "white", alpha = 1.0) +
  ggsimplex::geom_simplex_point(data = pmp_obs_df, aes(pmp = ggsimplex::make_list_column(pmp1, pmp2, pmp3)),
                                size = 1.5, shape=16, colour = "magenta", alpha = 1.0) +
  annotate(
    geom = "curve", x = 0.1, y = 0.5, xend=pmp_obs_cartesian[1]-0.02, yend = pmp_obs_cartesian[2]+0.02,
    curvature = .3, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text", x = 0.1, y = 0.51, label = TeX(r'($\mathring{\pi})'), hjust = "center", vjust="bottom", size=10) +
  ggsimplex::geom_simplex_canvas(fontsize=20)
ggsave(paste0("output/plots/experiment-2-level-3-predictive-mixture.pdf"), width=4, height=4)
