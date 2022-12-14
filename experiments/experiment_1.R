set.seed(152)

library(magrittr)
library(ggplot2)
library(latex2exp)
library(MetaUncertaintyPaper)

# Model Formulation ####
model_1 = list(
  predictors = c("x1", "x2", "x3"),
  cov_X = diag(rep(1, times = 5)),
  prior = list(
    family = "normal-inv-gamma",
    mu_0 = matrix(c(0, 0, 0), ncol = 1),
    Lambda_0 = diag(rep(5, 3)),
    a_0 = 1,
    b_0 = 1
  )
)

model_2 = list(
  predictors = c("x1", "x2", "x4"),
  cov_X = diag(rep(1, times = 5)),
  prior = list(
    family = "normal-inv-gamma",
    mu_0 = matrix(c(0,0,0), ncol = 1),
    Lambda_0 = diag(rep(5, 3)),
    a_0 = 1,
    b_0 = 1
  )
)

model_3 = list(
  predictors = c("x1", "x2", "x5"),
  cov_X = diag(rep(1, times = 5)),
  prior = list(
    family = "normal-inv-gamma",
    mu_0 = matrix(c(0, 0, 0), ncol = 1),
    Lambda_0 = diag(rep(5, 3)),
    a_0 = 1,
    b_0 = 1
  )
)

model_4 = list(
  predictors = c("x3", "x4", "x5"),
  cov_X = diag(rep(1, times = 5)),
  prior = list(
    family = "normal-inv-gamma",
    mu_0 = matrix(c(10, 10, 10), ncol = 1),
    Lambda_0 = diag(rep(10, 3)),
    a_0 = 1,
    b_0 = 1
  )
)

M = list(model_1, model_2, model_3)

K = 100
N_list = c(5, 10, 100)

# Level 2 Uncertainty ####

df_level_2 = simulate_data_and_pmps(M = M, K = K, N = N_list[1])[["df_wide"]]
for (N in N_list[2:length(N_list)]){ # more readable than N_list[-1]
  df_level_2 = rbind(df_level_2,
                     simulate_data_and_pmps(M = M,
                                            K = K,
                                            N = N)[["df_wide"]])
}

df_level_2 = df_level_2 %>%
  dplyr::mutate(N_label = factor(N, levels = N_list,
                    labels = sapply(paste0("$N = ", N_list, "$"), TeX)))

ggplot(df_level_2, aes(pmp=ggsimplex::make_list_column(pmp_1, pmp_2, pmp_3))) +
  coord_fixed(ratio = 1, xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1)) +
  theme_void() +
  ggsimplex::geom_simplex_canvas(fontsize=12)  +
  ggsimplex::geom_simplex_point(alpha=0.60) +
  facet_grid(rows=vars(N_label), cols=vars(true_model), labeller = label_parsed, switch="y") +
  theme(strip.text = element_text(size = 18))
ggsave("output/plots/experiment-1-level-2.pdf", width = 8, height = 8, units = "in")

# Fitting Meta Models ####
# N_obs = 10
# df_wide = df_level_2 %>%
#   dplyr::filter(N == N_obs)
# D = 1000 # number of posterior draws per chain
# #df_list = simulate_data_and_pmps(M = M, K = K, N = N)
# #df_wide = df_list[["df_wide"]]
# #df_long = df_list[["df_long"]]
# #remove(df_list)
#
# data = data.frame(k = 1:nrow(df_wide),
#                   true_model = df_wide$true_model,
#                   true_model_idx = df_wide$true_model_idx)
#
# data[, c("pmp1", "pmp2", "pmp3")] <-
#   continuity_correction(df_wide[, c("pmp_1", "pmp_2", "pmp_3")])
#
# data$pmp = with(data, cbind(pmp1, pmp2, pmp3))
#
# meta_model_posteriors = get_meta_model_posteriors(data, n_posterior_draws = D)
#
# save(meta_model_posteriors, file = "output/computations/experiment_1_meta_model_posteriors.RData")

# Level 3 Uncertainty ####

df_level_3 = data.frame(
  true_model_idx = rep(1:3, times=length(N_list)),
  N = rep(N_list, each=3)
) %>%
  dplyr::mutate(true_model = factor(true_model_idx, levels = c(1,2,3),
                                    labels = c(TeX("$M_*=M_1$"),
                                               TeX("$M_*=M_2$"),
                                               TeX("$M_*=M_3$")))) %>%
  dplyr::mutate(N_label = factor(N, levels = N_list,
                                 labels = sapply(paste0("$N = ", N_list, "$"), TeX)))
df_level_3$mu = rep(list(NA), nrow(df_level_3))
df_level_3$Sigma = rep(list(NA), nrow(df_level_3))
for (N_obs in N_list){
  df_wide = df_level_2 %>%
    dplyr::filter(N==N_obs)
  D = 1000
  data = data.frame(k = 1:nrow(df_wide),
                    true_model = df_wide$true_model,
                    true_model_idx = df_wide$true_model_idx)

  data[, c("pmp1", "pmp2", "pmp3")] <-
    continuity_correction(df_wide[, c("pmp_1", "pmp_2", "pmp_3")])

  data$pmp = with(data, cbind(pmp1, pmp2, pmp3))

  meta_model_posteriors = get_meta_model_posteriors(data, n_posterior_draws = D)

  for (j in 1:3){
    row = which(df_level_3$true_model_idx==j & df_level_3$N == N_obs)
    mu = meta_model_posteriors[[paste0("true_model_", j)]]$mu
    Sigma = meta_model_posteriors[[paste0("true_model_", j)]]$Sigma
    df_level_3$mu[row] = list(apply(mu, 2, mean))
    df_level_3$Sigma[row] = list(apply(Sigma, c(2, 3), mean))
  }

}


ggplot() +
  coord_fixed(ratio = 1, xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1)) +
  theme_void() +
  ggsimplex::geom_simplex_canvas(fontsize=11)  +
  ggsimplex::stat_simplex_density(
    data = df_level_3,
    fun = brms::dlogistic_normal,
    args = alist(mu = mu, Sigma = Sigma),
    col_scale = "linear"
  ) +
  ggsimplex::geom_simplex_point(data = df_level_2,
                                aes(pmp=ggsimplex::make_list_column(pmp_1, pmp_2, pmp_3)),
                                alpha = 0.80) +
  facet_grid(rows=vars(N_label), cols=vars(true_model), labeller = label_parsed, switch="y") +
  theme(strip.text = element_text(size = 18))
ggsave("output/plots/experiment-1-level-2-level-3.pdf", width=8, height=8)


# Predictive Mixture N=10####

# for (i in 1:100){
#   set.seed(i)
#   data_obs = simulate_data_bayesian_linear_regression(model_3, N = N_obs)$data
#   prior_model_prob = rep(1/3, 3)
#   log_ml_obs = c(
#     calculate_ml_bayesian_linear_regression(model = model_1, data = data_obs, return_log=TRUE),
#     calculate_ml_bayesian_linear_regression(model = model_2, data = data_obs, return_log=TRUE),
#     calculate_ml_bayesian_linear_regression(model = model_3, data = data_obs, return_log=TRUE)
#   )
#   log_pmp_normalization = log(sum(prior_model_prob * exp(log_ml_obs)))
#   pmp_obs = exp(log_ml_obs + log(prior_model_prob) - log_pmp_normalization)
#   print(c(i, pmp_obs %>% round(2)))
# }

set.seed(0)
data_obs_1 = simulate_data_bayesian_linear_regression(model_1, N = N_obs)$data

set.seed(72) # 17, 72
data_obs_2 = simulate_data_bayesian_linear_regression(model_3, N = N_obs)$data

set.seed(6)
data_obs_3 = simulate_data_bayesian_linear_regression(model_2, N = N_obs)$data

data_obs_list = list(data_obs_1, data_obs_2, data_obs_3)

for (i in 1:3){
  N_obs = 10
  data_obs = data_obs_list[[i]]
  prior_model_prob = rep(1/3, 3)
  log_ml_obs = c(
    calculate_ml_bayesian_linear_regression(model = model_1, data = data_obs, return_log=TRUE),
    calculate_ml_bayesian_linear_regression(model = model_2, data = data_obs, return_log=TRUE),
    calculate_ml_bayesian_linear_regression(model = model_3, data = data_obs, return_log=TRUE)
  )

  log_pmp_normalization = log(sum(prior_model_prob * exp(log_ml_obs)))

  pmp_obs = exp(log_ml_obs + log(prior_model_prob) - log_pmp_normalization)
  pmp_obs_df = data.frame(pmp1 = pmp_obs[1],
                          pmp2 = pmp_obs[2],
                          pmp3 = pmp_obs[3]
  )
  print(pmp_obs %>% round(2))


    mixture_function = purrr::partial(logistic_normal_mixture,
                                      theta = pmp_obs,
                                      mu_list = list(
                                        df_level_3$mu[df_level_3$N == N_obs & df_level_3$true_model_idx==1][[1]],
                                        df_level_3$mu[df_level_3$N == N_obs & df_level_3$true_model_idx==2][[1]],
                                        df_level_3$mu[df_level_3$N == N_obs & df_level_3$true_model_idx==3][[1]]
                                      ),
                                      Sigma_list = list(
                                        df_level_3$Sigma[df_level_3$N == N_obs & df_level_3$true_model_idx==1][[1]],
                                        df_level_3$Sigma[df_level_3$N == N_obs & df_level_3$true_model_idx==2][[1]],
                                        df_level_3$Sigma[df_level_3$N == N_obs & df_level_3$true_model_idx==3][[1]]
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
        col_scale = "linear"
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
    ggsave(paste0("output/plots/experiment-1-level-3-predictive-mixture-x-", i, ".pdf"), width=4, height=4)
}




# clustering ####
load(file = "output/computations/experiment_1_meta_model_posteriors.RData")

n_clusters = 3

cluster_center_list = lapply(meta_model_posteriors, function(m){
  M = matrix(data = 0.0,
             nrow = 2*D, # D draws per chain (2 chains in total)
             ncol = 2 + 2 + 1) # J-1 + sum(J-1)

  M[, 1:2] = m$mu
  M[, 3:5] = t(apply(m$Sigma, 1, function(a) a[lower.tri(a, diag=TRUE)]))
  cluster_centers = stats::kmeans(M, centers=5)$centers
  return(cluster_centers)
})

cluster_center_df = data.frame(
  true_model_idx = rep(1:3, each = n_clusters),
  cluster_idx = rep(1:n_clusters, times = 3),
  mu = NA,
  Sigma = NA
) %>%
  dplyr::mutate(true_model = factor(true_model_idx, levels = c(1,2,3),
                                    labels = c(TeX("$M_*=M_1$"),
                                               TeX("$M_*=M_2$"),
                                               TeX("$M_*=M_3$")))) %>%
  dplyr::mutate(cluster = factor(cluster_idx, levels = 1:n_clusters,
                                 labels = sapply(paste0("Cluster $", 1:n_clusters, "$"), TeX)))

for (i in 1:nrow(cluster_center_df)){
  true_model_idx = cluster_center_df[i, "true_model_idx"]
  cluster_idx = cluster_center_df[i, "cluster_idx"]
  cluster_center_df$mu[i] = list(cluster_center_list[[true_model_idx]][cluster_idx, 1:2])
  cluster_center_df$Sigma[i] = list(flat_lower_triag_to_symmetric(cluster_center_list[[true_model_idx]][cluster_idx, 3:5]))
}


ggplot() +
  coord_fixed(ratio = 1, xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1)) +
  theme_void() +
  ggsimplex::geom_simplex_canvas(fontsize=12)  +
  ggsimplex::stat_simplex_density(
    data = cluster_center_df,
    fun = brms::dlogistic_normal,
    args = alist(mu = mu, Sigma = Sigma),
    col_scale = "linear"
  ) +
  facet_grid(rows=vars(cluster), cols=vars(true_model), labeller = label_parsed, switch="y") +
  theme(strip.text = element_text(size = 18))
ggsave("output/plots/experiment-1-clustering.pdf", width=8, height=8)


# Level 3 postpred #####
prior_model_prob = rep(1/3, 3)
data_obs = simulate_data_bayesian_linear_regression(model_1, N = N_obs)$data
log_ml_obs = c(
  calculate_ml_bayesian_linear_regression(model = model_1, data = data_obs, return_log=TRUE),
  calculate_ml_bayesian_linear_regression(model = model_2, data = data_obs, return_log=TRUE),
  calculate_ml_bayesian_linear_regression(model = model_3, data = data_obs, return_log=TRUE)
)

log_pmp_normalization = log(sum(prior_model_prob * exp(log_ml_obs)))

pmp_obs = exp(log_ml_obs + log(prior_model_prob) - log_pmp_normalization)
pmp_obs_df = data.frame(pmp1 = pmp_obs[1],
                        pmp2 = pmp_obs[2],
                        pmp3 = pmp_obs[3]
                        )

for (cluster_idx in 1:n_clusters){
  mixture_function = purrr::partial(logistic_normal_mixture,
                                    theta = pmp_obs,
                                    mu_list = list(
                                      cluster_center_df[cluster_center_df$cluster_idx==cluster_idx & cluster_center_df$true_model_idx==1, "mu"][[1]],
                                      cluster_center_df[cluster_center_df$cluster_idx==cluster_idx & cluster_center_df$true_model_idx==2, "mu"][[1]],
                                      cluster_center_df[cluster_center_df$cluster_idx==cluster_idx & cluster_center_df$true_model_idx==3, "mu"][[1]]
                                    ),
                                    Sigma_list = list(
                                      cluster_center_df[cluster_center_df$cluster_idx==cluster_idx & cluster_center_df$true_model_idx==1, "Sigma"][[1]],
                                      cluster_center_df[cluster_center_df$cluster_idx==cluster_idx & cluster_center_df$true_model_idx==2, "Sigma"][[1]],
                                      cluster_center_df[cluster_center_df$cluster_idx==cluster_idx & cluster_center_df$true_model_idx==3, "Sigma"][[1]]
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
      col_scale = "linear"
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
  ggsave(paste0("output/plots/experiment-1-level-3-postpred-cluster-", cluster_idx, ".pdf"), width=4, height=4)
}

