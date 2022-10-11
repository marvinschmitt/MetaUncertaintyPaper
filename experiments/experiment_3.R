library(magrittr)
library(ggplot2)
library(latex2exp)
library(MetaUncertaintyPaper)
set.seed(129)


# BF Dirichlet visualizations for observed data sets ####
for (i in c(1)){
  alphas = read.csv(paste0("output/computations/experiment-3-alpha-", i, ".csv")) %>%
    t() %>%
    as.vector()
  print(alphas)
  pmp_obs = alphas/sum(alphas)
  print(paste0("x_", i, ": ", paste(pmp_obs %>% round(4), collapse=", ")))
  pmp_obs_df = data.frame(pmp1 = pmp_obs[1],
                          pmp2 = pmp_obs[2],
                          pmp3 = pmp_obs[3]
  )
  d = data.frame(NA)
  d$Alpha = list(alphas)
  ggplot() +
    coord_fixed(ratio = 1, xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1)) +
    theme_void() +
    ggsimplex::stat_simplex_density(
      data = d,
      fun = brms::ddirichlet,
      args = alist(Alpha = Alpha),
      col_scale = "linear"
    ) +
    ggsimplex::geom_simplex_canvas(fontsize=19) +
    ggsimplex::geom_simplex_point(data = pmp_obs_df, aes(pmp = ggsimplex::make_list_column(pmp1, pmp2, pmp3)),
                                  size = 1.7, shape=21, colour = "white", alpha = 1.0) +
    ggsimplex::geom_simplex_point(data = pmp_obs_df, aes(pmp = ggsimplex::make_list_column(pmp1, pmp2, pmp3)),
                                  size = 1.5, shape=16, colour = "magenta", alpha = 1.0) +
    annotate(
      geom = "curve", x = 0.1, y = 0.5, xend=pmp_obs_cartesian[1]-0.02, yend = pmp_obs_cartesian[2]+0.02,
      curvature = .3, arrow = arrow(length = unit(2, "mm"))
    ) +
    annotate(geom = "text", x = 0.1, y = 0.51, label = TeX(r'($\mathring{\pi})'), hjust = "center", vjust="bottom", size=10)
  ggsave(paste0("output/plots/experiment-3-bf-dirichlet-", i, ".pdf"), width=4, height=4)
}


# Level 2 ####

data = read.csv("output/computations/experiment-3-simulated-data.csv") %>%
  dplyr::select(-X) %>%
  dplyr::mutate(alpha_sum = alpha_1 + alpha_2 + alpha_3) %>%
  dplyr::mutate(pmp1 = alpha_1 / alpha_sum,
                pmp2 = alpha_2 / alpha_sum,
                pmp3 = alpha_3 / alpha_sum) %>%
  dplyr::mutate(true_model_idx = true_model) %>%
  dplyr::mutate(true_model = factor(true_model_idx, levels = c(1,2,3),
                                    labels = c(TeX("$M_*=M_1$"),
                                               TeX("$M_*=M_2$"),
                                               TeX("$M_*=M_3$"))))

data[c("pmp1", "pmp2", "pmp3")] <-
  continuity_correction(data[c("pmp1", "pmp2", "pmp3")])

  data  %>%
    ggplot(aes(pmp = ggsimplex::make_list_column(pmp1, pmp2, pmp3))) +
    coord_fixed(ratio = 1, xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1)) +
    theme_void() +
    ggsimplex::geom_simplex_point(color="#22A88477") +
    ggsimplex::geom_simplex_canvas(fontsize=12) +
    facet_grid(cols=vars(true_model),
               labeller = label_parsed) +
    theme(strip.text = element_text(size = 18))
  ggsave("output/plots/experiment-3-model-implied-pmp.pdf", width=8, height=3, units="in")

# Fitting Meta Models ####
  D = 1000
  data$pmp = with(data, cbind(pmp1, pmp2, pmp3))
  #meta_model_posteriors = get_meta_model_posteriors(data, n_posterior_draws = D)
  #save(meta_model_posteriors, file = "output/computations/experiment_3_meta_model_posteriors.RData")
  load("output/computations/experiment_3_meta_model_posteriors.RData")
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
    col_scale = "linear"
  ) +
  ggsimplex::geom_simplex_point(data = data,
                                aes(pmp=ggsimplex::make_list_column(pmp1, pmp2, pmp3)),
                                alpha = 1.0) +
  facet_grid(cols=vars(true_model), labeller = label_parsed) +
  theme(strip.text = element_text(size = 18))
ggsave("output/plots/experiment-3-level-2-level-3.pdf", width=8, height=3)


# predictive mixture ####
i = 1 # only one observed data set at this point
alphas = read.csv(paste0("output/computations/experiment-3-alpha-", i, ".csv")) %>%
  t() %>%
  as.vector()
pmp_obs = alphas/sum(alphas)
print(paste0("x_", i, ": ", paste(pmp_obs %>% round(4), collapse=", ")))
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
ggsave(paste0("output/plots/experiment-3-level-3-predictive-mixture.pdf"), width=4, height=4)


# Clustering ####
  load("output/computations/experiment_3_meta_model_posteriors.RData")
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
    facet_grid(rows=vars(cluster), cols=vars(true_model), labeller = label_parsed) +
    theme(strip.text = element_text(size = 18))
  ggsave("output/plots/experiment-3-clustering.pdf", width=8, height=8)


# Predictive Mixture ###
i = 1 # only one observed data set at this point
alphas = read.csv(paste0("output/computations/experiment-3-alpha-", i, ".csv")) %>%
  t() %>%
  as.vector()
pmp_obs = alphas/sum(alphas)
print(paste0("x_", i, ": ", paste(pmp_obs %>% round(4), collapse=", ")))
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
  ggsave(paste0("output/plots/experiment-3-level-3-postpred-cluster-", cluster_idx, ".pdf"), width=4, height=4)
}


