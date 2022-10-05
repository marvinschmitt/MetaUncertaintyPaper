library(magrittr)
library(ggplot2)
library(latex2exp)
set.seed(129)


# BF Dirichlet visualizations for observed data sets ####
for (i in 1:2){
  alphas = read.csv(paste0("output/computations/experiment-2-alpha-", i, ".csv")) %>%
    t() %>%
    as.vector()
  print(alphas)
  d = data.frame(NA)
  d$Alpha = list(alphas)
  ggplot() +
    coord_fixed(ratio = 1, xlim = c(-0.05, 1.05), ylim = c(-0.05, 1.05)) +
    theme_void() +
    ggsimplex::stat_simplex_density(
      data = d,
      fun = brms::ddirichlet,
      args = alist(Alpha = Alpha),
      col_scale = "linear"
    ) +
    ggsimplex::geom_simplex_canvas(fontsize=19)
  ggsave(paste0("output/plots/experiment-2-bf-dirichlet-", i, ".pdf"), width=4, height=4)
}


# Level 2 ####

data = read.csv("output/computations/experiment-2-simulated-data.csv") %>%
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
    theme(strip.text = element_text(size = 12))
  ggsave("output/plots/experiment-2-model-implied-pmp.pdf", width=8, height=3, units="in")

# Fitting Meta Models ####
  D = 1000
  data$pmp = with(data, cbind(pmp1, pmp2, pmp3))
  meta_model_posteriors = get_meta_model_posteriors(data, n_posterior_draws = D)
  save(meta_model_posteriors, file = "output/computations/experiment_2_meta_model_posteriors.RData")

# Clustering ####
  load("output/computations/experiment_2_meta_model_posteriors.RData")
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
    true_model = rep(1:3, each = n_clusters),
    cluster = rep(1:n_clusters, times = 3),
    mu = NA,
    Sigma = NA
  )
  for (i in 1:nrow(cluster_center_df)){
    true_model = cluster_center_df[i, "true_model"]
    cluster_idx = cluster_center_df[i, "cluster"]
    cluster_center_df$mu[i] = list(cluster_center_list[[true_model]][cluster_idx, 1:2])
    cluster_center_df$Sigma[i] = list(flat_lower_triag_to_symmetric(cluster_center_list[[true_model]][cluster_idx, 3:5]))
  }

  cluster_center_df = cluster_center_df %>%
    dplyr::mutate(true_model = factor(true_model, levels = c(1,2,3),
                                      labels = c(TeX("$M_*=M_1$"),
                                                 TeX("$M_*=M_2$"),
                                                 TeX("$M_*=M_3$")))) %>%
    dplyr::mutate(cluster = factor(cluster, levels = 1:n_clusters,
                                   labels = sapply(paste0("Cluster $", 1:n_clusters, "$"), TeX)))


  ggplot() +
    coord_fixed(ratio = 1, xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1)) +
    theme_void() +
    ggsimplex::geom_simplex_canvas(fontsize=12)  +
    ggsimplex::stat_simplex_density(
      data = cluster_center_df,
      fun = brms::dlogistic_normal,
      args = alist(mu = mu, Sigma = Sigma),
      col_scale = "sqrt"
    ) +
    facet_grid(rows=vars(cluster), cols=vars(true_model), labeller = label_parsed) +
    theme(strip.text = element_text(size = 12))
  ggsave("output/plots/experiment-2-clustering.pdf")



# Predictive Mixture ###
for (i in 1:2){
  alphas = read.csv(paste0("output/computations/experiment-2-alpha-", i, ".csv")) %>%
    t() %>%
    as.vector()
  pmp_obs = alphas/sum(alphas)
  print(paste0("x_", i, ": ", paste(pmp_obs %>% round(4), collapse=", ")))


}


