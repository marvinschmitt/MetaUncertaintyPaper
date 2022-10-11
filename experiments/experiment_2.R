library(magrittr)
library(ggplot2)
set.seed(129)


# BF Dirichlet visualizations for observed data sets ####
for (i in 1:2){
  alphas = read.csv(paste0("output/computations/experiment-3-alpha-", i, ".csv")) %>%
    t() %>%
    as.vector()
  d = data.frame(NA)
  d$Alpha = list(alphas)
  ggplot() +
    coord_fixed(ratio = 1, xlim = c(0, 1), ylim = c(0, 1)) +
    theme_void() +
    ggsimplex::stat_simplex_density(
      data = d,
      fun = brms::ddirichlet,
      args = alist(Alpha = Alpha),
      col_scale = "linear"
    ) +
    ggsimplex::geom_simplex_canvas(size=1)
}
