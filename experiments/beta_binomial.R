set.seed(152)

library(magrittr)
library(ggplot2)
library(latex2exp)
library(MetaUncertaintyPaper)




df_level_2 = read.csv("output/computations/beta_binomial_pmps.csv") %>%
  dplyr::mutate(true_model = factor(true_model_idx, levels = c(1,2,3),
                                    labels = c(TeX("$M_*=M_1$"),
                                               TeX("$M_*=M_2$"),
                                               TeX("$M_*=M_3$"))))


ggplot() +
  coord_fixed(ratio = 1, xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1)) +
  theme_void() +
  ggsimplex::geom_simplex_canvas(fontsize=12)  +
  ggsimplex::geom_simplex_point(data = df_level_2,
                                aes(pmp = ggsimplex::make_list_column(pmp1, pmp2, pmp3)),
                                alpha=0.4) +
  facet_grid(cols=vars(true_model_idx)) +
  theme(strip.text = element_text(size = 18))
ggsave("output/plots/experiment-1-level-2.pdf", width = 8, height = 8, units = "in")
