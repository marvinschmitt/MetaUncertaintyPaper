# https://mc-stan.org/users/documentation/case-studies/boarding_school_case_study.html

library(tidyverse)
library(rstan)
library(gridExtra)
library(tidybayes)

rstan_options (auto_write = TRUE)
options (mc.cores = parallel::detectCores ())

theme_set(theme_bw())
c_light <- c("#fee0d2")
c_mid <- c("#fc9272")
c_dark <- c("#de2d26")
c_simu <- "chartreuse3"
c_posterior = "orange"
c_prior = "aquamarine2"

set.seed(1) # for reproductibility


df_swiss <- read_csv("data/swiss_agg_data.csv")


df_swiss %>%
  ggplot() +
  geom_bar(mapping = aes(x = date, y = report_dt), fill = c_mid, color = c_dark, stat = "identity") +
  labs(y="Number of reported cases")


N <- 8.57E6;
i0 <- 1
s0 <- N - i0
r0 <- 0
y0 = c(S = s0, I = i0, R = r0)

# Cases
cases <- df_swiss$report_dt

# times
n_days <- length(cases)
t <- seq(1, n_days, by = 1)
t0 = 0
t <- t

data_sir <- list(n_days = n_days, y0 = y0, t0 = t0, ts = t, N = N, cases = cases)

model_1 <- stan_model("experiments/stan_models/models_swiss/sir_underreporting.stan")

fit_1 <- sampling(model_1,
                                   data_forcing_survey,
                                   iter=1000,
                                   seed = 0)
check_hmc_diagnostics(fit_1)



date_switch <- "2020-03-13" # date of introduction of control measures
tswitch <- df_swiss %>% filter(date < date_switch) %>% nrow() + 1 # convert time to number

data_forcing <- list(n_days = n_days, y0 = y0, t0 = t0, ts = t, N = N, cases = cases, tswitch = tswitch)
model_2 <- stan_model("experiments/stan_models/models_swiss/seir_forcing.stan")
#fit_forcing <- readRDS("saved_fits/fit_swiss/fit_forcing.RDS")
fit_2 <- sampling(model_2,
                        data_forcing,
                        iter=1000,
                        seed=4)


date_survey_left <- "2020-05-04"
date_survey_right <- "2020-05-07"
t_survey_start <- df_swiss %>% filter(date < date_survey_left) %>% nrow() + 1 # convert time to number
t_survey_end <- df_swiss %>% filter(date < date_survey_right) %>% nrow() + 1 # convert time to number
n_infected_survey <-  83
n_tested_survey <-  775
# add these data to the data given to stan
data_forcing_survey <- c(data_forcing, list(t_survey_start = t_survey_start,
                                            t_survey_end = t_survey_end,
                                            n_infected_survey = n_infected_survey,
                                            n_tested_survey = n_tested_survey))
