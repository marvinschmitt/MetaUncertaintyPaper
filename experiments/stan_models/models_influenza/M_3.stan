functions {
  real[] seir(real t, real[] y, real[] theta,
             real[] x_r, int[] x_i) {

      real N = x_i[1];

      real beta = theta[1];
      real gamma = theta[2];
      real eta = theta[3];
      //real i0 = theta[4];
      //real e0 = theta[5];

      real S = y[1];
      real E = y[2];
      real I = y[3];
      real R = y[4];

      real dS_dt = -beta * I * S / N;
      real dE_dt =  beta * I * S / N - eta * E;
      real dI_dt = eta * E - gamma * I;
      real dR_dt =  gamma * I;

      return {dS_dt, dE_dt, dI_dt, dR_dt};
  }
}
data {
  int<lower=1> n_days;
  real y0[4];
  real t0;
  real ts[n_days];
  int N;
  int cases[n_days];
  int prior_predictive;
}
transformed data {
  real x_r[0];
  int x_i[1] = { N };
}
parameters {
  real<lower=0> gamma;
  real<lower=0> beta;
  real<lower=0> eta;
  real<lower=0> phi;
  //real<lower=0, upper=1> p_reported; // proportion of infected (symptomatic) people reported
  //real<lower=0> i0; // number of infected people inititally
  //real<lower=0> e0; // number of exposed people inititally
}
transformed parameters{
  real y[n_days, 4];
  //real phi = 1. / phi_inv;
  real theta[3] = {beta, gamma, eta};//, i0, e0};
  y = integrate_ode_rk45(seir, y0, t0, ts, theta, x_r, x_i);
}
model {
  //priors
  target += normal_lpdf(beta | 3, 0.1) - normal_lccdf(0 | 3, 0.1);
  target += normal_lpdf(gamma | 0.5, 0.1) - normal_lccdf(0 | 0.5, 0.1);
  target += normal_lpdf(eta | 3, 0.1) - normal_lccdf(0 | 3, 0.1);
  target += normal_lpdf(phi | 100, 1) - normal_lccdf(0 | 100, 1);
  //p_reported ~ beta(1, 2);
  //i0 ~ normal(0, 2);
  //e0 ~ normal(0, 2);

  //sampling distribution
  //col(matrix x, int n) - The n-th column of matrix x. Here the number of infected people
  if (prior_predictive ==0){
    target += neg_binomial_2_lpmf(cases | col(to_matrix(y), 3), phi);
  }
}
generated quantities {
  real R0 = beta / gamma;
  real recovery_time = 1 / gamma;
  real incubation_time = 1 / eta;
  real pred_cases[n_days];
  pred_cases = neg_binomial_2_rng(col(to_matrix(y), 3)+ 1e-5, phi);
}
