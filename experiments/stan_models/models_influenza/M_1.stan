functions {
  real[] sir(real t, real[] y, real[] theta,
             real[] x_r, int[] x_i) {

      real S = y[1];
      real I = y[3];
      real R = y[4];
      real N = x_i[1];

      real beta = theta[1];
      real gamma = theta[2];

      real dS_dt = -beta * I * S / N;
      real dI_dt =  beta * I * S / N - gamma * I;
      real dR_dt =  gamma * I;

      return {dS_dt, 0.0, dI_dt, dR_dt};
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
}
transformed parameters{
  real y[n_days, 4];
  real theta[2] = {beta, gamma};

  y = integrate_ode_rk45(sir, y0, t0, ts, theta, x_r, x_i);
}
model {
  //priors
  target += normal_lpdf(beta | 2, 0.1) - normal_lccdf(0 | 2, 0.1);
  target += normal_lpdf(gamma | 0.4, 0.1) - normal_lccdf(0 | 0.4, 0.1);

  //sampling distribution
  if (prior_predictive == 0){
    target += poisson_lpmf(cases | col(to_matrix(y), 3));
  }

}
generated quantities {
  real R0 = beta / gamma;
  real recovery_time = 1 / gamma;
  real pred_cases[n_days];

  //col(matrix x, int n) - The n-th column of matrix x. Here the number of infected people
  pred_cases = poisson_rng(col(to_matrix(y), 3) + 1e-5);
}

