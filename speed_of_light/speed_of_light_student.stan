data {
  int N;
  real y[N];
}

parameters {
  real mu;
  real<lower=0> sigma;
}

model {
  y ~ student_t(1, mu, sigma);
}
