data {
  int N;
  int y[N];
}

parameters {
  real<lower=0> lambda;
}

model {
  lambda ~ normal(0,100);
  y ~ poisson(lambda);
}
