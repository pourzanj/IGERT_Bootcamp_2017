data {
  int N;
}

parameters {
  real<lower=0,upper=1> theta;
}

model {
  theta ~ beta(14,102);
  N ~ binomial(39, theta);
}
