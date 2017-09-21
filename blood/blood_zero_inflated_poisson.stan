data {
  int N;
  int y[N];
}

parameters {
  real<lower=0, upper=1> theta;
  real<lower=0> lambda;
}

model {
  lambda ~ normal(0,100);
  theta ~ beta(1,1);

  for (n in 1:N) {
      if (y[n] == 0)
        target += log_sum_exp(bernoulli_lpmf(1 | theta),
                              bernoulli_lpmf(0 | theta) + poisson_lpmf(y[n] | lambda));
      else
        target += bernoulli_lpmf(0 | theta) + poisson_lpmf(y[n] | lambda);
  }
                                
}
