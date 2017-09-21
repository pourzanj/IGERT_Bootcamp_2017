data {
  int N;
  int y[N];
}

parameters {
  real<lower=0, upper=1> theta;
  real<lower=0> alpha;
  real<lower=0> beta;
}

model {
  alpha ~ normal(0,100);
  beta ~ normal(0,100);
  theta ~ beta(1,1);

  for (n in 1:N) {
      if (y[n] == 0)
        target += log_sum_exp(bernoulli_lpmf(1 | theta),
                              bernoulli_lpmf(0 | theta) + neg_binomial_lpmf(y[n] | alpha, beta));
      else
        target += bernoulli_lpmf(0 | theta) + neg_binomial_lpmf(y[n] | alpha, beta);
  }
                                
}
