data {
  int J;    //number of locations of experiment
  
  int y[J]; //number of people who died
  int N[J]; //total number of people in the experiment
}

parameters {
  real<lower=0> alpha;
  real<lower=0> beta;
  real<lower=0,upper=1> theta[J];
}

model {
  theta ~ beta(alpha, beta);
  y ~ binomial(N, theta);
}
