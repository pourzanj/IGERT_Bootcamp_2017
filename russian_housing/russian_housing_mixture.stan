functions {
  real sgt_lpdf(real x, real mu, real s, real l, real p, real q) {
    // Skewed generalised t
    int N;
    real lz1;
    real lz2;
    real v;
    real m;
    real r;
    real out;
    //N = dims(x)[1];
    lz1 = lbeta(1.0/p,q);
    lz2 = lbeta(2.0/p,q-1.0/p);
    v = q^(-1.0/p)*((3*l^2+1)*exp(lbeta(3.0/p,q-2.0/p)-lz1)-4*l^2*exp(lz2-lz1)^2)^(-0.5);
    m = 2*v*s*l*q^(1.0/p)*exp(lz2-lz1);
    out = 0;
    
    //for (n in 1:N) {
      r = x-mu+m;
      if (r<0)
      	     out = out+log(p)-log(2*v*s*q^(1.0/p)*exp(lz1)*(fabs(r)^p /(q*(v*s)^p*(l*(-1)+1)^p)+1)^(1.0/p+q));
      else
      	     out = out+log(p)-log(2*v*s*q^(1.0/p)*exp(lz1)*(fabs(r)^p /(q*(v*s)^p*(l*(1)+1)^p)+1)^(1.0/p+q));
    //}
    return out;
  }
}
data {
  int<lower=0> N;
  int<lower=0> K;
  matrix[N, K] X;
  vector[N] y;
}
transformed data {
  matrix[N, K] Q_ast;
  matrix[K, K] R_ast;
  matrix[K, K] R_ast_inverse;
  // thin and scale the QR decomposition
  Q_ast = qr_Q(X)[, 1:K] * sqrt(N - 1);
  R_ast = qr_R(X)[1:K, ] / sqrt(N - 1);
  R_ast_inverse = inverse(R_ast);
}
parameters {
  simplex[4] p;
  
  vector[K] theta;
  real<lower=0> sigma;
  real<lower=-0.99, upper=0> l; 
  real<lower=1.001> q; 
}
model {
  real ps[4];
  
  for (n in 1:N) {
    ps[1] = log(p[1]) + normal_lpdf(y[n] | 13.810, 0.0025);
    ps[2] = log(p[2]) + normal_lpdf(y[n] | 14.509, 0.0025);
    ps[3] = log(p[3]) + normal_lpdf(y[n] | 14.914, 0.0025);
    ps[4] = log(p[4]) + sgt_lpdf(y[n] | Q_ast[n]*theta, sigma, l, 2, q);
    target += log_sum_exp(ps);
  }
}
generated quantities {
  vector[K] beta;
  beta = R_ast_inverse * theta; // coefficients on x
}
