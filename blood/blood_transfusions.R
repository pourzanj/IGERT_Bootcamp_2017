#load data
load(file = "data/blood.Rdata")

#plot histogram
blood_histogram <- qplot(blood_units, data = blood, binwidth = 1)

#fit poisson model
dat <- list(N = nrow(blood), y = pull(blood, blood_units))
blood_poisson_fit <- stan(file = "blood_poisson.stan", data = dat)

#extract samples and plot posterior predictive
posterior_predictice_draws <- blood_poisson_fit %>%
  extract(pars = c("lambda")) %>%
  as_tibble %>%
  sample_n(16) %>%
  mutate(sample_num = row_number()) %>%
  pmap(function(lambda,sample_num) tibble(y = rpois(244, lambda), sample_num = sample_num)) %>%
  bind_rows()

posterior_predictice_draws_plot <- posterior_predictice_draws %>%
  ggplot(aes(y)) +
  geom_histogram() +
  facet_wrap( ~ sample_num)

grid.arrange(blood_histogram, posterior_predictice_draws_plot, nrow = 2)

#there are way too many zeros and we never see high blood unit counts
#let's fix the zeros first with a zero_inflated poisson
blood_zero_poisson_fit <- stan(file = "blood_zero_inflated_poisson.stan", data = dat)

draw_zero_inf_pois <- function(N, theta, lambda) rbernoulli(N,theta)*rpois(N,lambda)

posterior_predictice_draws <- blood_zero_poisson_fit %>%
  extract(pars = c("theta","lambda")) %>%
  as_tibble %>%
  sample_n(16) %>%
  mutate(sample_num = row_number()) %>%
  pmap(function(theta,lambda,sample_num) tibble(y = draw_zero_inf_pois(244, theta, lambda), sample_num = sample_num)) %>%
  bind_rows()

posterior_predictice_draws_plot <- posterior_predictice_draws %>%
  ggplot(aes(y)) +
  geom_histogram() +
  facet_wrap( ~ sample_num)

grid.arrange(blood_histogram, posterior_predictice_draws_plot, nrow = 2)

#show difference in disperseion of negative binomial and poisson
draw_zero_inf_nbinom <- function(N, theta, size, p) rbernoulli(N,theta)*rnbinom(N,size,p)

tibble(poisson = rnorm(66,10.36), negbinom = rnbinom(66)) %>%
  gather(distribution, sample) %>%
  ggplot(aes(x=sample)) +
  geom_histogram() +
  facet_grid(distribution ~ .)

#fit negbinom model
blood_zero_negbinom_fit <- stan(file = "blood_zero_inflated_negbinom.stan", data = dat)

posterior_predictice_draws <- blood_zero_negbinom_fit %>%
  extract(pars = c("theta","alpha","beta")) %>%
  as_tibble %>%
  sample_n(16) %>%
  mutate(sample_num = row_number()) %>%
  mutate(size = alpha, p = beta/(beta+1)) %>%
  select(-alpha, -beta) %>%
  pmap(function(theta,size,p,sample_num) tibble(y = draw_zero_inf_nbinom(244, theta, size, p), sample_num = sample_num)) %>%
  bind_rows()

posterior_predictice_draws_plot <- posterior_predictice_draws %>%
  ggplot(aes(y)) +
  geom_histogram() +
  facet_wrap( ~ sample_num)

grid.arrange(blood_histogram, posterior_predictice_draws_plot, nrow = 2)
