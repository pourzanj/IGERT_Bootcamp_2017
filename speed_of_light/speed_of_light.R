library(tidyverse)
library(rstan)
library(gridExtra)

#load data in to tibble
speed_of_light <- tibble(y = c(28,26,33,24,34,-44,27,16,40,-2,
                         29,22,24,21,25,30,23,29,31,19,
                         24,20,36,32,36,28,25,21,28,29,
                         37,25,28,26,30,32,36,26,30,22,
                         36,23,27,27,28,27,31,27,26,33,
                         26,32,32,24,39,28,24,25,32,25,
                         29,27,28,29,16,23))

#plot histogram
speed_of_light_histogram <- speed_of_light %>%
  ggplot(aes(y)) + geom_histogram()

#fit normal model in stan
dat <- list(N = nrow(speed_of_light), y = pull(speed_of_light, y))
sol_normal_fit <- stan(file = "speed_of_light_normal.stan", data = dat)

#check posterior fits
sol_normal_fit

#extract 100 samples of the posterior of mu and sigma
sol_normal_fit_posterior_samples <- sol_normal_fit %>% extract(pars = c("mu", "sigma")) %>% as_tibble %>% sample_n(100) %>% mutate(sample_num = row_number())

#plot what these look like
posterior_normal_distributions <- sol_normal_fit_posterior_samples %>%
  pmap(function(mu,sigma,sample_num) tibble(x = seq(-40,60,by=1)) %>% mutate(f = dnorm(x,mu,sigma)) %>% mutate(sample_num = sample_num)) %>%
  bind_rows()

ggplot() +
  geom_histogram(aes(x=y,y=..density..), data = speed_of_light) +
  geom_line(aes(x,f,group = sample_num), color = "red", alpha = 0.1, data = posterior_normal_distributions)
  
#extract posterior predictice draws for 9 draws
posterior_predictice_draws <- sol_normal_fit_posterior_samples %>%
  sample_n(16) %>%
  mutate(sample_num = row_number()) %>%
  pmap(function(mu,sigma,sample_num) tibble(y = rnorm(66,mu,sigma), sample_num = sample_num)) %>%
  bind_rows()
  
posterior_predictice_draws_plot <- posterior_predictice_draws %>%
  ggplot(aes(y)) +
  geom_histogram() +
  facet_wrap( ~ sample_num)

grid.arrange(speed_of_light_histogram, posterior_predictice_draws_plot, nrow = 2)

#show different with student t distribution
tibble(normal = rnorm(66,26.20,10.86), student = rt(66,1)*3.05+27.28) %>%
  gather(distribution, sample) %>%
  ggplot(aes(x=sample)) +
  geom_histogram() +
  facet_grid(distribution ~ .)

#fit student t model
sol_student_fit <- stan(file = "speed_of_light_student.stan", data = dat)

sol_student_fit

sol_student_fit_posterior_samples <- sol_student_fit %>% extract(pars = c("mu", "sigma")) %>% as_tibble %>% sample_n(100) %>% mutate(sample_num = row_number())

posterior_predictice_draws <- sol_student_fit_posterior_samples %>%
  sample_n(16) %>%
  mutate(sample_num = row_number()) %>%
  pmap(function(mu,sigma,sample_num) tibble(y = rt(66,1)*sigma+mu, sample_num = sample_num)) %>%
  bind_rows()

posterior_predictice_draws_plot <- posterior_predictice_draws %>%
  ggplot(aes(y)) +
  geom_histogram() +
  facet_wrap( ~ sample_num) +
  xlim(-40,60)

grid.arrange(speed_of_light_histogram, posterior_predictice_draws_plot, nrow = 2)
