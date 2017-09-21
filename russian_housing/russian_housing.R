library(tidyverse)
library(rstan)
library(sgt)

#load data set
load(file = "houses.Rmd")

#work with just nekrasovka for now
bibirevo <- houses %>% filter(sub_area == "Bibirevo")

#plot price vs square feet
qplot(full_sq, log_price, data = bibirevo) + geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ I(x^2))

#fit linear model in stan
dat <- list(N = nrow(bibirevo),
            K = 3,
            X = model.matrix(log_price ~ full_sq + I(full_sq^2), bibirevo),
            y = pull(bibirevo, log_price))

fit_normal <- stan(file = "russian_housing.stan", data = dat)

#take one sample and draw posterior predictive
beta_samples <- fit_normal %>% extract(pars = "beta") %>% .$beta

predicted_means <- model.matrix(log_price ~ full_sq + I(full_sq^2), bibirevo) %*% beta_samples[100,]

bibirevo %>%
  mutate(log_price_sample = predicted_means + rnorm(length(predicted_means), sd = 0.54)) %>%
  ggplot() +
  geom_point(aes(full_sq, log_price)) +
  geom_point(aes(full_sq, log_price_sample), color = "red")

#try sgt distirubtion
fit_sgt <- stan(file = "russian_housing_sgt.stan", data = dat, chains = 1)

beta_samples <- fit_sgt %>% extract(pars = "beta") %>% .$beta

predicted_means <- model.matrix(log_price ~ full_sq + I(full_sq^2), bibirevo) %*% beta_samples[100,]

bibirevo %>%
  mutate(log_price_sample = predicted_means + rsgt(length(predicted_means), mu = 0, sigma = 2.0, lambda = -0.71, p = 2, q = 1.01)) %>%
  ggplot() +
  geom_point(aes(full_sq, log_price)) +
  geom_point(aes(full_sq, log_price_sample), color = "red")

#try mixture plus sgt
fit_mixture <- stan(file = "russian_housing_mixture.stan", data = dat, chains = 1)

beta_samples <- fit_mixture %>% extract(pars = "beta") %>% .$beta

predicted_means <- model.matrix(log_price ~ full_sq + I(full_sq^2), bibirevo) %*% beta_samples[100,]

bibirevo %>%
  mutate(log_price_sample = predicted_means + rsgt(length(predicted_means), mu = 0, sigma = 0.55, lambda = -0.4, p = 2, q = 1.03)) %>%
  mutate(mode_sample = apply(rmultinom(length(predicted_means), size = 1, prob = c(0.04, 0.06, 0.02, 0.88)), 2, function(x) which(x == 1))) %>%
  mutate(log_price_sample = ifelse(mode_sample == 1, 13.810,
                                   ifelse(mode_sample == 2, 14.509,
                                          ifelse(mode_sample == 3, 14.914, log_price_sample)))) %>%
  ggplot() +
  geom_point(aes(full_sq, log_price)) +
  geom_point(aes(full_sq, log_price_sample), color = "red")
