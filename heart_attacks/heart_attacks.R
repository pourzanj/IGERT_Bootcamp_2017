library(tidyverse)
library(rstan)
library(shinystan)

heart_attacks <- tribble(
~study,~control.deaths,~control.total,~treated.deaths,~treated.total,
1,3,39,3,38,
2,14,116,7,114,
3,11,93,5,69,
4,127,1520,102,1533,
5,27,365,28,355,
6,6,52,4,59,
7,152,939,98,945,
8,48,471,60,632,
9,37,282,25,278,
10,188,1921,138,1916,
11,52,583,64,873,
12,47,266,45,263,
13,16,293,9,291,
14,45,883,57,858,
15,31,147,25,154,
16,38,213,33,207,
17,12,122,28,251,
18,6,154,8,151,
19,3,134,6,174,
20,40,218,32,209,
21,43,364,27,391,
22,39,674,22,680
)

#plot posterior distribution with and without beta-blocker
#without beta-blockers
tibble(x = c(0,1)) %>%
  ggplot(aes(x)) +
  geom_histogram(aes(x), data = tibble(x = rbeta(100, 17, 138)), binwidth = 0.01) +
  stat_function(fun = function(theta) dbeta(theta, 17, 138), color = "red")

tibble(x = c(0,1)) %>%
  ggplot(aes(x)) +
  geom_histogram(aes(x), data = tibble(x = rbeta(100, 7+3, 35+107)), binwidth = 0.01) +
  stat_function(fun = function(theta) dbeta(theta, 7+3, 35+107), color = "red")

#estimate probability first group has higher percentage than second group
(rbeta(100, 17, 138) > rbeta(100, 7+3, 35+107)) %>% mean

#run model in Stan
dat <- list(N = 3)
stan_fit <- stan(file = "heart_attacks.stan", data = dat)

#examine posterior histogram and summary
stan_fit
stan_fit %>% extract %>% .$theta %>% qplot

#now try hierarchical model
dat <- list(J = nrow(heart_attacks), y = pull(heart_attacks, control.deaths), N = pull(heart_attacks, control.total))
hierarchical_fit <- stan(file = "heart_attacks_hierarchical.stan", data = dat)

hierarchical_fit
launch_shinystan(hierarchical_fit)
rbeta(22,4.591733,43.09560) %>% qplot

#now compare posteriors of controlled and treated
dat <- list(J = nrow(heart_attacks), y = pull(heart_attacks, treated.deaths), N = pull(heart_attacks, treated.total))
hierarchical_treated_fit <- stan(file = "heart_attacks_hierarchical.stan", data = dat)

control <- hierarchical_fit %>% extract(pars = c("alpha", "beta")) %>% as_tibble %>% mutate(control = "control")
treated <- hierarchical_treated_fit %>% extract(pars = c("alpha", "beta")) %>% as_tibble %>% mutate(control = "treated")

bind_rows(control, treated) %>%
  mutate(draw = rbeta(8000, alpha, beta)) %>%
  ggplot(aes(draw, fill = control)) +
  geom_density(alpha = 0.2)
