
library(tidyverse)
source("scripts/simulation_functions.r")

# with defaults
simulate() %>% plot_evals()

# with maxent (SLOW)
simulate(sdm_method = "maxent") %>% plot_evals()

# with alternative params
simulate(n_dims = 3, 
         n_species = 500,
         training_filter = function(x) which(x[,1] > 0)) %>%
  plot_evals()

# multiple simulations (each with different climate covariance structures)
rep_simulations(n_sims = 5, n_dims = 3) %>%
  plot_evals()
rep_simulations(n_sims = 8, n_dims = 3, sdm_method = "maxent") %>%
  plot_evals()

# an example where best metric differs by test
d <- rep_simulations(n_sims = 30, alpha_shape1 = 20, alpha_shape2 = 20)
d %>%
  group_by(niche_metric, eval_stat, eval) %>%
  summarize(mean = mean(value),
            se = sd(value) / sqrt(length(value))) %>%
  ggplot(aes(niche_metric, mean, ymin = mean - se, ymax = mean + se)) +
  facet_grid(eval_stat ~ eval, scales = "free") +
  geom_pointrange()

