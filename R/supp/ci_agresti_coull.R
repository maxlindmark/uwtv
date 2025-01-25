# Calculate CI with the Agresti-Coull method:
# Brown, L. D., Cai, T. T., & DasGupta, A. (2001). Interval estimation for a binomial proportion. Statistical science, 16(2), 101-133.

library(DescTools) # Agresti-Coull method in here
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggsidekick); theme_set(theme_sleek())

# Function to generate data and calculate statistics
simulate_hauls <- function(n_hauls, n_litter_hauls, area = 0.000148, conf.level = 0.95) {
  litter <- rep(0, n_hauls)
  litter[sample(n_hauls, n_litter_hauls)] <- 1
  mean_per_km2 <- mean(litter) / area
  
  ci <- BinomCI(sum(litter), n_hauls, conf.level = conf.level, 
                method = "agresti-coull")
  
  ci_per_km2 <- ci[,1:3] / area  # Tar alla tre värden (estimate, lower, upper)
  
  return(list(mean_per_km2 = mean_per_km2, 
              lower_ci_per_km2 = ci_per_km2[2], 
              upper_ci_per_km2 = ci_per_km2[3]))
}

# Parameters
haul_numbers <- c(50:800)
litter_hauls <- c(1:4)

# Run sims
results <- expand_grid(n_hauls = haul_numbers, n_litter = litter_hauls) %>%
  mutate(sim_results = map2(n_hauls, n_litter, ~simulate_hauls(.x, .y))) %>%
  unnest_wider(sim_results)

# Plot results
ggplot(results) +
  geom_ribbon(aes(n_hauls, mean_per_km2, ymin = lower_ci_per_km2, ymax = upper_ci_per_km2, group = 1),
              alpha = 0.3) +
  geom_line(aes(n_hauls, mean_per_km2, group = 1)) +
  theme(axis.title.y = element_markdown(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
  coord_cartesian(expand = 0) +
  labs(x = "Sample size (number of hauls)", 
       y = "Litter per km<sup>2</sup>") +
  facet_wrap(~n_litter, ncol = 2,
             labeller = labeller(n_litter = function(x) paste(" Hauls with litter: ",x ))) #+
  #scale_x_continuous(breaks = c(50, 100, 1000))

ggsave(paste0(home, "/figures/supp/agresti_coul.pdf"), width = 18, height = 13, units = "cm")


