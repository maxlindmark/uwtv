library(tidyr)
library(dplyr)
library(ggplot2)
library(ggsidekick)
theme_set(theme_sleek())

home <- here::here()

## Rule of 3 
rule <- tibble(n = 50:500) |> 
  mutate(`Upper 95% CI` = 3/n,
         `Upper 99% CI` = 4.61/n) |> 
  pivot_longer(c("Upper 95% CI", "Upper 99% CI"))

label_points <- rule |> 
  filter(n == 87)

ggplot(rule, aes(n, value, color = name)) +
  geom_vline(xintercept = 88, linetype = 2, alpha = 0.4) +
  geom_line() +
  geom_label(data = label_points, aes(y = value, label = round(value, digits = 2), color = name), 
             size = 3, position = position_nudge(x = 0)) +
  #scale_color_manual(values = c("#D55E00", "#0072B2")) +
  scale_color_brewer(palette = "Dark2") +
  coord_cartesian(expand = 0) + 
  labs(y = "Upper CI for litter presence",
       x = "Sample size") + 
  theme(legend.title = element_blank(),
        legend.position.inside = c(0.8, 0.8),
        plot.margin = unit(c(0.2, 0.5, 0.2, 0.2), "cm")) +
  guides(color = guide_legend(position = "inside")) +
  NULL

ggsave(paste0(home, "/figures/rule_of_three.pdf"), width = 12, height = 12, units = "cm")
