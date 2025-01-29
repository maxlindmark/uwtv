library(tidyr)
library(tibble)
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
library(parallel)
library(ggtext)
library(ggsidekick)
library(egg)
theme_set(theme_sleek())

home <- here::here()

pal <- pal_brewer(palette = "Accent")(8)

## Simulate

# Function to assign litter to cells
get_km2 <- function(mean_per_km2) {
  cells <- matrix(0, nrow = 1000, ncol = 1000) # 1000x1000 meter
  row_pos <- sample(1:1000, mean_per_km2, replace = TRUE)
  col_pos <- sample(1:1000, mean_per_km2, replace = TRUE)
  for (i in 1:length(row_pos)) {
    cells[row_pos[i], col_pos[i]] <- 1
  }
  return(cells)
}

vis_sim <- list()

set.seed(99)

for(i in c(100, 500, 1000, 5000)){
  
  t <- get_km2(i) |> 
    as_tibble() |>
    rownames_to_column("Var1") |>
    pivot_longer(-Var1, names_to = "Var2", values_to = "value") |>
    mutate(Var2 = str_remove(Var2, "V")) |> 
    mutate(X = as.numeric(Var1), 
           Y = as.numeric(Var2))
  
  vis_sim[[i]] <- t |> mutate(true_density = i)
  
}

plot_dim <- 200/4
sample_size <- 148/4

# Add hypothetical transect
start_row <- sample(1:plot_dim, 1)
start_col <- sample(1:(plot_dim - sample_size + 1), 1)

sims <- vis_sim |>
  bind_rows() |> 
  mutate(transect = ifelse(X == start_row & Y %in% c(start_col:(start_col + sample_size - 1)),
                           "sampled", "not sampled"))

simsp <- sims |> 
  filter(X <= plot_dim & Y <= plot_dim) |> 
  mutate(true_density = paste0("True density=", true_density, "/km\u00B2"))

ggplot(simsp) +
  scale_color_manual(values = c("white", pal[6])) +
  scale_fill_manual(values = c("white", "black")) + 
  geom_tile(aes(X, Y, fill = factor(value)), color = NA) +
  geom_tile(data = simsp |> filter(transect == "sampled"),
            aes(X, Y), fill = pal[6], color = NA, alpha = 0.2) + 
  #facet_wrap(~true_density) + 
  facet_wrap(~factor(true_density,
                     levels = c("True density=100/km²", "True density=500/km²",
                                "True density=1000/km²", "True density=5000/km²"))) +
  labs(x = "X (m)", y = "Y (m)", fill = "Litter", color = "") + 
  theme(aspect.ratio = 1)

ggsave(paste0(home, "/figures/sample_map.pdf"), width = 19, height = 17, units = "cm")

# Now do the sampling
# Function to take samples that are 1x148 meter transects, horizontally or vertically
take_samples <- function(km2, no_samples, sample_size = 148) {
  replicate(no_samples, {
    # Slumpmässigt välja om remsan ska vara horizontal (TRUE) eller vertikal (FALSE)
    horizontal <- sample(c(TRUE, FALSE), 1)
    
    if (horizontal) {
      # horizontal transect
      start_row <- sample(1:1000, 1)
      start_col <- sample(1:(1000 - sample_size + 1), 1)
      sum(km2[start_row, start_col:(start_col + sample_size - 1)])
    } else {
      # vertical transect
      start_row <- sample(1:(1000 - sample_size + 1), 1)
      start_col <- sample(1:1000, 1)
      sum(km2[start_row:(start_row + sample_size - 1), start_col])
    }
  })
}

# Function to simulate
do_sims <- function(mean_per_km2, no_samples_list, no_sims) {
  km2 <- get_km2(mean_per_km2)
  
  results <- lapply(no_samples_list, function(no_samples) {
    sims <- replicate(no_sims, {
      samples <- take_samples(km2, no_samples)
      mean(samples) / 148 * 1000000 # convert to litter per km2
    })
    
    data.frame(
      no_samples = no_samples,
      est_mean = sims
    )
  })
  
  do.call(rbind, results)
}

# Function for parallel processing
parallell_sim <- function(mean, no_samples_list, no_sims) {
  results <- do_sims(mean, no_samples_list, no_sims)
  results$true_mean <- mean
  return(results)
}

# Definiera parametrar
means <- c(10, 50, 100, 200, 1000, 20000) # Skräptätheter
no_samples_list <- c(50, 100, 200, 500, 1000) # Olika sample_sizear
no_sims <- 1000 # Antal sims per sample_size

# Starta kluster
cl <- makeCluster(detectCores() - 1) # Använd alla kärnor utom en

# Export functions to cluster
clusterExport(cl, c(
  "get_km2", "take_samples", "do_sims", "parallell_sim",
  "no_samples_list", "no_sims"
))

# Do sims for multiple means in paralell
set.seed(124) # För reproducerbarhet
all_results <- parLapply(cl, means, function(mean) {
  parallell_sim(mean, no_samples_list, no_sims)
})

# Close cluster
stopCluster(cl)

# Gather results
results_df <- do.call(rbind, all_results)

results_df$no_samples <- as.numeric(results_df$no_samples)

sub <- results_df |> 
  group_by(true_mean, no_samples) |> 
  sample_n(30)

mean <- results_df |> 
  summarise(mean = mean(est_mean), .by = c(no_samples, true_mean))

# Proportion empty hauls:
text <- results_df |> 
  ungroup() |> 
  summarise(
    total_count = n(),
    zero_count = sum(est_mean == 0, na.rm = TRUE),
    zero_proportion = zero_count / total_count,
    zero_proportion = zero_count / total_count,
    .by = c(no_samples, true_mean)
  )


p1 <- ggplot(results_df, aes(x = factor(no_samples), y = est_mean)) +
  facet_wrap(~true_mean,
             scales = "free_y",
             labeller = labeller(true_mean = function(x) paste0("Litter density=", x, "/km\u00B2"))
             ) +
  geom_jitter(data = sub, height = 2, width = 0.1, alpha = 0.5, color = pal[5], size = 0.9) +
  geom_boxplot(width = 0.3, fill = NA, color = pal[5], outliers = FALSE) + 
  geom_point(data = mean, aes(y = mean), shape = 20, color = pal[6], size = 2.5) + 
  geom_hline(aes(yintercept = true_mean), linetype = "dashed", color = pal[6]) +
  geom_text(data = text, aes(x = factor(no_samples),
                             y = Inf,
                             vjust = 3.75,
                             label = round(zero_proportion, digits = 2)),
            color = "grey20", size = 3.3, position = position_dodge(width = 0.85), fontface = 1
            ) +
  labs(
    x = "Number of hauls",
    y = "Litter density (km<sup>2</sup>)"
    ) + 
  theme(axis.title.y = element_markdown())

tag_facet(p1, fontface = 1, open = "", close = "", tag_pool = LETTERS)

ggsave(paste0(home, "/figures/sims.pdf"), width = 19, height = 14, units = "cm")

