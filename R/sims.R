library(tidyr)
library(tibble)
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
library(parallel)

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
  theme_bw() + 
  scale_color_manual(values = c("#D55E00", "#0072B2")) +
  coord_cartesian(expand = 0) + 
  labs(y = "Upper CI for litter presence",
       x = "Sample size") + 
  theme(legend.title = element_blank(),
        legend.position.inside = c(0.8, 0.8)) +
  guides(color = guide_legend(position = "inside")) +
  NULL

ggsave(paste0(home, "/figures/rule_of_three.pdf"), width = 14, height = 14, units = "cm")


## Simulering

# Funktion för att skapa en kvadratkilometer med skräp
skapa_km2 <- function(medel_per_km2) {
  rutor <- matrix(0, nrow = 1000, ncol = 1000) # 1000x1000 meter
  rad_pos <- sample(1:1000, medel_per_km2, replace = TRUE)
  kol_pos <- sample(1:1000, medel_per_km2, replace = TRUE)
  for (i in 1:length(rad_pos)) {
    rutor[rad_pos[i], kol_pos[i]] <- 1
  }
  return(rutor)
}

vis_sim <- list()

set.seed(99)

for(i in c(100, 500, 1000, 5000)){
  
  t <- skapa_km2(i) |> 
    as_tibble() |>
    rownames_to_column("Var1") |>
    pivot_longer(-Var1, names_to = "Var2", values_to = "value") |>
    mutate(Var2 = str_remove(Var2, "V")) |> 
    mutate(X = as.numeric(Var1), 
           Y = as.numeric(Var2))
  
  vis_sim[[i]] <- t |> mutate(true_density = i)
  
}

plot_dim <- 200/2
provstorlek <- 184/2

# Add hypothetical transect
start_rad <- sample(1:plot_dim, 1)
start_kol <- sample(1:(plot_dim - provstorlek + 1), 1)

sims <- vis_sim |>
  bind_rows() |> 
  mutate(transect = ifelse(X == start_rad & Y %in% c(start_kol:(start_kol + provstorlek - 1)),
                           "sampled", "not sampled"))

sims |> 
  filter(X <= plot_dim & Y <= plot_dim) |> 
  mutate(true_density = paste0("True density=", true_density)) |> 
  ggplot() +
  scale_color_manual(values = c("white", "#0072B2")) +
  geom_tile(aes(X, Y, fill = factor(value)), color = NA) +
  geom_tile(aes(X, Y, color = factor(transect)),  fill = NA) + 
  facet_wrap(~factor(true_density,
                     levels = c("True density=100", "True density=500",
                                "True density=1000", "True density=5000"))) + 
  labs(x = "X (m)", y = "Y (m)", fill = "Litter", color = "") + 
  scale_fill_manual(values = c("white", "black")) + 
  theme_bw(base_size = 12) +
  theme(aspect.ratio = 1) +
  coord_cartesian(expand = 0)

ggsave(paste0(home, "/figures/sample_map.pdf"), width = 17, height = 17, units = "cm")

# Now do the sampling
# skapar 1000x1000 meter tar horisontellt och vertikalt

# Funktion för att ta prover som 1x184 meter remsor, antingen horisontellt eller vertikalt
ta_prover <- function(km2, antal_prover, provstorlek = 184) {
  replicate(antal_prover, {
    # Slumpmässigt välja om remsan ska vara horisontell (TRUE) eller vertikal (FALSE)
    horisontell <- sample(c(TRUE, FALSE), 1)
    
    if (horisontell) {
      # Horisontell remsa
      start_rad <- sample(1:1000, 1)
      start_kol <- sample(1:(1000 - provstorlek + 1), 1)
      sum(km2[start_rad, start_kol:(start_kol + provstorlek - 1)])
    } else {
      # Vertikal remsa
      start_rad <- sample(1:(1000 - provstorlek + 1), 1)
      start_kol <- sample(1:1000, 1)
      sum(km2[start_rad:(start_rad + provstorlek - 1), start_kol])
    }
  })
}

# Funktion för att utföra simuleringar
utfor_simuleringar <- function(medel_per_km2, antal_prover_lista, antal_simuleringar) {
  km2 <- skapa_km2(medel_per_km2)
  
  resultat <- lapply(antal_prover_lista, function(antal_prover) {
    simuleringar <- replicate(antal_simuleringar, {
      prover <- ta_prover(km2, antal_prover)
      mean(prover) / 184 * 1000000 # Omvandla till skräp per km²
    })
    
    data.frame(
      antal_prover = antal_prover,
      uppskattat_medelvarde = simuleringar
    )
  })
  
  do.call(rbind, resultat)
}

# Funktion för parallell bearbetning
parallell_simulering <- function(medel, antal_prover_lista, antal_simuleringar) {
  resultat <- utfor_simuleringar(medel, antal_prover_lista, antal_simuleringar)
  resultat$faktiskt_medelvarde <- medel
  return(resultat)
}

# Definiera parametrar
medelvarden <- c(10, 50, 100, 200, 1000, 20000) # Skräptätheter
antal_prover_lista <- c(50, 100, 200, 500, 1000) # Olika provstorlekar
antal_simuleringar <- 1000 # Antal simuleringar per provstorlek

# Starta kluster
cl <- makeCluster(detectCores() - 1) # Använd alla kärnor utom en

# Exportera nödvändiga funktioner och variabler till klustret
clusterExport(cl, c(
  "skapa_km2", "ta_prover", "utfor_simuleringar", "parallell_simulering",
  "antal_prover_lista", "antal_simuleringar"
))

# Utför simuleringar för flera medelvärden parallellt
set.seed(124) # För reproducerbarhet
alla_resultat <- parLapply(cl, medelvarden, function(medel) {
  parallell_simulering(medel, antal_prover_lista, antal_simuleringar)
})

# Stäng klustret
stopCluster(cl)

# Sammanställ alla resultat
resultat_df <- do.call(rbind, alla_resultat)

# Säkerställ att antal_prover är numerisk
resultat_df$antal_prover <- as.numeric(resultat_df$antal_prover)

# Violinplot av medelvärden

# obs enligt ovan så kan faktiskt inte alla medelvärden hittas om sample size är för liten
# bara de fall då densitet*total area >1 så plottas det
# hm vet inte om det är sant egentligen. om skräpet är random så kan jag väl få flera i min survey även om det är osannolikt?
# Beräkna förväntat antal objekt
# resultat_df <- resultat_df %>%
#   mutate(forvantade_objekt = faktiskt_medelvarde * antal_prover * 0.000148)
#
# # Filtrera datan för violinplots
# violin_data <- resultat_df %>%
#   filter(forvantade_objekt >= 1)
# violin_data <- resultat_df

# windows()
cols <- c("#56B4E9", "#009E73", "#F0E442", "#0072B2", "#E69F00", "#D55E00") # för färgblinda aqua report

sub <- resultat_df |> 
  group_by(faktiskt_medelvarde, antal_prover) |> 
  sample_n(100)

mean <- resultat_df |> 
  group_by(faktiskt_medelvarde, antal_prover) |> 
  summarise(mean = mean(uppskattat_medelvarde))

# Proportion empty hauls:
text <- resultat_df |> 
  ungroup() |> 
  summarise(
    total_count = n(),
    zero_count = sum(uppskattat_medelvarde == 0, na.rm = TRUE),
    zero_proportion = zero_count / total_count,
    zero_proportion = zero_count / total_count,
    .by = c(antal_prover, faktiskt_medelvarde)
  )

ggplot(resultat_df, aes(x = factor(antal_prover), y = uppskattat_medelvarde)) +
  geom_jitter(data = sub, height = 3, width = 0.2, alpha = 0.5, color = "#0072B2", size = 1) +
  geom_boxplot(width = 0.2, alpha = 0.2, fill = "#0072B2", color = "#0072B2", outliers = FALSE) + 
  geom_point(data = mean, aes(y = mean), shape = 21, color = "#D55E00", size = 1.4, stroke = 1.4) + 
  geom_hline(aes(yintercept = faktiskt_medelvarde), linetype = "dashed", color = "#D55E00") +
  facet_wrap(~faktiskt_medelvarde,
             scales = "free_y",
             labeller = labeller(faktiskt_medelvarde = function(x) paste("Mean litter pr km\u00B2:", x))
  ) +
  scale_fill_manual(values = cols) + # lägger in cols
  scale_y_continuous(
    labels = scales::comma_format(),
    limits = c(0, NA), # Sätter en nedre gräns på 0 för y-axeln
    expand = expansion(mult = c(0, 0.05)) # Liten marginal ovanpå max-värden
  ) +
  geom_text(data = text, aes(x = factor(antal_prover),
                             y = Inf,
                             vjust = 1.5,
                             label = round(zero_proportion, digits = 2)),
            color = "grey20", size = 4, position = position_dodge(width = 0.85), fontface = 2
  ) +
  guides(fill = "none") + # Tar bort legenden för färgerna
  labs(
    x = "Sample size (number of hauls)",
    y = "Litter per km²",
    #title = "Simulated mean values",
    #subtitle = "Random dist. of litter (1000 simulations)"
  ) +
  theme_bw(base_size = 10) +
  theme(
    strip.background = element_rect(fill = "lightgray"),
    strip.text = element_text()
  )

ggsave(paste0(home, "/figures/sims.pdf"), width = 23, height = 19, units = "cm")
