# Load libraries
library(tidyverse)
library(tidylog)
library(patchwork)
library(viridis)
library(RColorBrewer)
library(ggsidekick)
library(raster)
library(sdmTMB)
library(ggtext)
theme_set(theme_sleek())

home <- here::here()

source(paste0(home, "/R/functions/map-plot.R"))

# Read polygons

coast <- shapefile(paste0(here::here(), "/data/shapefiles/kust_konkav_polygon.shp")) |> 
  st_as_sf()

uwtv <- shapefile(paste0(here::here(), "/data/shapefiles/uv_tv_konkav_polygon_true_points.shp")) |> 
  st_as_sf()

uwtv_coast <- shapefile(paste0(here::here(), "/data/shapefiles/uvtv_kust_polygon.shp")) |> 
  st_as_sf()

plot(coast)
plot(uwtv)
plot(uwtv_coast)

pal <- brewer.pal(n = 3, name = "Dark2")

plot_map + 
  geom_sf(data = coast, color = pal[1], fill = pal[1], alpha = 0.15, linetype = 2) + 
  geom_sf(data = uwtv, color = pal[2], fill = pal[2], alpha = 0.15, linetype = 1) +
  geom_sf(data = uwtv_coast, color = pal[3], fill = pal[3], alpha = 0.15, linetype = 3)


d1 <- readRDS(paste0(home, "/data/ibts_points_in_uv_kust_polygon.rds")) # ibts
d2 <- readRDS(paste0(home, "/data/kust_points_in_uv_polygon_4326.rds")) # kust i UW
d3 <- readRDS(paste0(home, "/data/kust_points_outside_uv_polygon_4326.rds")) # kust utanför UW

d <- bind_rows(d1 |> mutate(Survey = "IBTS"),
               d2 |> mutate(Survey = "CTS in UW", Quarter = 3),
               d3 |> mutate(Survey = "CTS", Quarter = 3)) |> 
  add_utm_columns(ll_names = c("long_stop_dec", "lat_stop_dec")) |> 
  st_drop_geometry() |> 
  mutate(haul_id = paste(Year, Quarter, X, Y, Survey, sep = "_"),
         haul_id = as.factor(haul_id)) |> 
  ungroup()

# Summarise by haul
catch <- d |> 
  summarise(no_km2 = sum(no_litter_per_km2), .by = haul_id)

catch <- catch |> 
  left_join(d |>
              distinct(haul_id, .keep_all = TRUE) |> 
              dplyr::select(X, Y, lat_stop_dec, long_stop_dec, haul_id) |> 
              rename(lat = lat_stop_dec,
                     lon = long_stop_dec),
            by = "haul_id") |> 
  separate(haul_id, sep = "_", into = c("year", "quarter", "X", "Y", "survey"), remove = FALSE) |> 
  mutate(survey = as.factor(survey),
         year_f = as.factor(year),
         year = as.integer(year),
         X = as.integer(X),
         Y = as.integer(Y))

##  Plot data
d4 <- read_csv(paste0(home, "/data/Megafauna_stn_litter.csv")) |> 
  rename(lon = Lon, 
         lat = Lat) |> 
  add_utm_columns(ll_names = c("lon", "lat")) |> 
  mutate(survey = "UWTV",
         year = 2024)

pd <- bind_rows(catch, d4)

# Plot data
p1 <- pd |> 
  filter(!survey == "UWTV") |> 
  mutate(survey = ifelse(survey == "CTS", "CTS coastal", survey)) |>
  summarise(mean = mean(no_km2), .by = c(year, survey)) |> 
  rename(Survey = survey) |> 
  ggplot(aes(year, mean, color = Survey, shape = Survey)) + 
  geom_point() + 
  geom_line() + 
  scale_color_manual(values = c(pal[1], pal[1], pal[2])) +
  theme(legend.position.inside = c(0.81, 0.81),
        axis.title.y = element_markdown()) +
  labs(x = "Year", y = "Mean density (no/km<sup>2</sup>)") + 
  guides(color = guide_legend(position = "inside"),
         shape = guide_legend(position = "inside"))

p1

pd2 <- pd |> 
  mutate(survey = ifelse(survey == "CTS in UW", "CTS", survey))

p2 <- plot_map + 
  geom_sf(fill = "grey97", color = "grey70") +
  geom_sf(data = coast, color = pal[1], fill = pal[1], alpha = 0.1, linetype = 1, linewidth = 0.3) + 
  geom_sf(data = uwtv_coast, color = pal[2], fill = pal[2], alpha = 0.1, linetype = 1, linewidth = 0.3) +
  geom_sf(data = uwtv, color = pal[3], fill = pal[3], alpha = 0.1, linetype = "longdash", linewidth = 0.3) +
  geom_point(data = pd2,
             aes(X*1000, Y*1000, color = survey), size = 0.6) +
  #Annotate works well when not faceting... need to ggsave to make sure
  # annotate("text", label = "Sweden", color = "gray50", size = 5,
  #          x = Inf, y = Inf, hjust = 1.5, vjust = 20) +
  # annotate("text", label = "Norway", color = "gray50", size = 5,
  #          x = Inf, y = Inf, hjust = 3.2, vjust = 2) +
  # annotate("text", label = "Denmark", color = "gray50", size = 5,
  #          x = Inf, y = Inf, hjust = 5.3, vjust = 55) +
  #facet_wrap(~year, ncol = 5) + 
  theme(legend.position.inside = c(0.73, 0.9),
        legend.direction = "vertical") + 
  scale_color_brewer(palette = "Dark2", name = "Survey") +
  guides(color = guide_legend(position = "inside", 
                              override.aes = list(size = 2)))

(p1 + p2) + plot_annotation(tag_levels = "A") + plot_layout(widths = c(1.6, 1))

ggsave(paste0(home, paste("/figures/supp/map_supp.pdf")), width = 17, height = 10, units = "cm")

