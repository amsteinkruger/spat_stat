# Sketch points, contours, polygons, and trends over time.

library(tidyverse)
library(terra)
library(tidyterra)
library(patchwork)

# Set seed.

set.seed(0112358)

# Set up data.

dat =
  tibble(x = rnorm(100) %>% abs,
         y = rnorm(100) %>% abs,
         fire = rbinom(100, 1, 0.25),
         fire_factor = fire %>% factor)

# Plot points.

vis = 
  dat %>% 
  ggplot() +
  geom_point(aes(x = x,
                 y = y,
                 color = fire_factor),
             shape = 21,
             fill = NA) +
  scale_color_manual(values = c("black", "#D73F09")) +
  theme_minimal() +
  theme(legend.position = "none")

# Export.

ggsave("out/vis_interpolation_1.png",
       vis,
       dpi = 300,
       width = 3.25,
       height = 3.25)

# Plot contours.

vis_contours = 
  dat %>% 
  ggplot() +
  geom_density_2d(aes(x = x,
                      y = y),
                  color = "black",
                  bins = 4) +
  geom_point(aes(x = x,
                 y = y,
                 color = fire_factor),
             shape = 21,
             fill = NA) +
  scale_color_manual(values = c("black", "#D73F09")) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("out/vis_interpolation_2.png",
       vis_contours,
       dpi = 300,
       width = 3.25,
       height = 3.25)

# Get polygons.

box = ext(c(0, 3, 0, 3))

dat_points = 
  dat %>% 
  vect(geom = c("x", "y"))

dat_polygons = 
  dat_points %>% 
  voronoi(box)

vis_polygons = 
  ggplot() +
  geom_spatvector(data = dat_points,
                  aes(color = fire_factor),
                  shape = 21,
                  fill = NA) +
  geom_spatvector(data = dat_polygons,
                  color = "black",
                  fill = NA) +
  scale_color_manual(values = c("black", "#D73F09")) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("out/vis_interpolation_3.png",
       vis_polygons,
       dpi = 300,
       width = 3.25,
       height = 3.25)

# Timeline

dat_time = 
  tibble(year = (10 - rnorm(100, 0, 5) %>% abs) %>% round(0),
         year_factor = year %>% factor,
         severity = rnorm(100, 0, 25) %>% abs) %>% 
  filter(year %in% 1:10)

vis_time = 
  dat_time %>% 
  ggplot() +
  geom_point(aes(x = year_factor,
                 y = severity),
             shape = 21) +
  labs(x = "Year", y = "Severity (0-100)") +
  scale_y_continuous(limits = c(0, 100)) +
  theme_minimal()

ggsave("out/vis_interpolation_4.png",
       vis_time,
       dpi = 300,
       width = 3.25,
       height = 3.25)

dat_time_less = 
  dat_time %>% 
  group_by(year, year_factor) %>% 
  summarize(severity_mean = mean(severity)) %>% 
  ungroup

vis_time_less = 
  dat_time_less %>% 
  ggplot() +
  geom_point(aes(x = year_factor,
                 y = severity_mean),
             shape = 21) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = "Year", y = "Mean Severity (0-100)") +
  theme_minimal()

ggsave("out/vis_interpolation_5.png",
       vis_time_less,
       dpi = 300,
       width = 3.25,
       height = 3.25)

vis_time_interpolate = 
  dat_time_less %>% 
  ggplot() +
  geom_line(aes(x = year_factor,
                y = severity_mean,
                group = 1)) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = "Year", y = "Mean Severity (0-100)") +
  theme_minimal()

ggsave("out/vis_interpolation_6.png",
       vis_time_interpolate,
       dpi = 300,
       width = 3.25,
       height = 3.25)

dat_time_smooth = 
  dat_time_less %>% 
  arrange(year) %>% 
  mutate(severity_mean_lead = lead(severity_mean),
         severity_mean_lag = lag(severity_mean),
         severity_mean_rolling = (severity_mean + severity_mean_lag + severity_mean_lead) / 3)

vis_time_smooth = 
  dat_time_smooth %>% 
  ggplot() +
  geom_line(aes(x = year_factor,
                y = severity_mean_rolling,
                group = 1)) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = "Year", y = "Mean Severity (0-100)") +
  theme_minimal()

ggsave("out/vis_interpolation_7.png",
       vis_time_smooth,
       dpi = 300,
       width = 3.25,
       height = 3.25)
