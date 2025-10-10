# Mock up prices over properties (points and polygons), neighborhoods, and scales for two groups of homeowners.

library(tidyverse)
library(terra)
library(tidyterra)
library(patchwork)

# Set seed.

set.seed(0112358)

# Set up data.

dat = 
  tibble(id = seq(1, 100),
         group = c(rep("Older", 50), rep("Newer", 50)),
         x = c(rnorm(50) * 10, rnorm(50) * 5)  %>% abs,
         y = c(rnorm(50) * 10, rnorm(50) * 5) %>% abs,
         price = c(rnorm(50, 200, 100), rnorm(50, 400, 200)) %>% round(0) %>% abs) %>% 
  mutate(neighborhood = ifelse(x < 15 & y < 15, "Inner", "Outer")) %>% 
  vect(geom = c("x", "y"))

# Get a bounding box.

box = ext(c(0, 30, 0, 30))

# (a) Points

vis_a_group = 
  dat %>% 
  ggplot() +
  geom_spatvector(aes(fill = group),
                  shape = 21,
                  color = scales::pal_viridis(option = "F")(2)[1]) +
  labs(fill = "Owner Group") +
  scale_fill_manual(values = c(scales::pal_viridis(option = "F")(2)[2], scales::pal_viridis(option = "F")(2)[1])) +
  theme_minimal() +
  theme(legend.position = "bottom")

vis_a_price = 
  dat %>% 
  ggplot() +
  geom_spatvector(aes(fill = price),
                  shape = 21,
                  color = scales::pal_viridis(option = "F")(2)[1]) +
  labs(fill = "Price ($, 1e3)") +
  scale_fill_viridis_c(option = "F") +
  theme_minimal() +
  theme(legend.position = "bottom")

# (b) Polygons, Prices

vis_b_group = 
  dat %>% 
  voronoi(box) %>% 
  ggplot() +
  geom_spatvector(aes(fill = group),
                  color = scales::pal_viridis(option = "F")(2)[1]) +
  labs(fill = "Owner Group") +
  scale_fill_manual(values = c(scales::pal_viridis(option = "F")(2)[2], scales::pal_viridis(option = "F")(2)[1])) +
  theme_minimal() +
  theme(legend.position = "bottom")

vis_b_price = 
  dat %>% 
  voronoi(box) %>% 
  ggplot() +
  geom_spatvector(aes(fill = price),
                  color = scales::pal_viridis(option = "F")(2)[1]) +
  labs(fill = "Price ($, 1e3)") +
  scale_fill_viridis_c(option = "F") +
  theme_minimal() +
  theme(legend.position = "bottom")

# (c) Neighborhoods

vis_c_group = 
  dat %>% 
  voronoi(box) %>% 
  mutate(group_numeric = ifelse(group == "Older", 0, 1)) %>% 
  group_by(neighborhood) %>% 
  summarize(group = mean(group_numeric) * 100) %>% 
  ungroup %>% 
  ggplot() +
  geom_spatvector(aes(fill = group),
                  color = NA) +
  geom_spatvector(data = dat %>% voronoi(box),
                  color = scales::pal_viridis(option = "F")(2)[2],
                  fill = NA) +
  labs(fill = "% New Owners") +
  scale_fill_viridis_c(option = "F", limits = c(0, 100), breaks = c(0, 100)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.ticks = element_blank())

vis_c_price = 
  dat %>% 
  voronoi(box) %>% 
  mutate(group_numeric = ifelse(group == "Older", 0, 1)) %>% 
  group_by(neighborhood) %>% 
  summarize(group = price %>% mean) %>% 
  ungroup %>% 
  ggplot() +
  geom_spatvector(aes(fill = group),
                  color = NA) +
  geom_spatvector(data = dat %>% voronoi(box) %>% filter(neighborhood == "Outer"),
                  color = scales::pal_viridis(option = "F")(2)[2],
                  fill = NA) +
  geom_spatvector(data = dat %>% voronoi(box) %>% filter(neighborhood == "Inner"),
                  color = scales::pal_viridis(option = "F")(2)[1],
                  fill = NA) +
  labs(fill = "Mean Price ($, 1e3)") +
  scale_fill_viridis_c(option = "F") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.ticks = element_blank())

# (d) Scaling

# Just refer to change from (b) to (c).

# Exports

vis_a = vis_a_group + vis_a_price
vis_b = vis_b_group + vis_b_price
vis_c = vis_c_group + vis_c_price

ggsave("out/vis_a.png",
       vis_a,
       dpi = 300,
       width = 6.5,
       height = 3,
       bg = NULL)

ggsave("out/vis_b.png",
       vis_b,
       dpi = 300,
       width = 6.5,
       height = 3.25,
       bg = NULL)

ggsave("out/vis_c.png",
       vis_c,
       dpi = 300,
       width = 6.5,
       height = 3.50,
       bg = NULL)
