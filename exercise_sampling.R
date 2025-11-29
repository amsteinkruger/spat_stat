# Mock up sampling strategies: random, systematic non-aligned sampling, stratified, nested, proportional probability. 

library(tidyverse)

# Set seed.

set.seed(0112358)

# Random

#  Set up data.

dat =
  tibble(x = runif(100),
         y = runif(100))

#  Set up a plot.

vis = 
  dat %>% 
  ggplot() +
  geom_point(aes(x = x,
                 y = y),
             shape = 21,
             fill = NA) +
  scale_x_continuous(limits = c(0, 1),
                     breaks = seq(0, 1.0, by = 0.1)) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0, 1.0, by = 0.1)) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank())

#  Export.

ggsave("out/vis_sampling_1.png",
       vis,
       dpi = 300,
       width = 3.25,
       height = 3.25)

# Systematic Non-Aligned

#  Set up data.

dat = 
  tibble(x = seq(0.05, 0.95, by = 0.10),
         y = seq(0.05, 0.95, by = 0.10)) %>% 
  expand(x, y) %>% 
  mutate(x = x + runif(100, -0.05, 0.025),
         y = y + runif(100, -0.05, 0.025))
  
#  Set up a plot.

vis = 
  dat %>% 
  ggplot() +
  geom_point(aes(x = x,
                 y = y),
             shape = 21,
             fill = NA) +
  scale_x_continuous(limits = c(0, 1),
                     breaks = seq(0, 1.0, by = 0.1)) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0, 1.0, by = 0.1)) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank())

#  Export.

ggsave("out/vis_sampling_2.png",
       vis,
       dpi = 300,
       width = 3.25,
       height = 3.25)

# Stratified

#  Set up data.

dat =
  tibble(x = c(runif(75, 0, 0.50), runif(25, 0.50, 1.00)),
         y = runif(100))

#  Set up a plot.

vis = 
  dat %>% 
  ggplot() +
  geom_point(aes(x = x,
                 y = y),
             shape = 21,
             fill = NA) +
  geom_vline(xintercept = 0.50,
             color = "#D73F09",
             linetype = "dashed") +
  scale_x_continuous(limits = c(0, 1),
                     breaks = seq(0, 1.0, by = 0.1)) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0, 1.0, by = 0.1)) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank())

#  Export.

ggsave("out/vis_sampling_3.png",
       vis,
       dpi = 300,
       width = 3.25,
       height = 3.25)

# Nested

#  Set up data.

dat =
  tibble(x = c(runif(20, 0, 0.1), runif(80, 0, 1.0)),
         y = c(runif(20, 0, 0.1), runif(80, 0, 1.0))) %>% 
  mutate(color = ifelse(x <= 0.1 & y <= 0.1, "Orange", "Black"))

#  Set up a plot.

vis = 
  dat %>% 
  ggplot() +
  geom_point(aes(x = x,
                 y = y,
                 color = color),
             shape = 21,
             fill = NA) +
  scale_x_continuous(limits = c(0, 1),
                     breaks = seq(0, 1.0, by = 0.1)) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0, 1.0, by = 0.1)) +
  scale_color_manual(values = c("black", "#D73F09")) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        panel.grid.minor = element_blank())

#  Export.

ggsave("out/vis_sampling_4.png",
       vis,
       dpi = 300,
       width = 3.25,
       height = 3.25)
