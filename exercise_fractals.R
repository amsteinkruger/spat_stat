# Draw three iterations of a fractal.

# Packages

library(tidyverse)
library(patchwork)
library(ggpubr)
library(alphahull)

# Plots

vis_koch_1 = 
  koch(side = 3, niter = 1) %>% 
  as.data.frame %>% 
  bind_rows(., slice_head(.)) %>% 
  ggplot() + 
  geom_path(aes(x = V1, 
                y = V2)) +
  theme_void()

vis_koch_2 = 
  koch(side = 3, niter = 2) %>% 
  as.data.frame %>% 
  bind_rows(., slice_head(.)) %>% 
  ggplot() + 
  geom_path(aes(x = V1, 
                y = V2)) +
  theme_void()

vis_koch_3 = 
  koch(side = 3, niter = 3) %>% 
  as.data.frame %>% 
  bind_rows(., slice_head(.)) %>% 
  ggplot() + 
  geom_path(aes(x = V1, 
                y = V2)) +
  theme_void()

vis_koch = vis_koch_1 + vis_koch_2 + vis_koch_3 + coord_fixed()

ggsave("out/vis_koch.png",
       vis_koch,
       dpi = 300,
       width = 6.5)

# And draw two waves out of phase by 90 degress while we're at it.

dat_wave_90 = 
  tibble(x = seq(0, 10, by = 0.01),
         x_pi = x * 2 * pi,
         x_pi_90 = x * 2 * pi - 1 / 2 * pi,
         y_1 = sin(x_pi),
         y_2 = sin(x_pi_90))

vis_wave_90 = 
  dat_wave_90 %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y_1)) +
  geom_line(aes(x = x,
                y = y_2),
            color = "#D73F09") +
  labs(y = expression("sin(2" * pi * "x)")) +
  scale_x_continuous(limits = c(0, 10),
                     breaks = c(0, 10)) +
  scale_y_continuous(limits = c(-2, 2),
                     breaks = c(-1, 0, 1)) +
  theme_pubr()

ggsave("out/vis_wave_90.png",
       vis_wave_90,
       dpi = 300,
       width = 3.25,
       height = 3.25)

# Finally, sketch a quick line for illustration.

# Set seed.

set.seed(0112358)

# Get a plot.

vis_acorns = 
  tibble(distance = seq(0, 2000, by = 100),
         y_1 = (750 - distance) / 1000,
         y_2 = runif(21, -0.25, 0.25)) %>% 
  mutate(y_3 = ifelse(row_number() > 10, y_2, y_1 + y_2)) %>% 
  ggplot() +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  geom_point(aes(x = distance,
                 y = y_3),
             shape = 21,
             fill = NA) +
  labs(x = "Distance (km)",
       y = "Mean Correlation") +
  scale_y_continuous(limits = c(-0.50, 1.00),
                     breaks = c(-0.50, 0, 0.50, 1.00)) +
  theme_pubr()

ggsave("out/vis_acorns.png",
       vis_acorns,
       dpi = 300,
       width = 3.25,
       height = 3.25)
