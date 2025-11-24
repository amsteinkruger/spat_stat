# Illustrate spectral analysis.

library(tidyverse)
library(patchwork)

# Set seed.

set.seed(0112358)

# Illustrate waves.

# Wavelength 1, amplitude 1, 10 waves.

dat_wave_10 = 
  tibble(x = seq(0, 10, by = 0.01),
         x_pi = x * 2 * pi,
         y = sin(x_pi))

vis_wave_10 = 
  dat_wave_10 %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y)) +
  labs(y = expression("sin(2" * pi * "x)")) +
  scale_x_continuous(limits = c(0, 10),
                     breaks = c(0, 10)) +
  scale_y_continuous(limits = c(-2, 2),
                     breaks = c(-1, 0, 1)) +
  theme_pubr()

ggsave("out/vis_wave_10.png",
       vis_wave_10,
       dpi = 300,
       width = 3.25,
       height = 3.25)

# Wavelength 0.5, amplitude 1, 20 waves.

dat_wave_20 = 
  tibble(x = seq(0, 10, by = 0.01),
         x_pi = x * 4 * pi,
         y = sin(x_pi))

vis_wave_20 = 
  dat_wave_20 %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y)) +
  labs(y = expression("sin(4" * pi * "x)")) +
  scale_x_continuous(limits = c(0, 10),
                     breaks = c(0, 10)) +
  scale_y_continuous(limits = c(-2, 2),
                     breaks = c(-1, 0, 1)) +
  theme_pubr()

ggsave("out/vis_wave_20.png",
       vis_wave_20,
       dpi = 300,
       width = 3.25,
       height = 3.25)

# Sum the preceding waves.

dat_wave_sum = 
  tibble(x = seq(0, 10, by = 0.01),
         x_2pi = x * 2 * pi,
         x_4pi = x * 4 * pi,
         y_2pi = sin(x_2pi),
         y_4pi = sin(x_4pi),
         y = y_2pi + y_4pi)
  
vis_wave_sum = 
  dat_wave_sum %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y)) +
  labs(y = expression("sin(2" * pi * "x) + sin(4" * pi * "x)")) +
  scale_x_continuous(limits = c(0, 10),
                     breaks = c(0, 10)) +
  scale_y_continuous(limits = c(-2, 2),
                     breaks = c(-2, -1, 0, 1, 2)) +
  theme_pubr()

ggsave("out/vis_wave_sum.png",
       vis_wave_sum,
       dpi = 300,
       width = 3.25,
       height = 3.25)

# Get periodograms.

#  10

list_periodogram_10 = 
  dat_wave_10 %>% 
  pull(y) %>% 
  spectrum(method = "ar")

vec_periodogram_10_freq = list_periodogram_10[[1]]
vec_periodogram_10_dens = list_periodogram_10[[2]]

vis_periodogram_10 = 
  ggplot() + 
  geom_line(aes(x = vec_periodogram_10_freq * 1000,
                 y = vec_periodogram_10_dens %>% log)) +
  geom_vline(xintercept = 10,
             color = "#D73F09",
             linetype = "dashed") +
  scale_x_continuous(limits = c(0, 100),
                     breaks = c(0, 10, 50, 100)) +
  labs(x = "Period", y = "Spectral Density (Log.)") +
  theme_pubr()

ggsave("out/vis_periodogram_10.png",
       vis_periodogram_10,
       dpi = 300,
       width = 3.25,
       height = 3.25)

#  20

list_periodogram_20 = 
  dat_wave_20 %>% 
  pull(y) %>% 
  spectrum(method = "ar")

vec_periodogram_20_freq = list_periodogram_20[[1]]
vec_periodogram_20_dens = list_periodogram_20[[2]]

vis_periodogram_20 = 
  ggplot() + 
  geom_line(aes(x = vec_periodogram_20_freq * 1000,
                y = vec_periodogram_20_dens %>% log)) +
  geom_vline(xintercept = 20,
             color = "#D73F09",
             linetype = "dashed") +
  scale_x_continuous(limits = c(0, 100),
                     breaks = c(0, 20, 50, 100)) +
  labs(x = "Period", y = "Spectral Density (Log.)") +
theme_pubr()

ggsave("out/vis_periodogram_20.png",
       vis_periodogram_20,
       dpi = 300,
       width = 3.25,
       height = 3.25)

#  Sum

list_periodogram_sum = 
  dat_wave_sum %>% 
  pull(y) %>% 
  spectrum(method = "ar")

vec_periodogram_sum_freq = list_periodogram_sum[[1]]
vec_periodogram_sum_dens = list_periodogram_sum[[2]]

vis_periodogram_sum = 
  ggplot() + 
  geom_line(aes(x = vec_periodogram_sum_freq * 1000,
                y = vec_periodogram_sum_dens %>% log)) +
  geom_vline(xintercept = 10,
             color = "#D73F09",
             linetype = "dashed") +
  geom_vline(xintercept = 20,
             color = "#D73F09",
             linetype = "dashed") +
  scale_x_continuous(limits = c(0, 100),
                     breaks = c(0, 10, 20, 50, 100)) +
  labs(x = "Period", y = "Spectral Density (Log.)") +
  theme_pubr()

ggsave("out/vis_periodogram_sum.png",
       vis_periodogram_sum,
       dpi = 300,
       width = 3.25,
       height = 3.25)
