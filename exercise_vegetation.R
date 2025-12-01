# Illustrate spectral analysis.

library(tidyverse)
library(ggpubr)
library(patchwork)

# Set seed.

set.seed(0112358)

# Plantation

dat_1 = 
  tibble(x = seq(1, 100, 1),
         y = rep(50) + runif(100, -5, 5))

vis_1 = 
  dat_1 %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y),
            linewidth = 1.25) +
  labs(x = "Transect Distance",
       y = "Tree Height") +
  scale_y_continuous(limits = c(0, 75)) +
  theme_minimal()

ggsave("out/vis_vegetation_1.png",
       vis_1,
       dpi = 300,
       width = 3.25,
       height = 3.25)

# Old-Growth

dat_2 = 
  tibble(x = sample.int(100, replace = TRUE) %>% unique,
         y = rep(75) + runif(length(x), -25, 25)) %>% 
  arrange(x)

vis_2 = 
  dat_2 %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y),
            linewidth = 1.25) +
  labs(x = "Transect Distance",
       y = "Tree Height") +
  scale_y_continuous(limits = c(0, 100)) +
  theme_minimal()

ggsave("out/vis_vegetation_2.png",
       vis_2,
       dpi = 300,
       width = 3.25,
       height = 3.25)

# Plantation Analysis

list_period_1 = 
  dat_1 %>% 
  pull(y) %>% 
  spectrum()

vec_period_1_freq = list_period_1[[1]]
vec_period_1_dens = list_period_1[[2]]

vis_period_1 = 
  ggplot() + 
  geom_line(aes(x = vec_period_1_freq,
                y = vec_period_1_dens %>% log)) +
  labs(x = "Frequency", y = "Spectral Density (Log.)") +
  theme_pubr()

ggsave("out/vis_period_1.png",
       vis_period_1,
       dpi = 300,
       width = 3.25,
       height = 3.25)

# Old-Growth Analysis

list_period_2 = 
  dat_2 %>% 
  pull(y) %>% 
  spectrum()

vec_period_2_freq = list_period_2[[1]]
vec_period_2_dens = list_period_2[[2]]

vis_period_2 = 
  ggplot() + 
  geom_line(aes(x = vec_period_2_freq,
                y = vec_period_2_dens %>% log)) +
  labs(x = "Frequency", y = "Spectral Density (Log.)") +
  theme_pubr()

ggsave("out/vis_period_2.png",
       vis_period_2,
       dpi = 300,
       width = 3.25,
       height = 3.25)

# Combined

dat_3 = 
  tibble(x = c(seq(1, 25, 1) * 2, seq(51, 100)),
         y = c(rep(50, 25) + runif(25, -10, 10), rep(25, 50) + runif(25, -5, 5)))

vis_3 = 
  dat_3 %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y),
            linewidth = 1.25) +
  labs(x = "Transect Distance",
       y = "Tree Height") +
  scale_y_continuous(limits = c(0, 100)) +
  theme_minimal()

ggsave("out/vis_vegetation_3.png",
       vis_3,
       dpi = 300,
       width = 3.25,
       height = 3.25)

list_period_3 = 
  dat_3 %>% 
  pull(y) %>% 
  spectrum()

vec_period_3_freq = list_period_3[[1]]
vec_period_3_dens = list_period_3[[2]]

vis_period_3 = 
  ggplot() + 
  geom_line(aes(x = vec_period_3_freq,
                y = vec_period_3_dens %>% log)) +
  labs(x = "Frequency", y = "Spectral Density (Log.)") +
  theme_pubr()

ggsave("out/vis_period_3.png",
       vis_period_3,
       dpi = 300,
       width = 3.25,
       height = 3.25)

# ENSO/PDO

dat_enso = 
  tibble(year = seq(1961, 2020, 1),
         effect_enso = rep(c(rep(-2, 3), rep(2, 3)), 10) + runif(60, -1, 1))

vis_enso = 
  dat_enso %>% 
  ggplot() +
  geom_line(aes(x = year,
                y = effect_enso),
            linewidth = 1.25) +
  scale_y_continuous(limits = c(-5, 5)) +
  labs(x = "Year", 
       y = "Made-Up ENSO (SST Deviations)") +
  theme_pubr()

ggsave("out/vis_enso.png",
       vis_enso,
       dpi = 300,
       width = 3.25,
       height = 3.25)

dat_pdo = 
  tibble(year = seq(1961, 2020, 1),
         effect_pdo = rep(c(rep(-3, 15), rep(3, 15)), 2) + runif(60, -2, 2))

vis_pdo = 
  dat_pdo %>% 
  ggplot() +
  geom_line(aes(x = year,
                y = effect_pdo),
            linewidth = 1.25) +
  scale_y_continuous(limits = c(-5, 5)) +
  labs(x = "Year", 
       y = "Made-Up PDO (SST Deviations)") +
  theme_pubr()

ggsave("out/vis_pdo.png",
       vis_pdo,
       dpi = 300,
       width = 3.25,
       height = 3.25)

dat_both = 
  left_join(dat_enso, dat_pdo) %>% 
  mutate(effect = effect_enso + effect_pdo)

vis_both = 
  dat_both %>% 
  ggplot() +
  geom_line(aes(x = year,
                y = effect),
            linewidth = 1.25) +
  scale_y_continuous(limits = c(-10, 10)) +
  labs(x = "Year", 
       y = "Made-Up PDO (SST Deviations)") +
  theme_pubr()

ggsave("out/vis_both.png",
       vis_both,
       dpi = 300,
       width = 3.25,
       height = 3.25)

list_period_enso = 
  dat_enso %>% 
  pull(effect_enso) %>% 
  spectrum(method = "ar")

vec_period_enso_freq = list_period_enso[[1]]
vec_period_enso_dens = list_period_enso[[2]]

vis_period_enso = 
  ggplot() + 
  geom_line(aes(x = vec_period_enso_freq,
                y = vec_period_enso_dens %>% log)) +
  labs(x = "Frequency", y = "Spectral Density (Log.)", title = "ENSO Analysis") +
  theme_pubr()

list_period_pdo = 
  dat_pdo %>% 
  pull(effect_pdo) %>% 
  spectrum(method = "ar")

vec_period_pdo_freq = list_period_pdo[[1]]
vec_period_pdo_dens = list_period_pdo[[2]]

vis_period_pdo = 
  ggplot() + 
  geom_line(aes(x = vec_period_pdo_freq,
                y = vec_period_pdo_dens %>% log)) +
  labs(x = "Frequency", y = "Spectral Density (Log.)", title = "PDO Analysis") +
  theme_pubr()

list_period_both = 
  dat_both %>% 
  pull(effect) %>% 
  spectrum(method = "ar")

vec_period_both_freq = list_period_both[[1]]
vec_period_both_dens = list_period_both[[2]]

vis_period_both = 
  ggplot() + 
  geom_line(aes(x = vec_period_both_freq,
                y = vec_period_both_dens %>% log)) +
  labs(x = "Frequency", y = "Spectral Density (Log.)", title = "Combined Analysis") +
  theme_pubr()

vis_period_climate = vis_period_enso + vis_period_pdo + vis_period_both

ggsave("out/vis_period_climate.png",
       vis_period_climate,
       dpi = 300,
       width = 6.5,
       height = 3)
