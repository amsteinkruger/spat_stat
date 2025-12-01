# Illustrate spectral analysis.

library(tidyverse)
library(ggpubr)
library(patchwork)

# Set seed.

set.seed(0112358)

# ENSO/PDO

#  Set up data.

dat_enso = 
  tibble(year = seq(1961, 2020, 1),
         effect_enso = rep(c(rep(-2, 3), rep(2, 3)), 10) + runif(60, -1, 1))

dat_pdo = 
  tibble(year = seq(1961, 2020, 1),
         effect_pdo = rep(c(rep(-3, 15), rep(3, 15)), 2) + runif(60, -2, 2))

dat_both = 
  left_join(dat_enso, dat_pdo) %>% 
  mutate(effect = effect_enso + effect_pdo)

list_enso = 
  dat_enso %>% 
  pull(effect_enso) %>% 
  spectrum(method = "ar")

#  Set up spectral analyses with plots.

vec_enso_freq = list_enso[[1]]
vec_enso_dens = list_enso[[2]]

vis_enso = 
  ggplot() + 
  geom_line(aes(x = vec_enso_freq,
                y = vec_enso_dens %>% log)) +
  labs(x = "Frequency", y = "Spectral Density (Log.)", title = "ENSO Analysis") +
  theme_pubr()

list_pdo = 
  dat_pdo %>% 
  pull(effect_pdo) %>% 
  spectrum(method = "ar")

vec_pdo_freq = list_pdo[[1]]
vec_pdo_dens = list_pdo[[2]]

vis_pdo = 
  ggplot() + 
  geom_line(aes(x = vec_pdo_freq,
                y = vec_pdo_dens %>% log)) +
  labs(x = "Frequency", y = "Spectral Density (Log.)", title = "PDO Analysis") +
  theme_pubr()

list_both = 
  dat_both %>% 
  pull(effect) %>% 
  spectrum(method = "ar")

vec_both_freq = list_both[[1]]
vec_both_dens = list_both[[2]]

vis_both = 
  ggplot() + 
  geom_line(aes(x = vec_both_freq,
                y = vec_both_dens %>% log)) +
  labs(x = "Frequency", y = "Spectral Density (Log.)", title = "Combined Analysis") +
  theme_pubr()

vis_climate = vis_enso + vis_pdo + vis_both

ggsave("vis_climate.png",
       vis_climate,
       dpi = 300,
       width = 6.5,
       height = 3)
