# Illustrate wavelet analysis.

library(tidyverse)
library(ggpubr)
library(patchwork)
library(wsyn)

# Set seed.

set.seed(0112358)

# Set up data.

dat_1 = 
  tibble(x = seq(1, 50, 1),
         y = rep(50) + runif(50, -5, 5))

dat_2 = 
  tibble(x = seq(51, 100, 1),
         y = rep(75) + runif(50, -25, 25))

dat_3 = bind_rows(dat_1, dat_2)

# Plot data.

vis_1 = 
  dat_3 %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y),
            linewidth = 1.25) +
  labs(x = "Transect Distance",
       y = "Tree Height") +
  scale_y_continuous(limits = c(0, 100)) +
  theme_minimal()

ggsave("out/vis_wavelet_1.png",
       vis_1,
       dpi = 300,
       width = 3.25,
       height = 3.25)

# Set up a wavelet plot.
#  With reference to https://library.virginia.edu/data/articles/using-wavelets-analyze-time-series-data

vec_x = min(dat_3$x):max(dat_3$x) # 1:100

dat_clean <- cleandat(dat_3$y, times = vec_x, clev = 1)$cdat

dat_wavelet <- wt(dat_clean, vec_x)

plotmag(dat_wavelet)

# Set up more data (ENSO, PDO).

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
       y = "Made-Up ENSO + PDO") +
  theme_pubr()

vis_all = vis_enso + vis_pdo + vis_both

ggsave("out/vis_wavelet_3.png",
       vis_all,
       dpi = 300,
       width = 6.5,
       height = 3)

# Set up another wavelet plot.

vec_x_climate = min(dat_both$year):max(dat_both$year) # 1961:2020

dat_clean_climate <- cleandat(dat_both$effect, times = vec_x_climate, clev = 1)$cdat

dat_wavelet_climate <- wt(dat_clean_climate, vec_x_climate)

plotmag(dat_wavelet_climate)
