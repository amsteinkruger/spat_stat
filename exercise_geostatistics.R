# Illustrate geostatistics.

library(tidyverse)
library(patchwork)

# Set seed.

set.seed(0112358)

# Set up a pattern, then introduce an anomaly.

n = 100

dat = 
  tibble(x = seq(1, 100, by = 0.01)) %>% 
  mutate(y = sin(x),
         y_anomalous = sin(x / 2) * (1 + 25 * dnorm(x / 2, 25, 2.5)))

# Plot.

vis = 
  dat %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y_anomalous)) +
  scale_y_continuous(limits = c(-5, 5),
                     breaks = c(-5, -2.5, -1, 0, 1, -2.5, 5)) +
  labs(x = "x", y = "y") +
  theme_pubr()

ggsave("out/vis_anomaly.png",
       vis,
       dpi = 300,
       width = 3.25,
       height = 3.25)

# Set up a pattern, then introduce anisotropy.

dat_anisotropy = 
  tibble(x = runif(250),
         y = runif(250) * (1 + (1 / 2) * dnorm(x, 0.50, 0.05)))

# Plot.

vis_anisotropy = 
  dat_anisotropy %>% 
  ggplot() + 
  geom_point(aes(x = x,
                 y = y),
             shape = 21,
             color = "black",
             fill = NA) +
  theme_pubr()

ggsave("out/vis_anisotropy.png",
       vis_anisotropy,
       dpi = 300,
       width = 3.25,
       height = 3.25)

# Set up a pattern, then introduce nonstationarity.

dat_nonstationarity = 
  tibble(x = runif(250),
         y = runif(250) + x)

vis_nonstationarity = 
  dat_nonstationarity %>% 
  ggplot() +
  geom_point(aes(x = x,
                 y = y),
             shape = 21,
             color = "black",
             fill = NA) +
  theme_pubr()

ggsave("out/vis_nonstationarity.png",
       vis_nonstationarity,
       dpi = 300,
       width = 3.25,
       height = 3.25)

# Set up a pattern of seasonal variation in daily temperature, then get autocorrelation.

dat_autocorr_time = 
  tibble(date = seq(as.Date('1901-01-01'), as.Date('1903-12-31'), by = 1),
         temperature = 30 + 3600 * dnorm(yday(date), 182, 45) + rnorm(1095, 0, 10))

# Plot.

vis_autocorr_time_data = 
  dat_autocorr_time %>% 
  ggplot() + 
  geom_line(aes(x = date,
                 y = temperature)) +
  labs(x = "Year", y = "Temperature (F)") +
  theme_pubr()

vis_autocorr_time_stat = 
  dat_autocorr_time %>% 
  pull(temperature) %>% 
  acf(lag.max = 365) %>% 
  magrittr::use_series(acf) %>% 
  as_tibble %>% 
  filter(row_number() != 1) %>% 
  rename(ACF = 1) %>% 
  mutate(Lag = 1:nrow(.)) %>% 
  ggplot() +
  geom_line(aes(x = Lag,
                y = ACF)) +
  labs(x = "Days") +
  theme_pubr()

# Combine plots.

vis_autocorr_time = vis_autocorr_time_data + vis_autocorr_time_stat

ggsave("out/vis_autocorr_time.png",
       dpi = 300,
       width = 6.5,
       height = 3.25)

# Set up a pattern of windthrows over space, then get a spherical semivariogram.

# Actually, don't.
