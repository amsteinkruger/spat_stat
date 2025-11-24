# beep boop

library(tidyverse)
library(patchwork)

# Set seed.

set.seed(0112358)

# Set up a pattern, then introduce anomalies.

n = 100

dat = 
  tibble(x = seq(1, 100, by = 0.01)) %>% #,
  # y = rnorm(n, 50, 1),
  # z = c(rep(1, 45), runif(10, 0.50, 1.50), rep(1, 45)),
  # y_anomalous = y + y * z,
  # anomaly = c(rep("Typical", 45), rep("Anomalous", 10), rep("Typical", 45)))
  mutate(y = sin(x),
         y_anomalous = sin(x / 2) * (1 + 25 * dnorm(x / 2, 25, 2.5)))

# Plot.

vis = 
  dat %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y_anomalous)) +
  theme_minimal()
