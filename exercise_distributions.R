# Compare fake probability distributions.

library(tidyverse)

# Set up individuals.

dat = 
  tibble(id = seq(1, 10),
         x = rnorm(10),
         y = rnorm(10),
         z = rnorm(10))

e = exp(1)

dat = 
  tibble(acres = seq(1, 10, by = 0.1),
         't' = 01.00 * e ^ -(01.00 * acres),
         't + n' = 00.66 * e ^ -(00.66 * acres)) %>% 
  pivot_longer(cols = c(t, 't + n'))

vis = 
  dat %>% 
  ggplot() +
  geom_line(aes(x = acres,
                y = value,
                color = name),
            linewidth = 1.5) +
  labs(x = "Acres Burnt (1e3)",
       y = "Probability Density",
       color = "Period") +
  theme_minimal()

ggsave("out/plot_density.png",
       vis,
       dpi = 300,
       width = 4.5,
       height = 3)
