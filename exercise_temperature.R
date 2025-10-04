# Observe autocorrelation, correlation, and cross-correlation in a random walk.

library(tidyverse)
library(patchwork)

# Set seed.

set.seed(0112358)

# Set up individuals.

x_1 = seq(1, 100) # c(-1, 1) %>% sample(100, TRUE) %>% cumsum 
y_1 = c(-1, 1) %>% sample(100, TRUE) %>% cumsum 
z_1 = rep(37, 100) + seq(0, 0.50, by = 0.50 / 100)[1:100]
x_2 = x_1
y_2 = y_1 + seq(15, 0, by = -0.1)[1:100]
z_2 = z_1

dat_1 =
  tibble(id = rep(1, 100),
         x = x_1,
         y = y_1,
         z = z_1)

dat_2 =
  tibble(id = rep(2, 100),
         x = x_2,
         y = y_2,
         z = z_2)

dat = bind_rows(dat_1, dat_2)

# Plot.

dat %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y,
                group = id),
            color = scales::pal_viridis(option = "F")(2)[1]) +
  geom_point(aes(x = x,
                 y = y,
                 fill = z),
             shape = 21,
             size = 3.00,
             color = scales::pal_viridis(option = "F")(2)[1]) +
  labs(fill = "Temperature") +
  scale_fill_viridis_c(option = "F", 
                       limits = c(37, 37.50), 
                       breaks = c(37, 37.25, 37.50)) +
  theme_minimal() +
  theme(legend.ticks = element_blank())

ggsave("out/plot_temperature.png",
       dpi = 300,
       width = 4,
       height = 3.25, 
       bg = "transparent")
