# Get fractal statistics from a random walk.

library(tidyverse)

# Set seed.

set.seed(0112358)

# Set path.

dat = 
  tibble(step = seq(1, 20),
         x = c(1, rep(NA, 19)),
         y = c(1, rep(NA, 19)),
         length = c(0, sample(1:5, 19, replace = TRUE)),
         length_x = (length / 2) ^ (1 / 2), # Let x = y for all lengths.
         length_y = length_x,
         angle = c(0, rep(45, 19)))

for(i in 2:20){dat$x[i] = dat$x[i - 1] + dat$length_x[i]}
for(i in 2:20){dat$y[i] = dat$y[i - 1] + dat$length_y[i]}

# Plot.

vis = 
  dat %>% 
  ggplot() + 
  geom_line(aes(x = x,
                y = y),
            linewidth = 1.25) +
  geom_point(aes(x = x,
                 y = y)) +
  theme_minimal()

ggsave("out/vis_walk.png",
       vis,
       dpi = 300,
       width = 3.25,
       height = 3.25)

write_csv(dat, "out/dat_walk.csv")

# Plot a histogram.

vis_histogram = 
  dat %>% 
  group_by(length) %>% 
  summarize(count = n()) %>% 
  ungroup %>% 
  arrange(length) %>% 
  mutate(length = length %>% factor) %>% 
  ggplot() +
  geom_col(aes(x = length,
               y = count)) +
  labs(x = "Length", y = "Count") +
  theme_minimal()

ggsave("out/vis_walk_histogram.png",
       vis_histogram,
       dpi = 300,
       width = 3.25,
       height = 3.25)
