# Get nearest-neighbor distances and Ripley's K.

library(tidyverse)

# Set up data.

dat_right =
  tibble(A = c(0.4, 0.6, 0.15, 0.3, 0.9, 0.7, 0.5, 0.8, 0.15) %>% sort,
         B = c(0.15, 0.15, 0.25, 0.25, 0.35, 0.35, 0.5, 0.55, 0.6) %>% sort,
         C = c(0.5, 0.8, 0.7, 0.9, 0.85, 0.5, 0.8, 0.7, 0.9) %>% sort)

dat_left = 
  tibble(bin = seq(0, 1, by = 0.1))

dat_A = 
  dat_left %>% 
  expand_grid(dat_right %>% select(A)) %>% 
  mutate(check = A <= bin) %>% 
  group_by(bin) %>% 
  summarize(A = check %>% sum) %>% 
  ungroup

dat_B = 
  dat_left %>% 
  expand_grid(dat_right %>% select(B)) %>% 
  mutate(check = B <= bin) %>% 
  group_by(bin) %>% 
  summarize(B = check %>% sum) %>% 
  ungroup

dat_C = 
  dat_left %>% 
  expand_grid(dat_right %>% select(C)) %>% 
  mutate(check = C <= bin) %>% 
  group_by(bin) %>% 
  summarize(C = check %>% sum) %>% 
  ungroup

dat = 
  dat_A %>% 
  left_join(dat_B) %>% 
  left_join(dat_C)

# Plot.

vis = 
  dat %>% 
  pivot_longer(cols = c(A, B, C)) %>% 
  ggplot() +
  geom_line(aes(x = bin,
                 y = value,
                 group = name,
                 color = name),
            linewidth = 1.25) +
  labs(x = "x",
       y = "Cumulative Count",
       color = NULL) +
  scale_y_continuous(limits = c(0, 10),
                     breaks = c(0, 5, 10)) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

# Export.

ggsave("out/vis_nearest_1.png",
       vis,
       dpi = 300,
       width = 3.25,
       height = 3.25)
