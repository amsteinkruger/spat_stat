# Compare distances and differences among points in two dimensions.

library(tidyverse)
library(patchwork)

# Set seed.

set.seed(0112358)

# Set up individuals.

dat = 
  tibble(id = seq(1, 10),
         x = rnorm(10),
         y = rnorm(10),
         z = rnorm(10))

# Plot.

dat %>% 
  ggplot() +
  geom_point(aes(x = x,
                 y = y,
                 fill = z),
             shape = 21,
             size = 3.00,
             color = scales::pal_viridis(option = "F")(2)[1]) +
  scale_fill_viridis_c(option = "F", 
                       limits = c(-2.50, 2.50), 
                       breaks = c(-2.50, 0, 2.50)) +
  theme_minimal() +
  theme(legend.ticks = element_blank())

ggsave("out/plot_1.png",
       dpi = 300,
       width = 4,
       height = 3.25, 
       bg = "transparent")

# Get distances.

dat_more_stat = 
  tibble(id_1 = 1:10, id_2 = 1:10) %>% 
  expand(id_1, id_2) %>% 
  left_join(dat %>% rename_with(~ str_c(., "_1"), everything())) %>% 
  left_join(dat %>% rename_with(~ str_c(., "_2"), everything())) %>% 
  mutate(distance = ((x_1 - x_2) ^ 2 + (y_1 - y_2) ^ 2) ^ (1 / 2),
         difference = z_1 - z_2) %>% 
  filter(id_1 < id_2) # Quick fix for doubled edges.

dat_more_plot = 
  dat_more_stat %>% 
  filter(id_1 != id_2) %>% 
  rename(from = id_1, 
         to = id_2) %>% 
  mutate(from_to = paste0(from, "_", to)) %>% 
  pivot_longer(cols = c(x_1, y_1, z_1, x_2, y_2, z_2),
               names_sep = "_",
               names_to = c("var", "which")) %>% 
  select(-which) %>% 
  pivot_wider(values_from = value,
              names_from = var) %>% 
  unnest

# Distance

vis_distance_scatter = 
  dat_more_plot %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y,
                group = from_to,
                color = distance),
            size = 1.50,
            alpha = 0.50) +
  geom_point(aes(x = x,
                 y = y,
                 group = from_to),
             shape = 21,
             size = 3.00,
             fill = scales::pal_viridis(option = "F")(2)[2],
             color = scales::pal_viridis(option = "F")(2)[1]) +
  labs(color = "Distance") +
  scale_color_viridis_c(option = "F", 
                        limits = c(0, 4), 
                        breaks = c(0, 2, 4)) +
  theme_minimal() +
  theme(legend.ticks = element_blank())

vis_distance_histogram = 
  dat_more_plot %>% 
  distinct(distance) %>% 
  mutate(Bin = 
           distance %>% 
           cut(breaks = 
                 seq(distance %>% min %>% floor, 
                     distance %>% max %>% ceiling, 
                     by = 0.50))) %>% 
  group_by(Bin) %>% 
  summarize(Count = n()) %>% 
  ungroup %>% 
  ggplot() +
  geom_col(aes(x = Bin,
               y = Count),
           fill = scales::pal_viridis(option = "F")(3)[2],
           color = scales::pal_viridis(option = "F")(3)[2]) +
  labs(x = "z") +
  scale_y_continuous(limits = c(0, 14),
                     breaks = c(0, 7, 14)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

vis_distance = vis_distance_scatter + vis_distance_histogram

ggsave("out/plot_2.png",
       vis_distance,
       dpi = 300,
       width = 6.5,
       height = 3.25, 
       bg = "transparent")

# Difference

vis_difference_scatter = 
  dat_more_plot %>% 
  ggplot() +
  geom_line(aes(x = x,
                y = y,
                group = from_to,
                color = difference),
            size = 1.50,
            alpha = 0.50) +
  geom_point(aes(x = x,
                 y = y,
                 group = from_to, 
                 fill = z),
             shape = 21,
             size = 3.00,
             color = scales::pal_viridis(option = "F")(2)[1]) +
  labs(color = "Difference",
       fill = "z") +
  scale_color_viridis_c(option = "F", 
                        limits = c(-5, 5),
                        breaks = c(-5, 0, 5)) +
  scale_fill_viridis_c(option = "F", 
                       limits = c(-5, 5),
                       breaks = c(-5, 0, 5)) +
  theme_minimal() +
  theme(legend.ticks = element_blank(),
        legend.key.height = unit(0.75, "lines"))

vis_difference_histogram = 
  dat_more_plot %>% 
  distinct(difference) %>% 
  mutate(Bin = 
           difference %>% 
           cut(breaks = 
                 seq(difference %>% min %>% floor, 
                     difference %>% max %>% ceiling, 
                     by = 1.00))) %>% 
  group_by(Bin) %>% 
  summarize(Count = n()) %>% 
  ungroup %>% 
  ggplot() +
  geom_col(aes(x = Bin,
               y = Count),
           fill = scales::pal_viridis(option = "F")(3)[2],
           color = scales::pal_viridis(option = "F")(3)[2]) +
  scale_y_continuous(limits = c(0, 20),
                     breaks = c(0, 10, 20)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

vis_difference = vis_difference_scatter + vis_difference_histogram

ggsave("out/plot_3.png",
       vis_difference,
       dpi = 300,
       width = 6.5,
       height = 3.25, 
       bg = "transparent")
