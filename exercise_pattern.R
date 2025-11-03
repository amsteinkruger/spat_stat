# Visualize point patterns and relevant statistical procedures.

library(tidyverse) # Get packages.
library(patchwork)

set.seed(0112358) # Pick a seed.

par_pick = 3 # Pick a particular observation to highlight.
par_n = 33 # Pick a number of observations.
par_sd = 0.33 # Pick SD for normal draws.
par_max = 1 # Pick maximum for uniform draws.
par_min = -1 # Pick minimum for uniform draws.
par_distance = ((par_max - par_min) ^ 2 + (par_max - par_min) ^ 2) ^ (1 / 2) # Get greatest possible Euclidean distance.
par_r = 0.50 # Pick a radius for RNN.

# Simulate clustering on two dimensions, then try RNN for the first point.

dat_cluster = 
  tibble(id = seq(1, par_n),
         x = rnorm(par_n, 0, par_sd),
         y = rnorm(par_n, 0, par_sd),
         pick = ifelse(id == par_pick, 1, 0) %>% factor)

dat_cluster_edges = 
  tibble(id_1 = rep(par_pick, par_n - 1),
         id_2 = seq(1, par_n)[-par_pick]) %>% 
  left_join(dat_cluster %>% select(-pick) %>% rename_with(~ str_c(., "_1"), everything())) %>% 
  left_join(dat_cluster %>% select(-pick) %>% rename_with(~ str_c(., "_2"), everything())) %>% 
  mutate(distance = ((x_1 - x_2) ^ 2 + (y_1 - y_2) ^ 2) ^ (1 / 2)) %>% 
  rename(from = id_1, 
         to = id_2) %>% 
  mutate(from_to = paste0(from, "_", to)) %>% 
  pivot_longer(cols = c(x_1, y_1, x_2, y_2),
               names_sep = "_",
               names_to = c("var", "which")) %>% 
  select(-which) %>% 
  pivot_wider(values_from = value,
              names_from = var) %>% 
  unnest %>% 
  mutate(pick = ifelse(distance < par_r, 1, 0) %>% factor)

vis_cluster = 
  ggplot() +
  geom_line(data = dat_cluster_edges,
            aes(x = x,
                y = y,
                group = from_to,
                color = pick),
            size = 1.50,
            alpha = 0.75) + 
  geom_point(data = dat_cluster,
             aes(x = x,
                 y = y,
                 fill = pick),
             shape = 21,
             size = 2.00,
             color = scales::pal_viridis(option = "F")(2)[1]) +
  scale_x_continuous(limits = c(par_min, par_max)) +
  scale_y_continuous(limits = c(par_min, par_max)) +
  scale_color_manual(values = c("#000000", "#D73F09")) +
  scale_fill_manual(values = c("#000000", "#D73F09")) +
  theme_minimal() +
  theme(legend.position = "none")

dat_cluster_rnn = 
  dat_cluster_edges %>% 
  distinct(from_to, distance) %>% 
  select(distance) %>% 
  arrange(distance) %>% 
  nest %>% 
  expand(r = seq(0, par_distance, by = 0.1), data) %>% 
  unnest(data) %>% 
  mutate(check = ifelse(distance < r, 1, 0)) %>% 
  group_by(r) %>% 
  summarize(f = sum(check) / (par_n - 1)) %>% 
  ungroup %>% 
  mutate(f_lag = f %>% lag) %>% 
  filter(f_lag < 1) %>% 
  select(-f_lag) %>% 
  mutate(p = 1 - exp(-1 * pi * r ^ 2))

vis_cluster_rnn = 
  dat_cluster_rnn %>% 
  ggplot() + 
  geom_line(aes(x = r,
                y = f),
            color = "#D73F09",
            size = 1.50) +
  # geom_line(aes(x = r,
  #               y = p),
  #           size = 1.50) +
  labs(x = "Radius (r)", y = "F(d < r)") +
  scale_x_continuous(limits = c(0, 1.00),
                     breaks = c(0, 0.50, 1.00)) +
  scale_y_continuous(limits = c(0, NA),
                     breaks = c(0, 0.50, 1.00)) +
  theme_minimal() +
  theme(legend.position = "none")

vis_cluster_patchwork = vis_cluster + vis_cluster_rnn

ggsave("out/plot_patterns_cluster.png",
       vis_cluster_patchwork,
       dpi = 300,
       width = 6.50,
       height = 3.25, 
       bg = "transparent")

# Simulate dispersal on two dimensions, then try second order analysis.

dat_disperse = 
  tibble(id = seq(1, par_n),
         x = runif(par_n, par_min, par_max),
         y = runif(par_n, par_min, par_max)) # ,
# pick = ifelse(id == 1, 1, 0) %>% factor)

dat_disperse_edges = 
  tibble(id_1 = seq(1, par_n),
         id_2 = seq(1, par_n)) %>% 
  expand(id_1, id_2) %>% 
  left_join(dat_disperse %>% rename_with(~ str_c(., "_1"), everything())) %>% 
  left_join(dat_disperse %>% rename_with(~ str_c(., "_2"), everything())) %>% 
  mutate(distance = ((x_1 - x_2) ^ 2 + (y_1 - y_2) ^ 2) ^ (1 / 2)) %>% 
  rename(from = id_1, 
         to = id_2) %>% 
  filter(from < to) %>% 
  mutate(from_to = paste0(from, "_", to)) %>% 
  pivot_longer(cols = c(x_1, y_1, x_2, y_2),
               names_sep = "_",
               names_to = c("var", "which")) %>% 
  select(-which) %>% 
  pivot_wider(values_from = value,
              names_from = var) %>% 
  unnest # %>% 
  # mutate(pick = ifelse(distance < par_r, 1, 0) %>% factor)

vis_disperse = 
  dat_disperse %>% 
  ggplot() +
  geom_point(aes(x = x,
                 y = y),
             shape = 21,
             size = 2.00) +
  scale_x_continuous(limits = c(par_min, par_max)) +
  scale_y_continuous(limits = c(par_min, par_max)) +
  theme_minimal() +
  theme(legend.position = "none")

dat_disperse_soa = 
  dat_disperse_edges %>% 
  distinct(from_to, distance) %>% 
  select(distance) %>% 
  arrange(distance) %>% 
  mutate(count = row_number()) %>% 
  mutate(L = abs(par_max ^ 2 * count * (1 / (pi * par_n * (par_n - 1)))) ^ (1 / 2))
  
vis_disperse_soa = 
  dat_disperse_soa %>% 
  ggplot() + 
  geom_line(aes(x = distance,
                y = L),
            color = "#D73F09",
            size = 1.50) +
  # geom_line(aes(x = distance,
  #               y = distance),
  #           size = 1.50) +
  labs(x = "Distance (d)", y = "L(d)") +
  # scale_x_continuous(limits = c(0, 1.00),
  #                    breaks = c(0, 0.50, 1.00)) +
  # scale_y_continuous(limits = c(0, NA),
  #                    breaks = c(0, 0.50, 1.00)) +
  theme_minimal() +
  theme(legend.position = "none")

vis_disperse_patchwork = vis_disperse + vis_disperse_soa

ggsave("out/plot_patterns_disperse.png",
       vis_disperse_patchwork,
       dpi = 300,
       width = 6.50,
       height = 3.25, 
       bg = "transparent")
