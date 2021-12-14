library(tidyverse)
# total number of locations
N <- seq(30, 120, by = 30)
# proportion of variation in the explored variable
p <- seq(0.1, 0.5, by = 0.1)
# amount of clusters
# percantage of observations taken from each cluster
r <- seq(0.1, 0.9, by = 0.1)

df <- expand.grid(N, p, r)
names(df) <- c("N", "p", "r")
rm(N, p, r)

df %>% 
  mutate(id = 1:n()) ->
  df

set.seed(42)
map_dfr(seq_along(df$N), function(i){
  # create a samle of places
  tibble(id = i,
         x = rnorm(df$N[i]*df$p[i], mean = -1),
         y = rnorm(df$N[i]*df$p[i], mean = 1),
         value = "a") ->
    a
  tibble(id = i,
         x = rnorm(df$N[i]*(1 - df$p[i])),
         y = rnorm(df$N[i]*(1 - df$p[i])),
         value = "b")  %>% 
    bind_rows(a) %>% 
    mutate(spatial_relations = "two_regions")
}) %>% 
  left_join(df) ->
  results_regions

results_regions %>% 
  count(id, N, p, value) %>% 
  filter(value == "a") %>% 
  mutate(ratio = n/N) %>% 
  select(id, ratio) %>% 
  left_join(results_regions) %>% 
  ggplot(aes(factor(p), ratio))+
  geom_violin()


set.seed(42)
map_dfr(seq_along(df$N), function(i){
  # create a samle of places
  tibble(id = i,
         x = rnorm(df$N[i]*df$p[i]),
         y = rnorm(df$N[i]*df$p[i]),
         value = "a") ->
    a
  tibble(id = i,
         x = rnorm(df$N[i]*(1 - df$p[i]), sd = 3),
         y = rnorm(df$N[i]*(1 - df$p[i]), sd = 3),
         value = "b")  %>% 
    bind_rows(a) %>% 
    mutate(spatial_relations = "central_periphery")
}) %>% 
  left_join(df) ->
  results_central

results_central %>% 
  count(id, N, p, value) %>% 
  filter(value == "a") %>% 
  mutate(ratio = n/N) %>% 
  select(id, ratio) %>% 
  left_join(results_central) %>% 
  ggplot(aes(factor(p), ratio))+
  geom_violin()

set.seed(42)
map_dfr(seq_along(df$N), function(i){
  # create a samle of places
  tibble(id = i,
         x = rnorm(df$N[i]*df$p[i]),
         y = rnorm(df$N[i]*df$p[i]),
         value = "a") ->
    a
  tibble(id = i,
         x = rnorm(df$N[i]*(1 - df$p[i])),
         y = rnorm(df$N[i]*(1 - df$p[i])),
         value = "b")  %>% 
    bind_rows(a) %>% 
    mutate(spatial_relations = "random")
}) %>% 
  left_join(df) ->
  results_random

results_random %>% 
  count(id, N, p, value) %>% 
  filter(value == "a") %>% 
  mutate(ratio = n/N) %>% 
  select(id, ratio) %>% 
  left_join(results_random) %>% 
  ggplot(aes(factor(p), ratio))+
  geom_violin()

results_random %>% 
  filter(id %in% sample(results_random$id, 20)) %>% 
  ggplot(aes(x, y, color = value))+
  geom_point()+
  facet_wrap(~id+p)

results_random %>% 
  bind_rows(results_central, 
            results_regions) ->
  results_all
# rm(results_random, results_central, results_regions, df)

results_all %>% 
  count(spatial_relations, id) %>% 
  select(-n) %>% 
  mutate(id_new = 1:n()) %>% 
  left_join(results_all) %>% 
  mutate(id = id_new) %>% 
  select(-id_new) ->
  results_all

results_all %>% 
  count(id, N, p, spatial_relations, value) %>% 
  filter(value == "a") %>% 
  mutate(ratio = n/N) %>% 
  select(id, ratio) %>% 
  left_join(results_all) %>% 
  ggplot(aes(factor(p), ratio))+
  geom_point()+
  facet_wrap(~spatial_relations, nrow = 3)

write_csv(results_all, "../data/fake_data.csv")
