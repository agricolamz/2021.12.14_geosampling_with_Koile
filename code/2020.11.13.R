setwd("/home/agricolamz/work/materials/2020_SLE_Koile_Moroz_geosampling/generated_data/")
library(tidyverse)
files <- c("equadistant.csv", "center_periphery.csv")
system.time(
map(files, function(file){

df <- data.table::fread(file)

colnames(df) <-
  c(
    'x',
    'y',
    'id',
    'set',
    'H',
    'n_villages',
    'n_categories',
    'V1',
    'V2',
    'V3',
    'V4',
    'V5',
    'V6',
    'V7',
    'V8',
    'V9'
  )

set.seed(42)
map_dfr(seq(0.05, 0.9, 0.05), function(i){
df %>% 
  mutate(n_centers = round(n_villages*i)) %>% 
  group_by(set, n_centers) %>% 
  mutate(cluster = kmeans(as.matrix(tibble(x, y)), centers = unique(round(n_villages*0.05)))$cluster) %>% 
  group_by(set, cluster) %>% 
  sample_n(1) %>% 
  group_by(set, H, n_villages, n_categories) %>% 
  summarise(variability = length(unique(id))/n_categories, .groups = "drop") %>% 
  mutate(type = "k-means",
         select_p = i) %>% 
  distinct()}) ->
  k_means_results

set.seed(42)
map_dfr(seq(0.05, 0.9, 0.05), function(i){
df %>% 
  mutate(n_centers = round(n_villages*i)) %>% 
  group_by(set, n_centers) %>% 
  mutate(cluster = cutree(hclust(dist(tibble(x, y))), k = unique(n_centers))) %>% 
  group_by(set, cluster) %>% 
  sample_n(1) %>% 
  group_by(set, H, n_villages, n_categories) %>% 
  summarise(variability = length(unique(id))/n_categories, .groups = "drop") %>% 
  mutate(type = "hclust",
         select_p = i) %>% 
  distinct()}) ->
  hclust_results

set.seed(42)
map_dfr(seq(0.05, 0.9, 0.05), function(i){
df %>% 
  mutate(n_centers = round(n_villages*i)) %>% 
  group_by(set, n_centers) %>% 
  sample_n(n_centers) %>% 
  group_by(set, H, n_villages, n_categories) %>% 
  summarise(variability = length(unique(id))/n_categories, .groups = "drop") %>% 
  mutate(type = "random",
         select_p = i) %>% 
  distinct() }) ->
  random_results

k_means_results %>% 
  bind_rows(hclust_results, random_results) %>% 
  mutate(dataset = file) %>% 
  write_csv(paste0(file, "_results.csv"), append = TRUE)
  
# read_csv("all_results.csv")  
#   mutate(n_categories = factor(n_categories)) %>% 
#   lm(variability~H*type*n_categories, data = .) %>% 
#   ggpredict(terms = c("H", "type", "n_categories")) %>% 
#   plot()+
#   labs(y = "detected variability")

rm(list=ls())
}))
