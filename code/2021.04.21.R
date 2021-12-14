setwd("/home/agricolamz/work/materials/2021_SLE_Koile_Moroz_geosampling/generated_data/")
library(tidyverse)

# generate data -----------------------------------------------------------

files <- c("equidistant", "center_periphery")
# map(files, function(x) list.files(pattern = str_c(x, "_"))) %>% 
#   unlist() %>% 
#   file.remove()

villages_fraction <- seq(0.1, 0.9, 0.1)

gc()

ft <- data.frame(from = 106663*0:4+1,
                 to = 106663*1:5)

map(seq_along(ft$from), function(section){
  map(files, function(file){
    df <- data.table::fread(str_c(file, ".csv"))
    
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
    
    
    
    # # reduce in order to check the size
    # df %>% 
    #   filter(set %in% sample(1:533315, 150)) ->
    #   df
    
    df %>% 
      filter(set %in% ft$from[section]:ft$to[section]) ->
      df
    
    map(villages_fraction, function(i){
      set.seed(42)
      df %>%
        mutate(n_centers = round(n_villages*i)) %>%
        group_by(set, n_centers) %>%
        mutate(cluster = tryCatch(kmeans(as.matrix(tibble(x, y)),
                                         centers = unique(n_centers))$cluster,
                                  error = function(e) "problem")) %>%
        group_by(set, cluster) %>%
        sample_n(1) %>%
        group_by(set, H, n_villages, n_categories) %>%
        summarise(variability = ifelse(cluster != "problem",
                                       length(unique(id))/n_categories,
                                       NA), .groups = "drop") %>%
        mutate(type = "k-means",
               space = file,
               select_p = i) %>%
        distinct() %>%
        write_csv(str_c(file, "_k_means_results.csv"), append = TRUE)
      gc()
    })
    
    map(villages_fraction, function(i){
      set.seed(42)
      df %>% 
        mutate(n_centers = round(n_villages*i)) %>% 
        group_by(set, n_centers) %>% 
        mutate(cluster = tryCatch(cutree(hclust(dist(tibble(x, y))), 
                                         k = unique(n_centers)),
                                  error = function(e) "problem")) %>% 
        group_by(set, cluster) %>% 
        sample_n(1) %>% 
        group_by(set, H, n_villages, n_categories) %>% 
        summarise(variability = ifelse(cluster != "problem", 
                                       length(unique(id))/n_categories,
                                       NA), .groups = "drop") %>% 
        mutate(type = "h-clust",
               space = file,
               select_p = i) %>% 
        distinct() %>% 
        write_csv(str_c(file, "_h_clust_results.csv"), append = TRUE)
      gc()
    })
    
    map(villages_fraction, function(i){
      set.seed(42)
      df %>% 
        mutate(n_centers = round(n_villages*i)) %>% 
        group_by(set, n_centers) %>% 
        sample_n(n_centers) %>% 
        group_by(set, H, n_villages, n_categories) %>% 
        summarise(variability = length(unique(id))/n_categories, .groups = "drop") %>% 
        mutate(type = "random",
               space = file,
               select_p = i) %>% 
        distinct() %>% 
        write_csv(str_c(file, "_random_results.csv"), append = TRUE)
      gc()
    })
    
    rm(list=ls())
    gc()
  })
})

# visualize results -------------------------------------------------------
cp_km <- data.table::fread("center_periphery_k_means_results.csv")
cp_hc <- data.table::fread("center_periphery_h_clust_results.csv")
cp_r <- data.table::fread("center_periphery_random_results.csv")
eq_km <- data.table::fread("equidistant_k_means_results.csv")
eq_hc <- data.table::fread("equidistant_h_clust_results.csv")
eq_r <- data.table::fread("equidistant_random_results.csv")

cp_km %>%
  bind_rows(cp_hc,
            cp_r,
            eq_km,
            eq_hc,
            eq_r) ->
  full_dataset

colnames(full_dataset) <-
  c(
    'set',
    'H',
    'n_villages',
    'n_categories',
    'discovery_fraction',
    'type',
    'location',
    'village_sample_fraction'
  )

rm(cp_hc, cp_km, cp_r, eq_hc, eq_km, eq_r)

full_dataset %>%
  filter(set %in% sample(1:533315, 100000)) %>% 
  ggplot(aes(village_sample_fraction, discovery_fraction, color = type))+
  geom_smooth(se = FALSE, method = "glm", method.args = list(family = "binomial"))+
  #geom_smooth(se = FALSE)+
  facet_grid(location~n_categories)

full_dataset %>%
  filter(set %in% sample(1:533315, 100000)) %>% 
  ggplot(aes(H, discovery_fraction, color = factor(n_categories), linetype = type))+
  geom_smooth(se = FALSE, method = "glm", method.args = list(family = "binomial"))+
  #geom_smooth(se = FALSE)+
  facet_grid(location~type)+
  scale_linetype_manual("", values = c(1, 2, 4))
