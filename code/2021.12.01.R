setwd("/home/agricolamz/work/materials/2021_SLE_Koile_Moroz_geosampling/generated_data/")
library(tidyverse)

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

theme_set(theme_bw())
full_dataset %>%
  filter(set %in% sample(1:533315, 100000)) %>% 
  ggplot(aes(village_sample_fraction, discovery_fraction, color = type))+
  geom_smooth(se = FALSE, method = "glm", method.args = list(family = "binomial"))+
  #geom_smooth(se = FALSE)+
  facet_grid(location~n_categories)
ggsave("~/Desktop/fig1.png")

full_dataset %>%
  filter(set %in% sample(1:533315, 100000)) %>% 
  ggplot(aes(H, discovery_fraction, color = factor(n_categories), linetype = type))+
  geom_smooth(se = FALSE, method = "glm", method.args = list(family = "binomial"))+
  #geom_smooth(se = FALSE)+
  facet_grid(location~type)+
  scale_linetype_manual("", values = c(1, 2, 4))
ggsave("~/Desktop/fig2.png")
