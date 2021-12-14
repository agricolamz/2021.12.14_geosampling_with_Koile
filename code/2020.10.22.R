all_categories_values <- function(n_villages = 30, n_categories = 5){
  require(tidyverse,quietly = TRUE)
  partitions::parts(n_villages) %>% 
    as.matrix() %>% 
    t() %>% 
    as.data.frame() %>% 
    filter(eval(parse(text = str_c("V", n_categories+1))) == 0,
           eval(parse(text = str_c("V", n_categories))) != 0) %>% 
    select(1:all_of(n_categories)) %>% 
    mutate(set = 1:n()) %>% 
    pivot_longer(values_to = "value", names_to = "column", -set) %>% 
    group_by(set) %>% 
    mutate(ratio = value/n_villages,
           H = -sum(ratio*log2(ratio))) %>% 
    select(-ratio) %>% 
    mutate(n_villages,
           n_categories) %>% 
    pivot_wider(values_from = value, names_from = column) %>% 
    ungroup()
}


n_villages <- 80
n_categories <- 10

df <- all_categories_values(n_villages, n_categories)

map_dfr(seq_along(df$set), function(j){
  generate_equadistant(N = n_categories, n = df[j,5:ncol(df)]) %>% 
    bind_cols(df[j,1:4])  %>% 
    mutate(type = "equadistant")  
}) ->
    equadistant_dataset

equadistant_dataset %>% 
  filter(set %in% sample(1:nrow(df), 6)) %>% 
  ggplot(aes(x, y, color = id))+
  geom_point()+
  facet_wrap(~set)+
  stat_ellipse()+
  theme_bw()

# number of villages 30-40-50-60-70-80
# number of categories 3-4-5-6-7-8-9-10

# 4Garik
# rewrite with Ezi's code - done!
# generate all data and run all clustering in order to produce entropy vs proportion of variation discovered
