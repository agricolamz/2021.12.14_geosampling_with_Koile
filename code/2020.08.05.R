library(tidyverse)
theme_set(theme_bw())

# df_cl_5_v_30 ------------------------------------------------------------
n_villages <- 30
n_category <- 5
X <- as.matrix(expand.grid(1:n_villages, 
                           1:n_villages, 
                           1:n_villages, 
                           1:n_villages)) # repeat n_clusters - 1 times
X %>% 
  as_tibble() %>% 
  mutate(rs = rowSums(.)) %>% 
  filter(rs <= n_villages) %>% 
  mutate(Var5 = n_villages-rs) %>% 
  filter(Var5 > 0) %>% 
  filter(Var1 >= Var2,
         Var1 >= Var3,
         Var1 >= Var4,
         Var1 >= Var5,
         Var2 >= Var3,
         Var2 >= Var4,
         Var2 >= Var5,
         Var3 >= Var4,
         Var3 >= Var5,
         Var4 >= Var5) %>% 
  select(-rs) %>% 
  rowwise() %>% 
  mutate(H = -sum(Var1/n_villages*log2(Var1/n_villages),
                  Var2/n_villages*log2(Var2/n_villages),
                  Var3/n_villages*log2(Var3/n_villages),
                  Var4/n_villages*log2(Var4/n_villages),
                  Var5/n_villages*log2(Var5/n_villages))) %>% 
  arrange(H) %>% 
  ungroup() %>% 
  mutate(id = 1:n(),
         n_villages = n_villages,
         n_category = n_category) %>% 
  select(id, n_category, n_villages, H, Var1, Var2, Var3, Var4, Var5) %>% 
  slice(round(seq(nrow(.), 1, length.out = 30))) ->
  cl_5_v_30_sample

scale <- 1
eq_x5 <- cos(2*pi/5*0:4)*scale
eq_y5 <- sin(2*pi/5*0:4)*scale
cp_x5 <- c(0, 1, 0, -1, 0) * scale
cp_y5 <- c(0, 0, 1, 0, -1) * scale


map_dfr(1:30, function(i){
  tibble(x = c(rnorm(cl_5_v_30_sample$Var1[i], mean = cp_x5[1]),
               rnorm(cl_5_v_30_sample$Var2[i], mean = cp_x5[2]),
               rnorm(cl_5_v_30_sample$Var3[i], mean = cp_x5[3]),
               rnorm(cl_5_v_30_sample$Var4[i], mean = cp_x5[4]),
               rnorm(cl_5_v_30_sample$Var5[i], mean = cp_x5[5])),
         y = c(rnorm(cl_5_v_30_sample$Var1[i], mean = cp_y5[1]),
               rnorm(cl_5_v_30_sample$Var2[i], mean = cp_y5[2]),
               rnorm(cl_5_v_30_sample$Var3[i], mean = cp_y5[3]),
               rnorm(cl_5_v_30_sample$Var4[i], mean = cp_y5[4]),
               rnorm(cl_5_v_30_sample$Var5[i], mean = cp_y5[5])),
         value = c(rep("Var1", cl_5_v_30_sample$Var1[i]),
                   rep("Var2", cl_5_v_30_sample$Var2[i]),
                   rep("Var3", cl_5_v_30_sample$Var3[i]),
                   rep("Var4", cl_5_v_30_sample$Var4[i]),
                   rep("Var5", cl_5_v_30_sample$Var5[i])),
         type = "central-periphery") ->
    cp
  
  tibble(x = c(rnorm(cl_5_v_30_sample$Var1[i], mean = eq_x5[1]),
               rnorm(cl_5_v_30_sample$Var2[i], mean = eq_x5[2]),
               rnorm(cl_5_v_30_sample$Var3[i], mean = eq_x5[3]),
               rnorm(cl_5_v_30_sample$Var4[i], mean = eq_x5[4]),
               rnorm(cl_5_v_30_sample$Var5[i], mean = eq_x5[5])),
         y = c(rnorm(cl_5_v_30_sample$Var1[i], mean = eq_y5[1]),
               rnorm(cl_5_v_30_sample$Var2[i], mean = eq_y5[2]),
               rnorm(cl_5_v_30_sample$Var3[i], mean = eq_y5[3]),
               rnorm(cl_5_v_30_sample$Var4[i], mean = eq_y5[4]),
               rnorm(cl_5_v_30_sample$Var5[i], mean = eq_y5[5])),
         value = c(rep("Var1", cl_5_v_30_sample$Var1[i]),
                   rep("Var2", cl_5_v_30_sample$Var2[i]),
                   rep("Var3", cl_5_v_30_sample$Var3[i]),
                   rep("Var4", cl_5_v_30_sample$Var4[i]),
                   rep("Var5", cl_5_v_30_sample$Var5[i])),
         type = "equadistant") ->
    eq
  
  tibble(x = c(rnorm(cl_5_v_30_sample$Var1[i], sd = scale),
               rnorm(cl_5_v_30_sample$Var2[i], sd = scale),
               rnorm(cl_5_v_30_sample$Var3[i], sd = scale),
               rnorm(cl_5_v_30_sample$Var4[i], sd = scale),
               rnorm(cl_5_v_30_sample$Var5[i], sd = scale)),
         y = c(rnorm(cl_5_v_30_sample$Var1[i], sd = scale),
               rnorm(cl_5_v_30_sample$Var2[i], sd = scale),
               rnorm(cl_5_v_30_sample$Var3[i], sd = scale),
               rnorm(cl_5_v_30_sample$Var4[i], sd = scale),
               rnorm(cl_5_v_30_sample$Var5[i], sd = scale)),
         value = c(rep("Var1", cl_5_v_30_sample$Var1[i]),
                   rep("Var2", cl_5_v_30_sample$Var2[i]),
                   rep("Var3", cl_5_v_30_sample$Var3[i]),
                   rep("Var4", cl_5_v_30_sample$Var4[i]),
                   rep("Var5", cl_5_v_30_sample$Var5[i])),
         type = "random") ->
    rd
  
  eq %>% 
    bind_rows(cp, rd) %>% 
    mutate(id = cl_5_v_30_sample$id[i],
           vars = str_c(cl_5_v_30_sample$Var1[i], 
                        cl_5_v_30_sample$Var2[i], 
                        cl_5_v_30_sample$Var3[i], 
                        cl_5_v_30_sample$Var4[i], 
                        cl_5_v_30_sample$Var5[i], 
                        sep = "-"),
           n_category = cl_5_v_30_sample$n_category[i],
           n_villages = cl_5_v_30_sample$n_villages[i],
           H = cl_5_v_30_sample$H[i])
}) ->
  df_cl_5_v_30


# df_cl_5_v_50 ------------------------------------------------------------
n_villages <- 50
n_category <- 5
X <- as.matrix(expand.grid(1:n_villages, 
                           1:n_villages, 
                           1:n_villages, 
                           1:n_villages)) # repeat n_clusters - 1 times
X %>% 
  as_tibble() %>% 
  mutate(rs = rowSums(.)) %>% 
  filter(rs <= n_villages) %>% 
  mutate(Var5 = n_villages-rs) %>% 
  filter(Var5 > 0) %>% 
  filter(Var1 >= Var2,
         Var1 >= Var3,
         Var1 >= Var4,
         Var1 >= Var5,
         Var2 >= Var3,
         Var2 >= Var4,
         Var2 >= Var5,
         Var3 >= Var4,
         Var3 >= Var5,
         Var4 >= Var5) %>% 
  select(-rs) %>% 
  rowwise() %>% 
  mutate(H = -sum(Var1/n_villages*log2(Var1/n_villages),
                  Var2/n_villages*log2(Var2/n_villages),
                  Var3/n_villages*log2(Var3/n_villages),
                  Var4/n_villages*log2(Var4/n_villages),
                  Var5/n_villages*log2(Var5/n_villages))) %>% 
  arrange(H) %>% 
  ungroup() %>% 
  mutate(id = 1:n(),
         n_villages = n_villages,
         n_category = n_category) %>% 
  select(id, n_category, n_villages, H, Var1, Var2, Var3, Var4, Var5) %>% 
  slice(round(seq(nrow(.), 1, length.out = 30))) ->
  cl_5_v_50_sample

map_dfr(1:30, function(i){
  tibble(x = c(rnorm(cl_5_v_50_sample$Var1[i], mean = cp_x5[1]),
               rnorm(cl_5_v_50_sample$Var2[i], mean = cp_x5[2]),
               rnorm(cl_5_v_50_sample$Var3[i], mean = cp_x5[3]),
               rnorm(cl_5_v_50_sample$Var4[i], mean = cp_x5[4]),
               rnorm(cl_5_v_50_sample$Var5[i], mean = cp_x5[5])),
         y = c(rnorm(cl_5_v_50_sample$Var1[i], mean = cp_y5[1]),
               rnorm(cl_5_v_50_sample$Var2[i], mean = cp_y5[2]),
               rnorm(cl_5_v_50_sample$Var3[i], mean = cp_y5[3]),
               rnorm(cl_5_v_50_sample$Var4[i], mean = cp_y5[4]),
               rnorm(cl_5_v_50_sample$Var5[i], mean = cp_y5[5])),
         value = c(rep("Var1", cl_5_v_50_sample$Var1[i]),
                  rep("Var2", cl_5_v_50_sample$Var2[i]),
                  rep("Var3", cl_5_v_50_sample$Var3[i]),
                  rep("Var4", cl_5_v_50_sample$Var4[i]),
                  rep("Var5", cl_5_v_50_sample$Var5[i])),
         type = "central-periphery") ->
    cp
  
  tibble(x = c(rnorm(cl_5_v_50_sample$Var1[i], mean = eq_x5[1]),
               rnorm(cl_5_v_50_sample$Var2[i], mean = eq_x5[2]),
               rnorm(cl_5_v_50_sample$Var3[i], mean = eq_x5[3]),
               rnorm(cl_5_v_50_sample$Var4[i], mean = eq_x5[4]),
               rnorm(cl_5_v_50_sample$Var5[i], mean = eq_x5[5])),
         y = c(rnorm(cl_5_v_50_sample$Var1[i], mean = eq_y5[1]),
               rnorm(cl_5_v_50_sample$Var2[i], mean = eq_y5[2]),
               rnorm(cl_5_v_50_sample$Var3[i], mean = eq_y5[3]),
               rnorm(cl_5_v_50_sample$Var4[i], mean = eq_y5[4]),
               rnorm(cl_5_v_50_sample$Var5[i], mean = eq_y5[5])),
         value = c(rep("Var1", cl_5_v_50_sample$Var1[i]),
                  rep("Var2", cl_5_v_50_sample$Var2[i]),
                  rep("Var3", cl_5_v_50_sample$Var3[i]),
                  rep("Var4", cl_5_v_50_sample$Var4[i]),
                  rep("Var5", cl_5_v_50_sample$Var5[i])),
         type = "equadistant") ->
    eq

  tibble(x = c(rnorm(cl_5_v_50_sample$Var1[i], sd = scale),
               rnorm(cl_5_v_50_sample$Var2[i], sd = scale),
               rnorm(cl_5_v_50_sample$Var3[i], sd = scale),
               rnorm(cl_5_v_50_sample$Var4[i], sd = scale),
               rnorm(cl_5_v_50_sample$Var5[i], sd = scale)),
         y = c(rnorm(cl_5_v_50_sample$Var1[i], sd = scale),
               rnorm(cl_5_v_50_sample$Var2[i], sd = scale),
               rnorm(cl_5_v_50_sample$Var3[i], sd = scale),
               rnorm(cl_5_v_50_sample$Var4[i], sd = scale),
               rnorm(cl_5_v_50_sample$Var5[i], sd = scale)),
         value = c(rep("Var1", cl_5_v_50_sample$Var1[i]),
                  rep("Var2", cl_5_v_50_sample$Var2[i]),
                  rep("Var3", cl_5_v_50_sample$Var3[i]),
                  rep("Var4", cl_5_v_50_sample$Var4[i]),
                  rep("Var5", cl_5_v_50_sample$Var5[i])),
         type = "random") ->
    rd
  
  eq %>% 
    bind_rows(cp, rd) %>% 
    mutate(id = cl_5_v_50_sample$id[i],
           vars = str_c(cl_5_v_50_sample$Var1[i], 
                        cl_5_v_50_sample$Var2[i], 
                        cl_5_v_50_sample$Var3[i], 
                        cl_5_v_50_sample$Var4[i], 
                        cl_5_v_50_sample$Var5[i], 
                        sep = "-"),
           n_category = cl_5_v_50_sample$n_category[i],
           n_villages = cl_5_v_50_sample$n_villages[i],
           H = cl_5_v_50_sample$H[i])
}) ->
  df_cl_5_v_50

# df_cl_5_v_70 ------------------------------------------------------------
n_villages <- 70
n_category <- 5
X <- as.matrix(expand.grid(1:n_villages, 
                           1:n_villages, 
                           1:n_villages, 
                           1:n_villages)) # repeat n_clusters - 1 times
X %>% 
  as_tibble() %>% 
  mutate(rs = rowSums(.)) %>% 
  filter(rs <= n_villages) %>% 
  mutate(Var5 = n_villages-rs) %>% 
  filter(Var5 > 0) %>% 
  filter(Var1 >= Var2,
         Var1 >= Var3,
         Var1 >= Var4,
         Var1 >= Var5,
         Var2 >= Var3,
         Var2 >= Var4,
         Var2 >= Var5,
         Var3 >= Var4,
         Var3 >= Var5,
         Var4 >= Var5) %>% 
  select(-rs) %>% 
  rowwise() %>% 
  mutate(H = -sum(Var1/n_villages*log2(Var1/n_villages),
                  Var2/n_villages*log2(Var2/n_villages),
                  Var3/n_villages*log2(Var3/n_villages),
                  Var4/n_villages*log2(Var4/n_villages),
                  Var5/n_villages*log2(Var5/n_villages))) %>% 
  arrange(H) %>% 
  ungroup() %>% 
  mutate(id = 1:n(),
         n_villages = n_villages,
         n_category = n_category) %>% 
  select(id, n_category, n_villages, H, Var1, Var2, Var3, Var4, Var5) %>% 
  slice(round(seq(nrow(.), 1, length.out = 70))) ->
  cl_5_v_70_sample

map_dfr(1:70, function(i){
  tibble(x = c(rnorm(cl_5_v_70_sample$Var1[i], mean = cp_x5[1]),
               rnorm(cl_5_v_70_sample$Var2[i], mean = cp_x5[2]),
               rnorm(cl_5_v_70_sample$Var3[i], mean = cp_x5[3]),
               rnorm(cl_5_v_70_sample$Var4[i], mean = cp_x5[4]),
               rnorm(cl_5_v_70_sample$Var5[i], mean = cp_x5[5])),
         y = c(rnorm(cl_5_v_70_sample$Var1[i], mean = cp_y5[1]),
               rnorm(cl_5_v_70_sample$Var2[i], mean = cp_y5[2]),
               rnorm(cl_5_v_70_sample$Var3[i], mean = cp_y5[3]),
               rnorm(cl_5_v_70_sample$Var4[i], mean = cp_y5[4]),
               rnorm(cl_5_v_70_sample$Var5[i], mean = cp_y5[5])),
         value = c(rep("Var1", cl_5_v_70_sample$Var1[i]),
                   rep("Var2", cl_5_v_70_sample$Var2[i]),
                   rep("Var3", cl_5_v_70_sample$Var3[i]),
                   rep("Var4", cl_5_v_70_sample$Var4[i]),
                   rep("Var5", cl_5_v_70_sample$Var5[i])),
         type = "central-periphery") ->
    cp
  
  tibble(x = c(rnorm(cl_5_v_70_sample$Var1[i], mean = eq_x5[1]),
               rnorm(cl_5_v_70_sample$Var2[i], mean = eq_x5[2]),
               rnorm(cl_5_v_70_sample$Var3[i], mean = eq_x5[3]),
               rnorm(cl_5_v_70_sample$Var4[i], mean = eq_x5[4]),
               rnorm(cl_5_v_70_sample$Var5[i], mean = eq_x5[5])),
         y = c(rnorm(cl_5_v_70_sample$Var1[i], mean = eq_y5[1]),
               rnorm(cl_5_v_70_sample$Var2[i], mean = eq_y5[2]),
               rnorm(cl_5_v_70_sample$Var3[i], mean = eq_y5[3]),
               rnorm(cl_5_v_70_sample$Var4[i], mean = eq_y5[4]),
               rnorm(cl_5_v_70_sample$Var5[i], mean = eq_y5[5])),
         value = c(rep("Var1", cl_5_v_70_sample$Var1[i]),
                   rep("Var2", cl_5_v_70_sample$Var2[i]),
                   rep("Var3", cl_5_v_70_sample$Var3[i]),
                   rep("Var4", cl_5_v_70_sample$Var4[i]),
                   rep("Var5", cl_5_v_70_sample$Var5[i])),
         type = "equadistant") ->
    eq
  
  tibble(x = c(rnorm(cl_5_v_70_sample$Var1[i], sd = scale),
               rnorm(cl_5_v_70_sample$Var2[i], sd = scale),
               rnorm(cl_5_v_70_sample$Var3[i], sd = scale),
               rnorm(cl_5_v_70_sample$Var4[i], sd = scale),
               rnorm(cl_5_v_70_sample$Var5[i], sd = scale)),
         y = c(rnorm(cl_5_v_70_sample$Var1[i], sd = scale),
               rnorm(cl_5_v_70_sample$Var2[i], sd = scale),
               rnorm(cl_5_v_70_sample$Var3[i], sd = scale),
               rnorm(cl_5_v_70_sample$Var4[i], sd = scale),
               rnorm(cl_5_v_70_sample$Var5[i], sd = scale)),
         value = c(rep("Var1", cl_5_v_70_sample$Var1[i]),
                   rep("Var2", cl_5_v_70_sample$Var2[i]),
                   rep("Var3", cl_5_v_70_sample$Var3[i]),
                   rep("Var4", cl_5_v_70_sample$Var4[i]),
                   rep("Var5", cl_5_v_70_sample$Var5[i])),
         type = "random") ->
    rd
  
  eq %>% 
    bind_rows(cp, rd) %>% 
    mutate(id = cl_5_v_70_sample$id[i],
           vars = str_c(cl_5_v_70_sample$Var1[i], 
                        cl_5_v_70_sample$Var2[i], 
                        cl_5_v_70_sample$Var3[i], 
                        cl_5_v_70_sample$Var4[i], 
                        cl_5_v_70_sample$Var5[i], 
                        sep = "-"),
           n_category = cl_5_v_70_sample$n_category[i],
           n_villages = cl_5_v_70_sample$n_villages[i],
           H = cl_5_v_70_sample$H[i])
}) ->
  df_cl_5_v_70

# df_cl_4_v_30 ------------------------------------------------------------
n_villages <- 30
n_category <- 4
X <- as.matrix(expand.grid(1:n_villages, 
                           1:n_villages, 
                           1:n_villages)) # repeat n_clusters - 1 times
X %>% 
  as_tibble() %>% 
  mutate(rs = rowSums(.)) %>% 
  filter(rs <= n_villages) %>% 
  mutate(Var4 = n_villages-rs) %>% 
  filter(Var4 > 0) %>% 
  filter(Var1 >= Var2,
         Var1 >= Var3,
         Var1 >= Var4,
         Var2 >= Var3,
         Var2 >= Var4,
         Var3 >= Var4) %>% 
  select(-rs) %>% 
  rowwise() %>% 
  mutate(H = -sum(Var1/n_villages*log2(Var1/n_villages),
                  Var2/n_villages*log2(Var2/n_villages),
                  Var3/n_villages*log2(Var3/n_villages),
                  Var4/n_villages*log2(Var4/n_villages))) %>% 
  arrange(H) %>% 
  ungroup() %>% 
  mutate(id = 1:n(),
         n_villages = n_villages,
         n_category = n_category) %>% 
  select(id, n_category, n_villages, H, Var1, Var2, Var3, Var4) %>% 
  slice(round(seq(nrow(.), 1, length.out = 30))) ->
  cl_4_v_30_sample

scale <- 1
eq_x4 = c(1, 0, -1, 0)* scale
eq_y4 = c(0, 1, 0, -1)* scale
cp_x4 <- c(0, (2*cos(2*pi/3*0:2))) * scale
cp_y4 <- c(0, (2*sin(2*pi/3*0:2))) * scale

map_dfr(1:30, function(i){
  tibble(x = c(rnorm(cl_4_v_30_sample$Var1[i], mean = eq_x4[1]),
               rnorm(cl_4_v_30_sample$Var2[i], mean = eq_x4[2]),
               rnorm(cl_4_v_30_sample$Var3[i], mean = eq_x4[3]),
               rnorm(cl_4_v_30_sample$Var4[i], mean = eq_x4[4])),
         y = c(rnorm(cl_4_v_30_sample$Var1[i], mean = eq_y4[1]),
               rnorm(cl_4_v_30_sample$Var2[i], mean = eq_y4[2]),
               rnorm(cl_4_v_30_sample$Var3[i], mean = eq_y4[3]),
               rnorm(cl_4_v_30_sample$Var4[i], mean = eq_y4[4])),
         value = c(rep("Var1", cl_4_v_30_sample$Var1[i]),
                   rep("Var2", cl_4_v_30_sample$Var2[i]),
                   rep("Var3", cl_4_v_30_sample$Var3[i]),
                   rep("Var4", cl_4_v_30_sample$Var4[i])),
         type = "equadistant") ->
    eq
  
  tibble(x = c(rnorm(cl_4_v_30_sample$Var1[i], mean = cp_x4[1]),
               rnorm(cl_4_v_30_sample$Var2[i], mean = cp_x4[2]),
               rnorm(cl_4_v_30_sample$Var3[i], mean = cp_x4[3]),
               rnorm(cl_4_v_30_sample$Var4[i], mean = cp_x4[4])),
         y = c(rnorm(cl_4_v_30_sample$Var1[i], mean = cp_y4[1]),
               rnorm(cl_4_v_30_sample$Var2[i], mean = cp_y4[2]),
               rnorm(cl_4_v_30_sample$Var3[i], mean = cp_y4[3]),
               rnorm(cl_4_v_30_sample$Var4[i], mean = cp_y4[4])),
         value = c(rep("Var1", cl_4_v_30_sample$Var1[i]),
                   rep("Var2", cl_4_v_30_sample$Var2[i]),
                   rep("Var3", cl_4_v_30_sample$Var3[i]),
                   rep("Var4", cl_4_v_30_sample$Var4[i])),
         type = "central-periphery") ->
    cp
  
  tibble(x = c(rnorm(cl_4_v_30_sample$Var1[i], sd = scale),
               rnorm(cl_4_v_30_sample$Var2[i], sd = scale),
               rnorm(cl_4_v_30_sample$Var3[i], sd = scale),
               rnorm(cl_4_v_30_sample$Var4[i], sd = scale)),
         y = c(rnorm(cl_4_v_30_sample$Var1[i], sd = scale),
               rnorm(cl_4_v_30_sample$Var2[i], sd = scale),
               rnorm(cl_4_v_30_sample$Var3[i], sd = scale),
               rnorm(cl_4_v_30_sample$Var4[i], sd = scale)),
         value = c(rep("Var1", cl_4_v_30_sample$Var1[i]),
                   rep("Var2", cl_4_v_30_sample$Var2[i]),
                   rep("Var3", cl_4_v_30_sample$Var3[i]),
                   rep("Var4", cl_4_v_30_sample$Var4[i])),
         type = "random") ->
    rd
  
  eq %>% 
    bind_rows(cp, rd) %>% 
    mutate(id = cl_4_v_30_sample$id[i],
           vars = str_c(cl_4_v_30_sample$Var1[i], 
                        cl_4_v_30_sample$Var2[i], 
                        cl_4_v_30_sample$Var3[i], 
                        cl_4_v_30_sample$Var4[i], 
                        sep = "-"),
           n_category = cl_4_v_30_sample$n_category[i],
           n_villages = cl_4_v_30_sample$n_villages[i],
           H = cl_4_v_30_sample$H[i])
}) ->
  df_cl_4_v_30


# df_cl_4_v_50 ------------------------------------------------------------
n_villages <- 50
n_category <- 4
X <- as.matrix(expand.grid(1:n_villages, 
                           1:n_villages, 
                           1:n_villages)) # repeat n_clusters - 1 times
X %>% 
  as_tibble() %>% 
  mutate(rs = rowSums(.)) %>% 
  filter(rs <= n_villages) %>% 
  mutate(Var4 = n_villages-rs) %>% 
  filter(Var4 > 0) %>% 
  filter(Var1 >= Var2,
         Var1 >= Var3,
         Var1 >= Var4,
         Var2 >= Var3,
         Var2 >= Var4,
         Var3 >= Var4) %>% 
  select(-rs) %>% 
  rowwise() %>% 
  mutate(H = -sum(Var1/n_villages*log2(Var1/n_villages),
                  Var2/n_villages*log2(Var2/n_villages),
                  Var3/n_villages*log2(Var3/n_villages),
                  Var4/n_villages*log2(Var4/n_villages))) %>% 
  arrange(H) %>% 
  ungroup() %>% 
  mutate(id = 1:n(),
         n_villages = n_villages,
         n_category = n_category) %>% 
  select(id, n_category, n_villages, H, Var1, Var2, Var3, Var4) %>% 
  slice(round(seq(nrow(.), 1, length.out = 30))) ->
  cl_4_v_50_sample

map_dfr(1:30, function(i){
  tibble(x = c(rnorm(cl_4_v_50_sample$Var1[i], mean = eq_x4[1]),
               rnorm(cl_4_v_50_sample$Var2[i], mean = eq_x4[2]),
               rnorm(cl_4_v_50_sample$Var3[i], mean = eq_x4[3]),
               rnorm(cl_4_v_50_sample$Var4[i], mean = eq_x4[4])),
         y = c(rnorm(cl_4_v_50_sample$Var1[i], mean = eq_y4[1]),
               rnorm(cl_4_v_50_sample$Var2[i], mean = eq_y4[2]),
               rnorm(cl_4_v_50_sample$Var3[i], mean = eq_y4[3]),
               rnorm(cl_4_v_50_sample$Var4[i], mean = eq_y4[4])),
         value = c(rep("Var1", cl_4_v_50_sample$Var1[i]),
                   rep("Var2", cl_4_v_50_sample$Var2[i]),
                   rep("Var3", cl_4_v_50_sample$Var3[i]),
                   rep("Var4", cl_4_v_50_sample$Var4[i])),
         type = "equadistant") ->
    eq
  
  tibble(x = c(rnorm(cl_4_v_50_sample$Var1[i], mean = cp_x4[1]),
               rnorm(cl_4_v_50_sample$Var2[i], mean = cp_x4[2]),
               rnorm(cl_4_v_50_sample$Var3[i], mean = cp_x4[3]),
               rnorm(cl_4_v_50_sample$Var4[i], mean = cp_x4[4])),
         y = c(rnorm(cl_4_v_50_sample$Var1[i], mean = cp_y4[1]),
               rnorm(cl_4_v_50_sample$Var2[i], mean = cp_y4[2]),
               rnorm(cl_4_v_50_sample$Var3[i], mean = cp_y4[3]),
               rnorm(cl_4_v_50_sample$Var4[i], mean = cp_y4[4])),
         value = c(rep("Var1", cl_4_v_50_sample$Var1[i]),
                   rep("Var2", cl_4_v_50_sample$Var2[i]),
                   rep("Var3", cl_4_v_50_sample$Var3[i]),
                   rep("Var4", cl_4_v_50_sample$Var4[i])),
         type = "central-periphery") ->
    cp
  
  tibble(x = c(rnorm(cl_4_v_50_sample$Var1[i], sd = scale),
               rnorm(cl_4_v_50_sample$Var2[i], sd = scale),
               rnorm(cl_4_v_50_sample$Var3[i], sd = scale),
               rnorm(cl_4_v_50_sample$Var4[i], sd = scale)),
         y = c(rnorm(cl_4_v_50_sample$Var1[i], sd = scale),
               rnorm(cl_4_v_50_sample$Var2[i], sd = scale),
               rnorm(cl_4_v_50_sample$Var3[i], sd = scale),
               rnorm(cl_4_v_50_sample$Var4[i], sd = scale)),
         value = c(rep("Var1", cl_4_v_50_sample$Var1[i]),
                   rep("Var2", cl_4_v_50_sample$Var2[i]),
                   rep("Var3", cl_4_v_50_sample$Var3[i]),
                   rep("Var4", cl_4_v_50_sample$Var4[i])),
         type = "random") ->
    rd
  
  eq %>% 
    bind_rows(cp, rd) %>% 
    mutate(id = cl_4_v_50_sample$id[i],
           vars = str_c(cl_4_v_50_sample$Var1[i], 
                        cl_4_v_50_sample$Var2[i], 
                        cl_4_v_50_sample$Var3[i], 
                        cl_4_v_50_sample$Var4[i], 
                        sep = "-"),
           n_category = cl_4_v_50_sample$n_category[i],
           n_villages = cl_4_v_50_sample$n_villages[i],
           H = cl_4_v_50_sample$H[i])
}) ->
  df_cl_4_v_50

theme_set(theme_bw())

# df_cl_4_v_70 ------------------------------------------------------------
n_villages <- 70
n_category <- 4
X <- as.matrix(expand.grid(1:n_villages, 
                           1:n_villages, 
                           1:n_villages)) # repeat n_clusters - 1 times
X %>% 
  as_tibble() %>% 
  mutate(rs = rowSums(.)) %>% 
  filter(rs <= n_villages) %>% 
  mutate(Var4 = n_villages-rs) %>% 
  filter(Var4 > 0) %>% 
  filter(Var1 >= Var2,
         Var1 >= Var3,
         Var1 >= Var4,
         Var2 >= Var3,
         Var2 >= Var4,
         Var3 >= Var4) %>% 
  select(-rs) %>% 
  rowwise() %>% 
  mutate(H = -sum(Var1/n_villages*log2(Var1/n_villages),
                  Var2/n_villages*log2(Var2/n_villages),
                  Var3/n_villages*log2(Var3/n_villages),
                  Var4/n_villages*log2(Var4/n_villages))) %>% 
  arrange(H) %>% 
  ungroup() %>% 
  mutate(id = 1:n(),
         n_villages = n_villages,
         n_category = n_category) %>% 
  select(id, n_category, n_villages, H, Var1, Var2, Var3, Var4) %>% 
  slice(round(seq(nrow(.), 1, length.out = 70))) ->
  cl_4_v_70_sample

map_dfr(1:70, function(i){
  tibble(x = c(rnorm(cl_4_v_70_sample$Var1[i], mean = eq_x4[1]),
               rnorm(cl_4_v_70_sample$Var2[i], mean = eq_x4[2]),
               rnorm(cl_4_v_70_sample$Var3[i], mean = eq_x4[3]),
               rnorm(cl_4_v_70_sample$Var4[i], mean = eq_x4[4])),
         y = c(rnorm(cl_4_v_70_sample$Var1[i], mean = eq_y4[1]),
               rnorm(cl_4_v_70_sample$Var2[i], mean = eq_y4[2]),
               rnorm(cl_4_v_70_sample$Var3[i], mean = eq_y4[3]),
               rnorm(cl_4_v_70_sample$Var4[i], mean = eq_y4[4])),
         value = c(rep("Var1", cl_4_v_70_sample$Var1[i]),
                   rep("Var2", cl_4_v_70_sample$Var2[i]),
                   rep("Var3", cl_4_v_70_sample$Var3[i]),
                   rep("Var4", cl_4_v_70_sample$Var4[i])),
         type = "equadistant") ->
    eq
  
  tibble(x = c(rnorm(cl_4_v_70_sample$Var1[i], mean = cp_x4[1]),
               rnorm(cl_4_v_70_sample$Var2[i], mean = cp_x4[2]),
               rnorm(cl_4_v_70_sample$Var3[i], mean = cp_x4[3]),
               rnorm(cl_4_v_70_sample$Var4[i], mean = cp_x4[4])),
         y = c(rnorm(cl_4_v_70_sample$Var1[i], mean = cp_y4[1]),
               rnorm(cl_4_v_70_sample$Var2[i], mean = cp_y4[2]),
               rnorm(cl_4_v_70_sample$Var3[i], mean = cp_y4[3]),
               rnorm(cl_4_v_70_sample$Var4[i], mean = cp_y4[4])),
         value = c(rep("Var1", cl_4_v_70_sample$Var1[i]),
                   rep("Var2", cl_4_v_70_sample$Var2[i]),
                   rep("Var3", cl_4_v_70_sample$Var3[i]),
                   rep("Var4", cl_4_v_70_sample$Var4[i])),
         type = "central-periphery") ->
    cp
  
  tibble(x = c(rnorm(cl_4_v_70_sample$Var1[i], sd = scale),
               rnorm(cl_4_v_70_sample$Var2[i], sd = scale),
               rnorm(cl_4_v_70_sample$Var3[i], sd = scale),
               rnorm(cl_4_v_70_sample$Var4[i], sd = scale)),
         y = c(rnorm(cl_4_v_70_sample$Var1[i], sd = scale),
               rnorm(cl_4_v_70_sample$Var2[i], sd = scale),
               rnorm(cl_4_v_70_sample$Var3[i], sd = scale),
               rnorm(cl_4_v_70_sample$Var4[i], sd = scale)),
         value = c(rep("Var1", cl_4_v_70_sample$Var1[i]),
                   rep("Var2", cl_4_v_70_sample$Var2[i]),
                   rep("Var3", cl_4_v_70_sample$Var3[i]),
                   rep("Var4", cl_4_v_70_sample$Var4[i])),
         type = "random") ->
    rd
  
  eq %>% 
    bind_rows(cp, rd) %>% 
    mutate(id = cl_4_v_70_sample$id[i],
           vars = str_c(cl_4_v_70_sample$Var1[i], 
                        cl_4_v_70_sample$Var2[i], 
                        cl_4_v_70_sample$Var3[i], 
                        cl_4_v_70_sample$Var4[i], 
                        sep = "-"),
           n_category = cl_4_v_70_sample$n_category[i],
           n_villages = cl_4_v_70_sample$n_villages[i],
           H = cl_4_v_70_sample$H[i])
}) ->
  df_cl_4_v_70

# df_cl_3_v_30 ------------------------------------------------------------
n_villages <- 30
n_category <- 3
X <- as.matrix(expand.grid(1:n_villages, 
                           1:n_villages)) # repeat n_clusters - 1 times
X %>% 
  as_tibble() %>% 
  mutate(rs = rowSums(.)) %>% 
  filter(rs <= n_villages) %>% 
  mutate(Var3 = n_villages-rs) %>% 
  filter(Var3 > 0) %>% 
  filter(Var1 >= Var2,
         Var1 >= Var3,
         Var2 >= Var3) %>% 
  select(-rs) %>% 
  rowwise() %>% 
  mutate(H = -sum(Var1/n_villages*log2(Var1/n_villages),
                  Var2/n_villages*log2(Var2/n_villages),
                  Var3/n_villages*log2(Var3/n_villages))) %>% 
  arrange(H) %>% 
  ungroup() %>% 
  mutate(id = 1:n(),
         n_villages = n_villages,
         n_category = n_category) %>% 
  select(id, n_category, n_villages, H, Var1, Var2, Var3) %>% 
  slice(round(seq(nrow(.), 1, length.out = 30))) ->
  cl_3_v_30_sample

scale <- 1
cp_x3 = c(0, 1, -1)* scale*3/2
cp_y3 = c(0, 0, 0)* scale*3/2
eq_x3 <- (2*cos(2*pi/3*0:2)) * scale*2/3
eq_y3 <- (2*sin(2*pi/3*0:3)) * scale*2/3

map_dfr(1:30, function(i){
  tibble(x = c(rnorm(cl_3_v_30_sample$Var1[i], mean = eq_x3[1]),
               rnorm(cl_3_v_30_sample$Var2[i], mean = eq_x3[2]),
               rnorm(cl_3_v_30_sample$Var3[i], mean = eq_x3[3])),
         y = c(rnorm(cl_3_v_30_sample$Var1[i], mean = eq_y3[1]),
               rnorm(cl_3_v_30_sample$Var2[i], mean = eq_y3[2]),
               rnorm(cl_3_v_30_sample$Var3[i], mean = eq_y3[3])),
         value = c(rep("Var1", cl_3_v_30_sample$Var1[i]),
                   rep("Var2", cl_3_v_30_sample$Var2[i]),
                   rep("Var3", cl_3_v_30_sample$Var3[i])),
         type = "equadistant") ->
    eq
  
  tibble(x = c(rnorm(cl_3_v_30_sample$Var1[i], mean = cp_x3[1]),
               rnorm(cl_3_v_30_sample$Var2[i], mean = cp_x3[2]),
               rnorm(cl_3_v_30_sample$Var3[i], mean = cp_x3[3])),
         y = c(rnorm(cl_3_v_30_sample$Var1[i], mean = cp_y3[1]),
               rnorm(cl_3_v_30_sample$Var2[i], mean = cp_y3[2]),
               rnorm(cl_3_v_30_sample$Var3[i], mean = cp_y3[3])),
         value = c(rep("Var1", cl_3_v_30_sample$Var1[i]),
                   rep("Var2", cl_3_v_30_sample$Var2[i]),
                   rep("Var3", cl_3_v_30_sample$Var3[i])),
         type = "central-periphery") ->
    cp
  
  tibble(x = c(rnorm(cl_3_v_30_sample$Var1[i], sd = scale),
               rnorm(cl_3_v_30_sample$Var2[i], sd = scale),
               rnorm(cl_3_v_30_sample$Var3[i], sd = scale)),
         y = c(rnorm(cl_3_v_30_sample$Var1[i], sd = scale),
               rnorm(cl_3_v_30_sample$Var2[i], sd = scale),
               rnorm(cl_3_v_30_sample$Var3[i], sd = scale)),
         value = c(rep("Var1", cl_3_v_30_sample$Var1[i]),
                   rep("Var2", cl_3_v_30_sample$Var2[i]),
                   rep("Var3", cl_3_v_30_sample$Var3[i])),
         type = "random") ->
    rd
  
  eq %>% 
    bind_rows(cp, rd) %>% 
    mutate(id = cl_3_v_30_sample$id[i],
           vars = str_c(cl_3_v_30_sample$Var1[i], 
                        cl_3_v_30_sample$Var2[i], 
                        cl_3_v_30_sample$Var3[i], 
                        sep = "-"),
           n_category = cl_3_v_30_sample$n_category[i],
           n_villages = cl_3_v_30_sample$n_villages[i],
           H = cl_3_v_30_sample$H[i])
}) ->
  df_cl_3_v_30


# df_cl_3_v_50 ------------------------------------------------------------
n_villages <- 50
n_category <- 3
X <- as.matrix(expand.grid(1:n_villages, 
                           1:n_villages)) # repeat n_clusters - 1 times
X %>% 
  as_tibble() %>% 
  mutate(rs = rowSums(.)) %>% 
  filter(rs <= n_villages) %>% 
  mutate(Var3 = n_villages-rs) %>% 
  filter(Var3 > 0) %>% 
  filter(Var1 >= Var2,
         Var1 >= Var3,
         Var2 >= Var3) %>% 
  select(-rs) %>% 
  rowwise() %>% 
  mutate(H = -sum(Var1/n_villages*log2(Var1/n_villages),
                  Var2/n_villages*log2(Var2/n_villages),
                  Var3/n_villages*log2(Var3/n_villages))) %>% 
  arrange(H) %>% 
  ungroup() %>% 
  mutate(id = 1:n(),
         n_villages = n_villages,
         n_category = n_category) %>% 
  select(id, n_category, n_villages, H, Var1, Var2, Var3) %>% 
  slice(round(seq(nrow(.), 1, length.out = 30))) ->
  cl_3_v_50_sample

map_dfr(1:30, function(i){
  tibble(x = c(rnorm(cl_3_v_50_sample$Var1[i], mean = eq_x3[1]),
               rnorm(cl_3_v_50_sample$Var2[i], mean = eq_x3[2]),
               rnorm(cl_3_v_50_sample$Var3[i], mean = eq_x3[3])),
         y = c(rnorm(cl_3_v_50_sample$Var1[i], mean = eq_y3[1]),
               rnorm(cl_3_v_50_sample$Var2[i], mean = eq_y3[2]),
               rnorm(cl_3_v_50_sample$Var3[i], mean = eq_y3[3])),
         value = c(rep("Var1", cl_3_v_50_sample$Var1[i]),
                   rep("Var2", cl_3_v_50_sample$Var2[i]),
                   rep("Var3", cl_3_v_50_sample$Var3[i])),
         type = "equadistant") ->
    eq
  
  tibble(x = c(rnorm(cl_3_v_50_sample$Var1[i], mean = cp_x3[1]),
               rnorm(cl_3_v_50_sample$Var2[i], mean = cp_x3[2]),
               rnorm(cl_3_v_50_sample$Var3[i], mean = cp_x3[3])),
         y = c(rnorm(cl_3_v_50_sample$Var1[i], mean = cp_y3[1]),
               rnorm(cl_3_v_50_sample$Var2[i], mean = cp_y3[2]),
               rnorm(cl_3_v_50_sample$Var3[i], mean = cp_y3[3])),
         value = c(rep("Var1", cl_3_v_50_sample$Var1[i]),
                   rep("Var2", cl_3_v_50_sample$Var2[i]),
                   rep("Var3", cl_3_v_50_sample$Var3[i])),
         type = "central-periphery") ->
    cp
  
  tibble(x = c(rnorm(cl_3_v_50_sample$Var1[i], sd = scale),
               rnorm(cl_3_v_50_sample$Var2[i], sd = scale),
               rnorm(cl_3_v_50_sample$Var3[i], sd = scale)),
         y = c(rnorm(cl_3_v_50_sample$Var1[i], sd = scale),
               rnorm(cl_3_v_50_sample$Var2[i], sd = scale),
               rnorm(cl_3_v_50_sample$Var3[i], sd = scale)),
         value = c(rep("Var1", cl_3_v_50_sample$Var1[i]),
                   rep("Var2", cl_3_v_50_sample$Var2[i]),
                   rep("Var3", cl_3_v_50_sample$Var3[i])),
         type = "random") ->
    rd
  
  eq %>% 
    bind_rows(cp, rd) %>% 
    mutate(id = cl_3_v_30_sample$id[i],
           vars = str_c(cl_3_v_50_sample$Var1[i], 
                        cl_3_v_50_sample$Var2[i], 
                        cl_3_v_50_sample$Var3[i], 
                        sep = "-"),
           n_category = cl_3_v_50_sample$n_category[i],
           n_villages = cl_3_v_50_sample$n_villages[i],
           H = cl_3_v_30_sample$H[i])
}) ->
  df_cl_3_v_50

theme_set(theme_bw())

# df_cl_3_v_70 ------------------------------------------------------------
n_villages <- 70
n_category <- 3
X <- as.matrix(expand.grid(1:n_villages, 
                           1:n_villages)) # repeat n_clusters - 1 times
X %>% 
  as_tibble() %>% 
  mutate(rs = rowSums(.)) %>% 
  filter(rs <= n_villages) %>% 
  mutate(Var3 = n_villages-rs) %>% 
  filter(Var3 > 0) %>% 
  filter(Var1 >= Var2,
         Var1 >= Var3,
         Var2 >= Var3) %>% 
  select(-rs) %>% 
  rowwise() %>% 
  mutate(H = -sum(Var1/n_villages*log2(Var1/n_villages),
                  Var2/n_villages*log2(Var2/n_villages),
                  Var3/n_villages*log2(Var3/n_villages))) %>% 
  arrange(H) %>% 
  ungroup() %>% 
  mutate(id = 1:n(),
         n_villages = n_villages,
         n_category = n_category) %>% 
  select(id, n_category, n_villages, H, Var1, Var2, Var3) %>% 
  slice(round(seq(nrow(.), 1, length.out = 70))) ->
  cl_3_v_70_sample

map_dfr(1:70, function(i){
  tibble(x = c(rnorm(cl_3_v_70_sample$Var1[i], mean = eq_x3[1]),
               rnorm(cl_3_v_70_sample$Var2[i], mean = eq_x3[2]),
               rnorm(cl_3_v_70_sample$Var3[i], mean = eq_x3[3])),
         y = c(rnorm(cl_3_v_70_sample$Var1[i], mean = eq_y3[1]),
               rnorm(cl_3_v_70_sample$Var2[i], mean = eq_y3[2]),
               rnorm(cl_3_v_70_sample$Var3[i], mean = eq_y3[3])),
         value = c(rep("Var1", cl_3_v_70_sample$Var1[i]),
                   rep("Var2", cl_3_v_70_sample$Var2[i]),
                   rep("Var3", cl_3_v_70_sample$Var3[i])),
         type = "equadistant") ->
    eq
  
  tibble(x = c(rnorm(cl_3_v_70_sample$Var1[i], mean = cp_x3[1]),
               rnorm(cl_3_v_70_sample$Var2[i], mean = cp_x3[2]),
               rnorm(cl_3_v_70_sample$Var3[i], mean = cp_x3[3])),
         y = c(rnorm(cl_3_v_70_sample$Var1[i], mean = cp_y3[1]),
               rnorm(cl_3_v_70_sample$Var2[i], mean = cp_y3[2]),
               rnorm(cl_3_v_70_sample$Var3[i], mean = cp_y3[3])),
         value = c(rep("Var1", cl_3_v_70_sample$Var1[i]),
                   rep("Var2", cl_3_v_70_sample$Var2[i]),
                   rep("Var3", cl_3_v_70_sample$Var3[i])),
         type = "central-periphery") ->
    cp
  
  tibble(x = c(rnorm(cl_3_v_70_sample$Var1[i], sd = scale),
               rnorm(cl_3_v_70_sample$Var2[i], sd = scale),
               rnorm(cl_3_v_70_sample$Var3[i], sd = scale)),
         y = c(rnorm(cl_3_v_70_sample$Var1[i], sd = scale),
               rnorm(cl_3_v_70_sample$Var2[i], sd = scale),
               rnorm(cl_3_v_70_sample$Var3[i], sd = scale)),
         value = c(rep("Var1", cl_3_v_70_sample$Var1[i]),
                   rep("Var2", cl_3_v_70_sample$Var2[i]),
                   rep("Var3", cl_3_v_70_sample$Var3[i])),
         type = "random") ->
    rd
  
  eq %>% 
    bind_rows(cp, rd) %>% 
    mutate(id = cl_3_v_70_sample$id[i],
           vars = str_c(cl_3_v_70_sample$Var1[i], 
                        cl_3_v_70_sample$Var2[i], 
                        cl_3_v_70_sample$Var3[i], 
                        sep = "-"),
           n_category = cl_3_v_70_sample$n_category[i],
           n_villages = cl_3_v_70_sample$n_villages[i],
           H = cl_3_v_30_sample$H[i])
}) ->
  df_cl_3_v_70


# merge all  --------------------------------------------------------------
df_cl_5_v_70 %>% 
  bind_rows(df_cl_5_v_50, df_cl_5_v_30, 
            df_cl_4_v_70, df_cl_4_v_50, df_cl_4_v_30,
            df_cl_3_v_70, df_cl_3_v_50, df_cl_3_v_30) %>% 
  write_csv("/home/agricolamz/work/materials/2020_SLE_Koile_Moroz_geosampling/data/generated_data.csv")

rm(list = ls())

df_cl <- read_csv("/home/agricolamz/work/materials/2020_SLE_Koile_Moroz_geosampling/data/generated_data.csv")

df_cl %>% 
  count(type, n_category, n_villages, vars) %>% 
  mutate(dataset_id  = 1:n()) %>% 
  left_join(df_cl[, -5]) %>% 
  select(dataset_id, type, n_category, n_villages, vars, x, y, value, H) %>% 
  write_csv("/home/agricolamz/work/materials/2020_SLE_Koile_Moroz_geosampling/data/generated_data.csv")

# analyze -----------------------------------------------------------------
df_cl <- read_csv("/home/agricolamz/work/materials/2020_SLE_Koile_Moroz_geosampling/data/generated_data.csv")

set.seed(42)
map_dfr(seq(0.05, 0.9, 0.05), function(i){
  df_cl %>% 
    group_by(dataset_id) %>% 
    mutate(kmeans_cluster = kmeans(as.matrix(tibble(x, y)), centers = unique(n_villages*i))$cluster) %>% 
    group_by(dataset_id, kmeans_cluster) %>% 
    sample_n(1) %>% 
    mutate(proportion_of_village = i,
           cluster_type = "k-means") %>% 
    ungroup() %>% 
    select(-kmeans_cluster)
}) ->
  kmeans_result

set.seed(42)
map_dfr(seq(0.05, 0.9, 0.05), function(i){
  df_cl %>% 
    group_by(dataset_id) %>% 
    mutate(hclust_cluster = cutree(hclust(dist(tibble(x, y))), k = unique(n_villages*i))) %>% 
    group_by(dataset_id, hclust_cluster) %>% 
    sample_n(1) %>% 
    mutate(proportion_of_village = i,
           cluster_type = "hierarchical clustering") %>% 
    ungroup() %>% 
    select(-hclust_cluster)
}) ->
  hclust_result

set.seed(42)
map_dfr(seq(0.05, 0.9, 0.05), function(i){
  df_cl %>% 
    group_by(dataset_id) %>% 
    sample_n(unique(n_villages*i)) %>% 
    mutate(proportion_of_village = i,
           cluster_type = "random")
}) ->
  random_result

random_result %>% 
  bind_rows(hclust_result, kmeans_result) %>% 
  group_by(type, n_category, n_villages, cluster_type, proportion_of_village) %>% 
  #mutate(H = H/max(H, na.rm = TRUE)) %>% 
  ungroup() %>% 
  distinct(proportion_of_village, dataset_id, type, value, H, n_category, n_villages, cluster_type) %>% 
  count(dataset_id, H, n_category, type, proportion_of_village, n_villages, cluster_type) %>% 
  mutate(ratio = n/n_category) ->
  all_results

write_csv(all_results, "data/all_results.csv")
all_results <- read_csv("data/all_results.csv")

all_results %>% 
  mutate(type = factor(type, levels = c("random", "equadistant", "central-periphery")),
         cluster_type = factor(cluster_type, levels = c("random", "k-means", "hierarchical clustering")),
         ratio_binary = ifelse(ratio == 1, 1, 0)) ->
  all_results

all_results %>%   
  ggplot(aes(proportion_of_village, ratio, color = cluster_type))+
  geom_jitter(alpha = 0.2, size = 0.7)+
  geom_smooth(se = FALSE)+
  #facet_wrap(~n_category)
  facet_grid(n_category+n_villages~type)

fit_1 <- glm(ratio_binary ~ (type+cluster_type)*proportion_of_village, 
             family = "binomial",
             data=all_results)

summary(fit_1)

library(ggeffects)
ggeffect(fit_1, terms = c("proportion_of_village", "cluster_type", "type")) %>% 
  plot()



all_results %>%   
  ggplot(aes(H, ratio, linetype = factor(n_category), color = cluster_type))+
  #geom_jitter(alpha = 0.2, size = 0.7)+
  geom_smooth(se = FALSE)
  #facet_wrap(~n_category)
  facet_grid(cluster_type~type)


fit_2 <- glm(ratio_binary ~ (type+cluster_type)*H, 
             family = "binomial",
             data=all_results)

summary(fit_2)

library(ggeffects)
ggeffect(fit_2, terms = c("H", "cluster_type", "type")) %>% 
  plot()


random_result %>% 
  bind_rows(hclust_result, kmeans_result) %>% 
  group_by(type, n_category, n_villages, cluster_type, proportion_of_village, vars) %>% 
  ungroup() %>% 
  distinct(proportion_of_village, dataset_id, type, value, H, n_category, n_villages, cluster_type, vars) %>% 
  count(dataset_id, H, n_category, type, proportion_of_village, n_villages, cluster_type, vars) %>% 
  mutate(ratio = n/n_category) %>% 
  select(-n) %>% 
  pivot_wider(values_from = ratio, names_from = cluster_type) %>% 
  mutate(hc = `hierarchical clustering` - random,
         km = `k-means` - random) %>%
  pivot_longer(values_to = "value", names_to = "cluster_type", hc:km) %>% 
  ggplot(aes(proportion_of_village, value, color = cluster_type))+
  geom_smooth(se = TRUE)+
  xlim(0.05, 0.6)+
  facet_grid(n_villages+n_category~type)+
  geom_hline(yintercept = 0, linetype = 2)



# circassian data ---------------------------------------------------------

library(lingtypology)

circassian %>% 
  mutate(dialect = ifelse(dialect == "Abadzex", "Temirgoy", dialect),
         dialect = ifelse(dialect == "Xakuch", "Shapsug", dialect)) -> 
  new_circassian

N <- nrow(circassian)

map_dfr(1:100, function(i){
  circassian %>% 
    mutate(dataset_id = i)
}) ->
  circassian_100

set.seed(42)
map_dfr(seq(0.05, 0.9, 0.01), function(i){
  circassian_100 %>% 
    group_by(dataset_id) %>% 
    mutate(kmeans_cluster = kmeans(as.matrix(tibble(latitude, longitude)), 
                                   centers = N*i)$cluster) %>% 
    group_by(dataset_id, kmeans_cluster) %>% 
    sample_n(1) %>% 
    mutate(proportion_of_village = i,
           cluster_type = "k-means") %>% 
    ungroup() %>% 
    select(-kmeans_cluster)
}) ->
  kmeans_result

set.seed(42)
map_dfr(seq(0.05, 0.9, 0.01), function(i){
  circassian_100 %>% 
    group_by(dataset_id) %>% 
    mutate(hclust_cluster = cutree(hclust(dist(tibble(latitude, longitude))), k = N*i)) %>% 
    group_by(dataset_id, hclust_cluster) %>% 
    sample_n(1) %>% 
    mutate(proportion_of_village = i,
           cluster_type = "hierarchical clustering") %>% 
    ungroup() %>% 
    select(-hclust_cluster)
}) ->
  hclust_result

set.seed(42)
map_dfr(seq(0.05, 0.9, 0.01), function(i){
  circassian_100 %>% 
    group_by(dataset_id) %>% 
    sample_n(N*i) %>% 
    mutate(proportion_of_village = i,
           cluster_type = "random")
}) ->
  random_result

random_result %>% 
  bind_rows(hclust_result, kmeans_result) %>% 
  distinct(proportion_of_village, cluster_type, dialect) %>% 
  count(proportion_of_village, cluster_type) %>% 
  mutate(ratio = n/8) %>% 
  write_csv("data/circassian_all_results.csv")

circassian_all_results <- read_csv("data/circassian_all_results.csv")
circassian_all_results %>% 
  mutate(cluster_type = factor(cluster_type, levels = c("random", "k-means", "hierarchical clustering"))) %>% 
  ggplot(aes(proportion_of_village, ratio, color = cluster_type))+
  geom_jitter(alpha = 0.02)+
  geom_smooth(se = FALSE)
