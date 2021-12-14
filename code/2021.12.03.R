#' @param N number of groups
#' @param n number of observations within each group. It is also possible to put a vector of obervations per group (in that situation center group should go last).
#' @param r distance of the group centroid from the center that is (0, 0)
#' @param d distance of the group centroid from the center that is (0, 0)
#' @param central_distance function that discribes the distance of observations from the centroid.
#' @param neighbour_distance function. Function that 
#' @param center logical. Whether one group should be positioned in the center

generate_equadistant <- function(N = 5, 
                                 n = 100, 
                                 r = 26,
                                 d = 26,
                                 central_distance = function(d){log(d)},
                                 neighbour_distance = function(d, N){2*d*sin(pi/N)},
                                 center = FALSE){
  require(tidyverse,quietly = TRUE)
  n <- unlist(n)
  
  if(length(n) == 1){
    n <- rep(n, N)
  }
  
  if(length(n) != N){
    stop("The number of groups (N) should be equal to the number of values in number of observations (n)")
  }
  
  if(center){
    N <- N-1
    n_center <-  n[length(n)]
    n <- n[-length(n)]
  }
  
  n <- sample(n) # in order to have random order of number of observation per groups 
  
  angle <- 2*pi/N
  lapply(1:N, function(k){
    V1 <- (2-rlnorm(n = n[k], meanlog = 0, sdlog = central_distance(d)))*d
    V2 <- rnorm(n = n[k], mean = 0, sd = neighbour_distance(d, N))
    while(sum(V1 > d*1.5 | V1 < -d*1.5) > 0){
      V1 <- ifelse(V1 > d*1.5 | V1 < -d*1.5, 
                   (2-rlnorm(n = n[k], meanlog = 0, sdlog = 1))*d, 
                   V1)  
    }
    df <- data.frame(V1, V2)
    df$V1 <- df$V1+r
    df$x <- df$V1*cos(angle*k)-df$V2*sin(angle*k)
    df$y <- df$V1*sin(angle*k)+df$V2*cos(angle*k)
    return(df[,3:4])
  }) %>% 
    do.call(rbind, .) %>% 
    as.data.frame() %>% 
    mutate(id = c(unlist(mapply(rep, 1:N, n)))) ->
    results
  
  if(center){
    data.frame(x = rnorm(n = n_center, mean = 0, sd = r*0.75),
               y = rnorm(n = n_center, mean = 0, sd = r*0.75),
               id = N + 1) %>% 
      bind_rows(results) ->
      results
  }
  results %>% 
    mutate(id = factor(id)) %>% 
    return()
}

#' @param n_villages number of villages to simulate
#' @param n_categories number of categories to simulate

all_categories_values <- function(n_villages = 30, n_categories = 5){
  require(tidyverse, quietly = TRUE)
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
