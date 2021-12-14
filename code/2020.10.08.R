#' @param N number of groups
#' @param n number of observations within each group. It is also possible to put a vector of obervations per group (in that situation center group should go last).
#' @param r distance of the group centroid from the center that is (0, 0)
#' @param central_distance function. ...
#' @param neighbour_distance function. Function that 
#' @param center logical. Whether one group shoulbe positioned in the center

generate_equadistant <- function(N = 5, 
                                 n = 100, 
                                 r = 26,
                                 central_distance = function(r){log(r)},
                                 neighbour_distance = function(r, N){2*r*sin(pi/N)},
                                 center = FALSE){
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
    V1 <- (2-rlnorm(n = n[k], meanlog = 0, sdlog = central_distance(r)))*r
    V2 <- rnorm(n = n[k], mean = 0, sd = neighbour_distance(r, N))
    while(sum(V1 > r*1.5 | V1 < -r*1.5) > 0){
      V1 <- ifelse(V1 > r*1.5 | V1 < -r*1.5, 
                   (2-rlnorm(n = n[k], meanlog = 0, sdlog = 1))*r, 
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


library(tidyverse)

set.seed(43)
generate_equadistant(N = 5, n = c(1, 2, 3, 4, 5)*10, r = 20, 
                     central_distance = function(x){log(x)/5},
                     neighbour_distance = function(r, N){0.6*r*sin(pi/N)},                    
                     center = TRUE) %>% 
  ggplot(aes(x, y, color = id, shape = id))+
  geom_point(size = 3)+
  stat_ellipse()
