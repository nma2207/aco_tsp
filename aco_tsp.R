get_ant <- function(n)
{
  ant <- list()
  ant$current_vertex <- 0
  #visited vertexes, init value = 0
  ant$memory <- integer(n)
  return(ant)
}

#get distance of way
get_total_weight <- function(weights, way)
{
  n <- length(way)
  total <- 0
  for (i in 1:(n-1)) {
    total <- total + weights[way[i], way[i+1]]
  }
  total <- total + weights[way[1], way[n]]
  return(total)
}

get_next_vertex <- function(weights, smell_matrix, current, vertexes, alpha, betta)
{
  #vertexes are non-visited
  n <- length(vertexes)
  p <- integer(n)
  if (n==1 )
    return (vertexes[1])
  
  for (i in 1:n) {
    smell <- smell_matrix[current, vertexes[i]]
    weight <- weights[current, vertexes[i]]
    
    p[i] <- (smell^alpha)/(1/weight)^betta
  }
  p <- p/sum(p)
  #choose vertex with probs p
  #print(typeof(p))
  next_vertex <- sample(vertexes, size = 1, replace = T, prob = p)
  
  return(next_vertex)
}

get_delta_smell_matrix <- function(way, total_weight, q) {
  n <- length(way)
  delta_smell <- matrix(integer(n^2), ncol = n)
  
  for (i in 1:(n-1)) {
    v_i <- way[i]
    v_j <- way[i+1]
    #matrix shold be simmetric
    delta_smell[v_i, v_j] <- q / total_weight
    delta_smell[v_j, v_i] <- q / total_weight
  }
  v_1 <- way[1]
  v_n <- way[n]
  delta_smell[v_1, v_n] <- q / total_weight
  delta_smell[v_n, v_1] <- q / total_weight
  return (delta_smell)
}

#ant colony optimization for solving TSP 
acp_tsp <- function(weights,ant_count, iteration_count, 
                    alpha, betta, q, p, labels = NULL)
{
  #we don't check input data, we always put correct data :D
  
  n <- dim(weights)[1]
  #matrix of 1
  smell_matrix <- matrix(integer(n^2), ncol = n) + 1
  
  best_way <- 1:n
  best_total_weight <- get_total_weight(weights, best_way)
  ways <- integer(n+1)
  ways[1] <- best_total_weight
  
  for (i in 1:iteration_count) {
    
    delta_smell_matrix <- matrix(integer(n^2), ncol = n) 
    
    for (j in 1:ant_count) {
      # "create" a ant
      ant <- get_ant(n)
      #choose some start vertex
      start_vertex <- round(runif(1, 1, n))
      
      ant$current_vertex <- start_vertex
      ant$memory[1] <- start_vertex
      
      for (k in 2:n) {
        no_visited_vertexes <- setdiff(1:n, ant$memory)
        next_vertex <- get_next_vertex(weights, smell_matrix, 
                                       ant$current, no_visited_vertexes,
                                       alpha, betta)
        ant$current <- next_vertex
        ant$memory[k] <- next_vertex
        
      }
      total_weight <- get_total_weight(weights, ant$memory)
      
      #add delta smell
      delta_smell_for_ant <- get_delta_smell_matrix(ant$memory, total_weight, q)
      
      if (total_weight < best_total_weight) {
        best_total_weight <- total_weight
        best_way <- ant$memory
      }
      
    }
    #update smell
    smell_matrix <- (1-p)*smell_matrix + delta_smell_matrix
    print(paste("iteration #", i))
    print(best_total_weight)
    print('#---------------------#')
    ways[i+1]<-best_total_weight
  }
  plot(1:(iteration_count+1), ways, type = "l")
  result <- list()
  result$total_weight <- best_total_weight
  if (!is.null(labels)){
    result$way <- labels[best_way]
  }
  else {
    result$way <- best_way
  }
  return(result)
}