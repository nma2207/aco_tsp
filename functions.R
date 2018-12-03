parse_txt<-function(data){
  mn <- dim(data)
  m <- mn[1]-1 # number of constraints
  n <- mn[2]-2 #the number of variables
  C <- vector(length = n, mode ='numeric')
  for(i in 1:n) C[i] <- data[1, i]
  signs = vector(length = m, mode='character')
  for (i in 1:m)
  {
    if (data[ i+1, n+1]==-1) {signs[i]<-"<=";}
    else if (data[i+1, n+1]==1){signs[i]<-">=";}
    else signs[i]<-"=";
  }
  b <- vector(length = m, mode='numeric')
  for(i in 1:m)
  {
    b[i]=data[i+1, n+2]
  }
  A = matrix(1:(m*n), ncol=n)
  for(i in 1:m)
  {
    for(j in 1:n)
      A[i,j]<- data[i+1, j]
  }
  result <- list()
  result$C <- C
  result$A <-A
  result$b <- b
  result$signs <- signs
  return (result)
}


parse_transport_problem <- function(data){
  mn <- dim(data)
  m <- mn[1]-1 
  n <- mn[2]-1
  stocks <- vector(length = m, mode="numeric")
  demands <- vector(length = n, mode ="numeric")
  A <- matrix(1:(m*n), ncol = n)
  
  for(i in 1:m){
    stocks[i] = data[i, n+1]
  }
  
  for(j in 1:n){
    demands[j] = data[m+1, j]
  }
  
  for(i in 1:m){
    for(j in 1:n){
      A[i, j] = data[i, j]
    }
  }
  
  result <- list()
  result$demands <- demands
  result$stocks <- stocks
  result$A <- A
  result$stocks_signs <- rep("=", m)
  result$demands_signs <- rep("=", n)
  return(result)
  
}

parse_txt_assignment_problem <- function(data)
{
  n <- dim(data)[1]
  A <- matrix(1:(n*n), ncol = n)
  
  for(i in 1:n){
    for(j in 1:n){
      A[i,j] <- data[i, j]
    }
  }
  result <- list()
  result$cost = A
  return (result)
}

parse_txt_knapsak_problem<-function(data)
{
  n <- dim(data)[2] #count of columns
  weight <- vector(length = n, mode = "numeric")
  
  for(i in 1:n){
    weight[i] <- data[1, i]
  }
  price <- vector(length = n, mode = "numeric")
  for(i in 1:n){
    price[i] <- data[2, i]
  }
  capacity <- data[3,1]
  
  result <- list()
  result$weight <- weight
  result$price <- price
  result$capacity <- capacity
  return(result)
}