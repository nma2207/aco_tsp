n <- 50

m <- matrix(integer(n^2), ncol = n)

#generate symmetric matrix
for(i in 1:(n-1)) {
  for (j in (i+1):n){
    k <- runif(1, 1, 10)
    m[i, j] <- k
    m[j, i] <- k
  }
}

isSymmetric(m)
source("aco_tsp.r")
solution <- acp_tsp(weights = m,
                    ant_count = 100,
                    iteration_count = 1000,
                    alpha = 0.5,
                    betta = 0.5,
                    q = 0.005,
                    p = 0.5)
