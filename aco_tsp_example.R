
#for read txt file
source("functions.r")

#read table
table <- read.csv("tsp_1.txt", sep="\t", header = FALSE)

distances <- parse_txt(table)
villages <- c("Sukkulovo", "Atsuyarovo",
              "Mamadalevo", "Utkeneevo", "Yukalekulevo")

source("aco_tsp.r")

solution <- acp_tsp(weights = distances,
                    ant_count = 10,
                    iteration_count = 10,
                    alpha = 0.5,
                    betta = 0.5,
                    q = 0.1,
                    p = 0.5,
                    labels = villages)

print(solution$total_weight)
print(solution$way)
