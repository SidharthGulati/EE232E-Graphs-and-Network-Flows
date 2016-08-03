## Question 6
library(igraph)
load('question4.Rdata')
# Using the graph object created in 4

E(network_mov)$weight <- E(network_mov)$weight/2

network_mov_simp <- simplify(network_mov)

# Algorithm

movie_list <- c("Batman v Superman: Dawn of Justice (2016)  ","Mission: Impossible - Rogue Nation (2015)  ","Minions (2015)  ")
top_neighbors = {}

for(movie in movie_list){
  index_1 <- which(V(network_mov_simp)$name ==  movie)
  edge_set_1 <- E(network_mov_simp) [ from(index_1)]
  edge_weight_1 <- E(network_mov_simp)[edge_set_1]$weight
  sorted<- sort.int(edge_weight_1,index.return = TRUE,decreasing = TRUE)
  top_neighbors[[movie]] <-edge_set_1[sorted$ix][1:5]
  
}
sink('output6.txt')
print(top_neighbors)
sink()

save.image("question6.Rdata")