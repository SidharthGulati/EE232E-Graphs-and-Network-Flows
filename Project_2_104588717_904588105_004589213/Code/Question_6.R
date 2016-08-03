## Question 6

# Creating the graph object 

library(igraph)

edgelistFile <- "movie_jaccard_index.txt"
data_edge <- do.call(rbind,strsplit(readLines(edgelistFile),'\t\t',fixed=T))
network_mov <- graph.data.frame(data_edge, directed=FALSE)
E(network_mov)$weight <-as.double(data_edge[,3])


# Algorithm

movie_list <- c("Batman v Superman: Dawn of Justice (2016)  ","Mission: Impossible - Rogue Nation (2015)  ","Minions (2015)  ")
top_neighbors = {}

for(movie in movie_list){
  index_1 <- which(V(network_mov)$name ==  movie)
  edge_set_1 <- E(network_mov) [ from(index_1)]
  edge_weight_1 <- E(network_mov)[edge_set_1]$weight
  sorted<- sort.int(edge_weight_1,index.return = TRUE,decreasing = TRUE)
  top_neighbors[[movie]] <-edge_set_1[sorted$ix][1:5]
  
}
print(top_neighbors)