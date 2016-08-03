library(igraph)
edgelistFile <- "movie_jaccard_index.txt"
data_edge <- do.call(rbind,strsplit(readLines(edgelistFile),'\t\t',fixed=T))
network_mov <- graph.data.frame(data_edge, directed=FALSE)
E(network_mov)$weight <-as.double(data_edge[,3])


save(network_mov, file = 'graph_object.Rdata')