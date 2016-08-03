library(igraph)
edgelistFile <- "edge_list.txt"
data_edge <- do.call(rbind,strsplit(readLines(edgelistFile),'\t\t',fixed=T))
network_act <- graph.data.frame(data_edge, directed=TRUE)
E(network_act)$weight <-as.double(data_edge[,3])
pagerank<-page.rank(network_act,directed=TRUE,damping=0.85)
png("plot1.png")
plot(degree(network_act,mode = "in"),pagerank$vector,xlab="Degree of node visited",ylab = "Page Rank of Node", main = "Page Rank vs Node Degree");
dev.off()
ordered_pr <-pagerank$vector[order(pagerank$vector,decreasing = TRUE)]

save.image("question2.Rdata")