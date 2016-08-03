#EE 232 E Project 1 Final

library("igraph")
library(R.utils)
#creating the facebook network
data_list = scan("/Users/Sidharth/Desktop/OneDrive/Courses_UCLA/Spring16/EE232E/Project_1/facebook_combined.txt",what = list(0,0))
edge_in <- data_list[[1]] + 1
edge_out <- data_list[[2]] + 1
edge_set = cbind(edge_in,edge_out)

network <- graph.edgelist(el = edge_set, directed = FALSE)

cat(" Running Exercise #1\n")
############################### Exercise #1 ###################################

# connectivity test

is.connected(network)
vcount(network)
diameter(network)
V(network)$color <- "green"
plot(network,vertex.size=4,vertex.label=NA,vertex.color=V(network)$color)

# Plotting the degree distribution of the network

plot(degree_distribution(network), main = "Degree distribution of the nodes of the network")

#Histogram of the degree distribution of the network
degree.distribution(network)
degreeDist = degree(network)
h1 = hist(degreeDist, breaks=seq(0.0, by=1 , length.out=max(degreeDist)+2))       
df1 = data.frame(x=h1$mids, y=h1$density)

#fitting model
deg_data_y <- degree.distribution(network)
deg_data_x <- c(1:1046)
deg_data <- data.frame(x=deg_data_x,y=deg_data_y)

# Decaying Exponential
exp_model <- nls(y ~ I(exp(1)^(a + b*x)), data = df1, start = list(a=-1,b=-1))
deg_fitted1 <- fitted(exp_model)
plot(deg_data_x,deg_fitted1,type="l",col="red",xlab = "Node degrees",ylab = "Normalized Frequency",main = "Degree distribution (decaying exponentialmodel (Red) vs Actual(Green)")
lines(deg_data_x,deg_data_y,col="green")

exp_res <- residuals(exp_model)
exp_res_sq <- exp_res^2
mean(exp_res_sq)

#Logarithmic model
log_model <- nls(y ~ (1/(a +b*log(x))), data = deg_data, start = list(a=1,b=1))
deg_fitted2 <- fitted(log_model)
plot(deg_data_x,deg_fitted2,type="l",col="red",xlab = "Node degrees",ylim=c(0,0.026),ylab = "Normalized Frequency",main = "Degree distribution (Logarithmic model (Red) vs Actual(Green)")
lines(deg_data_x,deg_data_y,col="green")

log_res <- residuals(log_model)
log_res_sq <- log_res^2
mean(log_res_sq)

#inverse model
inv_model <- nls(y ~ (1/(a +b*x)), data = deg_data, start = list(a=1,b=1))
deg_fitted3 <- fitted(inv_model)
plot(deg_data_x,deg_fitted3,type="l",col="red",xlab = "Node degrees",ylab = "Normalized Frequency",main = "Degree distribution (inverse model (Red) vs Actual(Green)")
lines(deg_data_x,deg_data_y,col="green")

inv_res <- residuals(inv_model)
inv_res_sq <- inv_res^2
mean(inv_res_sq)

mean(degree(network))


cat(" Running Exercise #2\n")
############################### Exercise #2 ###################################

pers_net_1 <- make_ego_graph(network,1,1)
pers_net_1 <- pers_net_1[[1]]
V(pers_net_1)$color <- "green"
V(pers_net_1)$color[1] <- "red"
E(pers_net_1)$color <- "yellow"
plot(pers_net_1,vertex.size=4,vertex.label=NA,vertex.color=V(pers_net_1)$color,edge.color=E(pers_net_1)$color)

nonpers_net_1 <- which( !( (1:vcount(network)) %in% pers_net_1)  )
subpers_net_1 <- delete.vertices(network , nonpers_net_1)
E(pers_net_1)
V(pers_net_1)

cat(" Running Exercise #3\n")
############################### Exercise #3 ###################################
# Community structure of core node 39's personal network

core_nodes = {}
core_nodes<-which(neighborhood.size(network, 1 , nodes=V(network)) > 200)
mean(degreeDist[core_nodes])
length(core_nodes)

core <- core_nodes[39]
subGraph_core_nodes <- neighborhood(network , 1 , nodes=core)
subGraph_core_nodes <- subGraph_core_nodes[[1]]

nonSubGraph_core_nodes <- which( !( (1:vcount(network)) %in% subGraph_core_nodes)  )
subGraph_core <- delete.vertices(network , nonSubGraph_core_nodes)
vcount(subGraph_core)
plot(subGraph_core,vertex.size=4,vertex.label=NA)

# Community structure of core node 39's personal network

# Fast greedy and plotting the community strcuture

subGraph_core_fg <- fastgreedy.community(subGraph_core)
plot(subGraph_core_fg,subGraph_core,main="Communities for Core Node 39 Graph, FAST-GREEDY",vertex.label = NA)
print(sizes(subGraph_core_fg))
print(length(subGraph_core_fg))
print(modularity(subGraph_core_fg))

cg_1 <- contract.vertices(subGraph_core, membership(subGraph_core_fg))
E(cg_1)$weight <- 1
cgsimp_1 <- simplify(cg_1, remove.loops=FALSE)

plot(cgsimp_1, edge.label=E(cgsimp_1)$weight, margin=.5, layout=layout.circle,main = "Community structure (Fast-greedy)")



# Edge betweeness and plotting the community structure

subGraph_core_edgebet <- edge.betweenness.community(subGraph_core)
print(length(subGraph_core_edgebet))
print(modularity(subGraph_core_edgebet))
plot(subGraph_core_edgebet, subGraph_core,main="Communities for Core Node 39 Graph, EDGE-BETWEENNESS",vertex.label = NA)

cg_1 <- contract.vertices(subGraph_core, membership(subGraph_core_edgebet))
E(cg_1)$weight <- 1
cgsimp_1 <- simplify(cg_1, remove.loops=FALSE)

plot(cgsimp_1, edge.label=E(cgsimp_1)$weight, margin=.5, layout=layout.circle,main = "Community structure (EDGE-BETWEENNESS)")


# Info map and plotting the community structure

subGraph_core_imap <- infomap.community(subGraph_core)
plot(subGraph_core_imap, subGraph_core,main="Communities for Core Node 39 Graph, INFOMAP",vertex.label = NA)
print(length(subGraph_core_imap))
print(modularity(subGraph_core_imap))
print(sizes(subGraph_core_imap))

cg_1 <- contract.vertices(subGraph_core, membership(subGraph_core_imap))
E(cg_1)$weight <- 1
cgsimp_1 <- simplify(cg_1, remove.loops=FALSE)

plot(cgsimp_1, edge.label=E(cgsimp_1)$weight, margin=.5, layout=layout.circle,main = "Community structure (infomap)")


cat(" Running Exercise #4\n")
############################### Exercise #4 ###################################
# Community structure of core node 39's personal network with the core node removed

pers_net_39_rem <- delete_vertices(subGraph_core,1) 
plot(pers_net_39_rem,vertex.size=4,vertex.label=NA)

#Core node 1
core <- core_nodes[1]
subGraph_core_nodes <- neighborhood(network , 1 , nodes=core)
subGraph_core_nodes <- subGraph_core_nodes[[1]]

nonSubGraph_core_nodes <- which( !( (1:vcount(network)) %in% subGraph_core_nodes)  )
subGraph_core_1 <- delete.vertices(network , nonSubGraph_core_nodes)
vcount(subGraph_core_1)

pers_net_1_rem <- delete_vertices(subGraph_core_1,1) 
# Fast greedy and plotting the community strcuture

comm_fg_rem = fastgreedy.community(pers_net_1_rem)
plot(comm_fg_rem,pers_net_1_rem,main="Communities for Core Node 1 Graph (Node 1 removed), FAST-GREEDY",vertex.label = NA)
print(length(comm_fg_rem))
print(modularity(comm_fg_rem))
print(sizes(comm_fg_rem))
cg_1 <- contract.vertices(pers_net_1_rem , membership(comm_fg_rem))
E(cg_1)$weight <- 1
cgsimp_1 <- simplify(cg_1, remove.loops=FALSE)

plot(cgsimp_1, edge.label=E(cgsimp_1)$weight, margin=.5, layout=layout.circle,main = "Community structure (infomap)")


# Edge betweeness and plotting the community structure

comm_bet_rem <- edge.betweenness.community(pers_net_1_rem)
plot(comm_bet_rem, pers_net_1_rem,main="Communities for Core Node 1 Graph (Node 1 removed), EDGE-BETWEENNESS",vertex.label = NA)
print(length(comm_bet_rem))
print(modularity(comm_bet_rem))

cg_1 <- contract.vertices(pers_net_1_rem , membership(comm_bet_rem))
E(cg_1)$weight <- 1
cgsimp_1 <- simplify(cg_1, remove.loops=FALSE)

plot(cgsimp_1, edge.label=E(cgsimp_1)$weight, margin=.5, layout=layout.circle,main = "Community structure (infomap)")


# Info map and plotting the community structure

comm_imap_rem <- infomap.community(pers_net_1_rem)
plot(comm_imap_rem,pers_net_1_rem,main="Communities for Core Node 1 Graph (Node 1 removed), INFOMAP",vertex.label = NA)
print(length(comm_imap_rem))
print(modularity(comm_imap_rem))

cg_1 <- contract.vertices(pers_net_1_rem , membership(comm_imap_rem))
E(cg_1)$weight <- 1
cgsimp_1 <- simplify(cg_1, remove.loops=FALSE)

plot(cgsimp_1, edge.label=E(cgsimp_1)$weight, margin=.5, layout=layout.circle,main = "Community structure (infomap)")

# Fast greedy and plotting the community strcuture of core 39 removed

comm_fg_rem = fastgreedy.community(pers_net_39_rem)
plot(comm_fg_rem,pers_net_39_rem,main="Communities for Core Node 39 Graph (Node 39 removed), FAST-GREEDY",vertex.label = NA)
print(length(comm_fg_rem))
print(modularity(comm_fg_rem))
print(sizes(comm_fg_rem))
cg_1 <- contract.vertices(pers_net_39_rem , membership(comm_fg_rem))
E(cg_1)$weight <- 1
cgsimp_1 <- simplify(cg_1, remove.loops=FALSE)

plot(cgsimp_1, edge.label=E(cgsimp_1)$weight, margin=.5, layout=layout.circle,main = "Community structure (infomap)")


# Edge betweeness and plotting the community structure

comm_bet_rem <- edge.betweenness.community(pers_net_39_rem)
plot(comm_bet_rem, pers_net_39_rem,main="Communities for Core Node 39 Graph (Node 39 removed), EDGE-BETWEENNESS",vertex.label = NA)
print(length(comm_bet_rem))
print(modularity(comm_bet_rem))
print(sizes(comm_bet_rem))
cg_1 <- contract.vertices(pers_net_39_rem , membership(comm_bet_rem))
E(cg_1)$weight <- 1
cgsimp_1 <- simplify(cg_1, remove.loops=FALSE)

plot(cgsimp_1, edge.label=E(cgsimp_1)$weight, margin=.5, layout=layout.circle,main = "Community structure (EDGE-BETWEENNESS)")


# Info map and plotting the community structure

comm_imap_rem <- infomap.community(pers_net_39_rem)
plot(comm_imap_rem,pers_net_39_rem,main="Communities for Core Node 39 Graph (Node 39 removed), INFOMAP",vertex.label = NA)
print(length(comm_imap_rem))
print(modularity(comm_imap_rem))
print(sizes(comm_imap_rem))
cg_1 <- contract.vertices(pers_net_39_rem , membership(comm_imap_rem))
E(cg_1)$weight <- 1
cgsimp_1 <- simplify(cg_1, remove.loops=FALSE)

plot(cgsimp_1, edge.label=E(cgsimp_1)$weight, margin=.5, layout=layout.circle,main = "Community structure (infomap)")

cat(" Running Exercise #5\n")
############################### Exercise #5 ###################################


calc_disp <-function(pers_net)
{
  
  neighbor_list = list();
  for (i in 1:vcount(pers_net))
  {
    temp = which(pers_net[i] > 0, arr.ind = TRUE)
    neighbor_list[[length(neighbor_list) + 1]] = temp
  }
  
  dispersion = c();
  for (i in 1:vcount(pers_net))
    #for (i in 1:1)
  {
    temp_disp = 0
    #print(i)
    if(is.na(neighbor_list[[i]][1]))
    {
      dispersion  <- append(dispersion,0)
    }
    
    else
    {
      
      
      edge_names =c()
      for (j in 1: length(neighbor_list[[i]]))
      {
        
        edge_name = paste(toString(i),"|",toString(neighbor_list[[i]][j]),sep="")
        edge_names = append(edge_names,edge_name)
        
      }
      temp_graph <- delete_edges(pers_net,edge_names)
      dt = distances(temp_graph,v = neighbor_list[[i]],to = neighbor_list[[i]])
      
      dt[dt<=2] = 0
      dt[dt>2] = 1
      
      #dt[dt<Inf] = 0
      #dt[dt==Inf] = 1
      temp_disp = sum(dt)/2
      dispersion <-append(dispersion,temp_disp)
      
    }
  }
  return(dispersion)
}



embeddedness <- c()
disp <- c()
max_d <- c()
max_e <-c()
max_r <-c()
vm_d<-c()
vm_e <-c()
vm_r <- c()
comm_size <-c()

core_id <- c()
for (i in 1:41) 
{
  personal_net_i <- NULL
  pers_net_rem <- NULL
  net <- NULL
  net <- network
  
  personal_net_i <- make_ego_graph(net, 1, core_nodes[i])
  personal_net_i <- personal_net_i[[1]]
  core_id = append(core_id,which.max(degree(personal_net_i)))
}



for (i in 1:41) 
{
  personal_net_i <- NULL
  pers_net_rem <- NULL
  net <- NULL
  net <- network
  V(net)[core_nodes[i]]$names = "core"
  personal_net_i <- make_ego_graph(net, 1, core_nodes[i])
  personal_net_i <- personal_net_i[[1]]
  
  
  ind = which.max(degree(personal_net_i))
  pers_net_rem <- delete.vertices(personal_net_i,ind)
  
  comm_size <- append(comm_size,vcount(pers_net_rem))
  print(i)
  temp_e <- degree(pers_net_rem)
  
  temp_d <- calc_disp(pers_net_rem)
  temp_r <-temp_d/temp_e
  nan_ind = which(is.nan(temp_r),arr.ind = TRUE)
  temp_r[nan_ind] = 0
  
  
  
  
  max_d <- append(max_d,which.max(temp_d))
  print(max_d)
  max_e <- append(max_e,which.max(temp_e))
  max_r <- append(max_r,which.max(temp_r))
  vm_d <-append(vm_d,max(temp_d))
  vm_e <-append(vm_e,max(temp_e))
  vm_r <-append(vm_r,max(temp_r))
  
  
  embeddedness <- append(embeddedness,temp_e)
  disp <- append(disp,temp_d)
  if(i==1||i==3||i==7)
  {
    corr_d = 0;  
    corr_e = 0;
    corr_r = 0;
    if(max_d[i]>=core_id[i])
      corr_d = 1;
    if(max_e[i]>=core_id[i])
      corr_e = 1;
    if(max_r[i]>=core_id[i])
      corr_r = 1;
    
    eset_d <- E(personal_net_i)[from(max_d[i]+corr_d)]
    eset_e <- E(personal_net_i)[from(max_e[i]+corr_e)]
    eset_r <- E(personal_net_i)[from(max_r[i]+corr_r)]
    
    if(max_r[i]==max_d[i]&&max_r[i]==max_e[i])
    {
      
      E(personal_net_i)[eset_r]$color<-"red"
      
      print("here")
    }  
    else
    {
      E(personal_net_i)[eset_d]$color<-"green"
      E(personal_net_i)[eset_r]$color<-"red"
      E(personal_net_i)[eset_e]$color<-"blue"
    }
    V(personal_net_i)$color <- "white"
    V(personal_net_i)$color[core_id[i]] <- "blue"
    
    
    V(personal_net_i)[max_d[i]+corr_d]$color <- "green"
    V(personal_net_i)[max_r[i]+corr_r]$color <- "red"
    V(personal_net_i)[max_e[i]+corr_e]$color <- "black"
    comm_fg_i = infomap.community(personal_net_i)
    
    plot(comm_fg_i,personal_net_i,vertex.size=2,vertex.label=NA,edge.color=E(personal_net_i)$color)
    
    E(personal_net_i)$color<-"yellow"
    
    plot(personal_net_i,vertex.size=4,vertex.label=NA,vertex.color=V(personal_net_i)$color,edge.color=E(personal_net_i)$color)
    
    
  }
  
  
  
}

hist(disp,breaks =100)
hist(embeddedness,breaks = 100)

cat(" Running Exercise #6\n")
############################### Exercise #6 ###################################
g <- network
comm_list_map <- new.env()
vector_data <- c()
for (i in 1:length(core_nodes)) {
  intersted_comm <- c()
  persnal_net_i <- make_ego_graph(g, 1, core_nodes[i])
  persnal_net_i <- persnal_net_i[[1]]
  
  comm_fg_i <- fastgreedy.community(persnal_net_i)
  comm_len <- length(comm_fg_i)
  comm_sizes <- sizes(comm_fg_i)
  for (j in 1:comm_len) {
    if (comm_sizes[j] > 10) {
      #intersted_comm <- append(intersted_comm, j)
      members <- which(comm_fg_i$membership == j)
      sub_graph_j <- induced.subgraph(g, vids = members)
      m = modularity(comm_fg_i)
      cc = transitivity(sub_graph_j, "globalundirected")
      d = graph.density(sub_graph_j, loops = TRUE)
      s = vcount(sub_graph_j)
      vector_data <-data.frame(rbind(vector_data, cbind(m, cc, d, s,i,j)))
    }
  }
  
  #assign(key, intersted_comm, comm_list_map, inherits = FALSE)
}

feature_vector <- as.matrix(vector_data[,1:4])
feature_vector[!is.finite(feature_vector)] <- 0
cluster_len <- c()
unique_net_len <- c()
for (i in 2:6){
  clustering <- kmeans(feature_vector,i)
  cluster_len_i <- c()
  for (j in 1:i){
    cluster_len_i <- cbind(cluster_len_i,length(which(clustering$cluster ==j)))
    
  }
  sort_len <- sort(cluster_len_i, decreasing = TRUE,index.return = TRUE,method = "radix")
  cluster_len <- rbind(cluster_len,sort_len$x[1:2])
  network_len <- c()
  sorted_ind_len <- sort_len$ix[1:2]
  for (j in 1:length(sorted_ind_len)){
    network <- vector_data[which(clustering$cluster ==sorted_ind_len[j]),5]
    network_len <- cbind(network_len,length(unique(network)))
  }
  
  unique_net_len <- rbind(unique_net_len,network_len)
}


cat(" Running Exercise #7\n")
############################### Exercise #7 ###################################

node_list <- c()
node_list_copy <- c()
options("scipen"=100, "digits"=4)
# change the path name to your respective directory
path_name = "/Users/Sidharth/Desktop/"
#path_name = "/Users/deepakmuralidharan/Downloads/gplus/"

for (i in list.files(path = path_name, pattern = ".circles")){
  if(countLines((paste(path_name,i,sep = "")))[1] > 2){
    s <- paste(path_name,i,sep = "")
    s1 <- unlist(strsplit(s, split = '.', fixed=TRUE))[1]
    s2 <- paste(s1,".edges",sep = "")
    node_list <- append(node_list,s2)
    s3 <- tail(unlist(strsplit(s1, split = '/', fixed=TRUE)),1)
    node_list_copy <- append(node_list_copy,s3)
  }
}

network <- {}
info_community <- {}
walk_community <- {}
count = 0

print("Community ... ")
for (i in node_list){
  count = count + 1
  print(count)
  s1 <- tail(unlist(strsplit(i,split='/',fixed=TRUE)),n=1)
  name <- unlist(strsplit(s1,split='.',fixed=TRUE))[1]
  g <- read.graph(i,format = "ncol", directed=TRUE)
  
  a1 <- rep(name, vcount(g))
  a2 <- V(g)$name
  a3 <- c(rbind(a1,a2))
  
  g <- add_vertices(g,1,name = name)
  g <- add_edges(g,a3)
  
  network[[name]] <- g
  info_community[[name]] <- infomap.community(g)
  walk_community[[name]] <- walktrap.community(g)
  
}

# Plotting the personal network of user "name"

user_pnet <-  network[[name]]

V(user_pnet)$color <- "blue"
V(user_pnet)$color[695] <- "green"
#E(pers_net_1)$color <- "yellow"
#E(pers_net_1)$color[s] <- "blue"
plot(user_pnet,vertex.size=4,vertex.label=NA,vertex.color=V(user_pnet)$color,edge.color="yellow")

# Plotting the community structure of user "name"

# Walktrap

pnet_comm_walk <- walk_community[["118379821279745746467"]]
plot(pnet_comm_walk,user_pnet,main="Communities for Personal network of user, Walktrap",vertex.label = NA,vertex.size=4)
print(length(pnet_comm_walk))
print(modularity(pnet_comm_walk))

# Infomap


print("Circles ... ")

ground_truth <- {}
for (i in list.files(path = path_name, pattern = ".circles")){
  if(countLines((paste(path_name,i,sep = "")))[1] > 2){
    print(i)
    node_name <- unlist(strsplit(i, split = '.', fixed=TRUE))[1]
    f <- readLines((paste(path_name,i,sep = "")), n=-1)
    circle_hashmap <- {}
    for(j in 1:length(f)){
      circle_list <- as.list(unlist(strsplit(f[j], split = '\t', fixed=TRUE)),n=-1)
      #key <- circle_list[[1]]
      key <- j
      value <- tail(circle_list,length(circle_list)-1)
      circle_hashmap[[key]] <- value
    }
    ground_truth[[node_name]] <- circle_hashmap
  }
}

print("Calculating ratios ... ")
intersection_ratio_map = {}
intersection_size_map = {}

for (i in node_list_copy){
  print(i)
  max_community_index <- which.max(sizes(info_community[[i]]))
  max_community_list <- info_community[[i]][[max_community_index]]
  max_community_size <- max(sizes(info_community[[i]]))
  
  max_circle_index <- NULL
  max_circle_value <- 0
  for (j in 1:length(ground_truth[[i]])){
    if(length(ground_truth[[i]][[j]]) > max_circle_value){
      max_circle_value <- length(ground_truth[[i]][[j]])
      max_circle_index <- j
    }
  }
  
  #print(max_circle_value)
  #print(max_community_sizes)
  
  intersect_length <- length(intersect(max_community_list, ground_truth[[i]][[max_circle_index]]))
  intersection_ratio <- intersect_length/min(max_community_size, max_circle_value)
  print(intersection_ratio)
  
  #size_ratio = min(max_community_size, max_circle_value)/max(max_community_size, max_circle_value)
  size_ratio = max_community_size/max_circle_value
  print(size_ratio)
  intersection_ratio_map[[i]] <- intersection_ratio
  intersection_size_map[[i]] <- size_ratio
  
}