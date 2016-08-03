#!/usr/bin/env Rscript
############################################ Homework 1 ###########################################
library(igraph)

cat(" Running Exercise #1\n")
############################### Exercise #1 ###################################

# p = 0.01
g_0.01 <- sample_gnp(1000,0.01)
plot(g_0.01,vertex.size=2,vertex.label=NA)
degree.distribution(g_0.01)
plot(degree.distribution(g_0.01))
hist(degree(g_0.01),seq(-0.5, by=1, length.out = max(degree(g_0.01))+2))
diameter(g_0.01)
is_connected(g_0.01)
get_diameter(g_0.01)
farthest_vertices(g_0.01) 

# p = 0.05
g_0.05 <- sample_gnp(1000,0.05)
plot(g_0.05,vertex.size=2,vertex.label=NA)
degree(g_0.05)
plot(degree.distribution(g_0.05))
hist(degree(g_0.05),seq(-0.5, by=1, length.out = max(degree(g_0.05))+2))
diameter(g_0.05)
is_connected(g_0.05)
get_diameter(g_0.05)
farthest_vertices(g_0.05) 


# p = 0.1
g_0.1 <- sample_gnp(1000,0.1)
plot(g_0.1,vertex.size=2,vertex.label=NA)
degree(g_0.1)
plot(degree.distribution(g_0.1))
hist(degree(g_0.1),seq(-0.5, by=1, length.out = max(degree(g_0.1))+2))
diameter(g_0.1)
is_connected(g_0.1)
get_diameter(g_0.1)
farthest_vertices(g_0.1) 

prob_seq = seq(0.0001,0.01,by=0.00001)
gccvec <- rep(0,length(prob_seq))
index = 1
for (i in prob_seq) {
  g_1 <- sample_gnp(1000,i)
  gr_conn_list <- decompose.graph(g_1)
  gcc_index <- which.max(sapply(gr_conn_list,vcount))
  gcc <- gr_conn_list[[gcc_index]]
  gccvec[index] = vcount(gcc)/1000
  index = index + 1
}
plot(prob_seq,gccvec,xlab = "p",ylab = "|GCC|/N")

#Connectivity test, p = 0.007 is the threshold
g_c <- sample_gnp(1000,0.007)
is_connected(g_c)


cat(" Running Exercise #2\n")
############################### Exercise #2 ###################################
## Part a)
graphics.off()
library("igraph")
N = 1000;
dist_exponent = -3;
deg_dist_a <- sample(1:N, N, replace=TRUE, prob=(1:N)^dist_exponent)
if (sum(deg_dist_a) %% 2 != 0) { deg_dist_a[1] <- deg_dist_a[1] + 1 }
graph_a <- sample_degseq(out.deg = deg_dist_a, in.deg = NULL, method = c("simple.no.multiple"))

plot(graph_a, main="Random Graph with N=1000",vertex.size=2,vertex.label=NA, vertex.color = "green")
plot(degree_distribution(graph_a), main = "Degree Distribution", xlab = "Degree of the random graph", ylab = "Probabilty Distribution")
hist(degree(graph_a), seq(-0.5,by=1,length.out = max(degree(graph_a))+2), main="Histogram of Degree of Random Graph", xlab ="Degree", ylab = "Frequency")
dia <- diameter(graph_a)

## Part b)
is_connected(graph_a)

# Creating the giant connected component

gr_connected_list <- decompose.graph(graph_a)
gcc_index <- which.max(sapply(gr_connected_list,vcount))
gcc <- gr_connected_list[[gcc_index]]
vcount(gcc)
graph_a_community <- cluster_fast_greedy(graph_a)
modularity(graph_a_community)

plot(gcc, main = "Greatest Connected Component for N =1000",vertex.size=1,vertex.label=NA)

# Part c)
N_L = 10000
deg_dist_c <- sample(1:N_L, N_L, replace=TRUE, prob=(1:N_L)^dist_exponent)
if (sum(deg_dist_c) %% 2 != 0) { deg_dist_c[1] <- deg_dist_c[1] + 1 }
graph_c <- sample_degseq(out.deg = deg_dist_c, in.deg = NULL, method = c("simple.no.multiple"))

# Plotting Degree Distribution plots.
plot(graph_c,main = "Random Graph with N = 10000",vertex.size=1,vertex.label=NA,vertex.color="green")
diameter(graph_c)
plot(degree.distribution(graph_c), main ="Degree Distribution for Random Graph with N=10000",xlab="Degree",ylab="Distribution")
hist(degree(graph_c),seq(-0.5, by=1, length.out = max(degree(graph_c))+2),main="Histogram of Degree for Random Graph with N=10000",xlab="Degree",ylab="Frequency")
is_connected(graph_c)
ecount(graph_c)

# Creating the giant connected component

gr_connected_list_c <- decompose.graph(graph_c)
gcc_index_c <- which.max(sapply(gr_connected_list_c,vcount))
gcc_c <- gr_connected_list_c[[gcc_index_c]]
vcount(gcc_c)
plot(gcc_c, main = "Greatest Connected Component for N =10000",vertex.size=1,vertex.label=NA,vertex.color="green")
# Finding the community Structure.

fast_greedy_community_c <- cluster_fast_greedy(graph_c)
#plot(fast_greedy_community_c,main = "Fast Greedy Community for Random Graph with N =10000")
modularity(fast_greedy_community_c)

## Part d)
degrees = array(0,vcount(graph_c));
for(k in 1:vcount(graph_c))
{
  i = sample(vcount(graph_c),1);
  n = neighbors(graph_c, i, mode = "total");
  
  if(length(n)>0){
    
    j = sample(length(n),1);
    degrees[k] = degree(graph_c,n[j]);
  }else{
    degrees[k] = 0;
  }
}

hist(degrees,seq(-0.5,by=1,length.out=max(degrees)+2))

cat(" Running Exercise #3\n")
############################### Exercise #3 ###################################

# Evolving preferential attatchment random graph\
#################### For fixed Aging exponent but varying pa.exp ##########################

# For pa.exp = 1
g_growing <- sample_pa_age(1000, pa.exp=1, aging.exp=-3, aging.bin = 100,directed = FALSE);
plot(g_growing,vertex.size=2,vertex.label=NA)
degree(g_growing)
plot(degree.distribution(g_growing))
hist(degree(g_growing),seq(-0.5, by=1, length.out = max(degree(g_growing))+2))
diameter(g_growing)
is_connected(g_growing)
get_diameter(g_growing)
farthest_vertices(g_growing)

# For pa.exp = 2
g_growing <- sample_pa_age(1000, pa.exp=2, aging.exp=-3, aging.bin = 100,directed = FALSE);
plot(g_growing,vertex.size=2,vertex.label=NA)
degree(g_growing)
plot(degree.distribution(g_growing))
hist(degree(g_growing),seq(-0.5, by=1, length.out = max(degree(g_growing))+2))
diameter(g_growing)
is_connected(g_growing)
get_diameter(g_growing)
farthest_vertices(g_growing)

# For pa.exp = 3
g_growing <- sample_pa_age(1000, pa.exp=3, aging.exp=-3, aging.bin = 100,directed = FALSE);
plot(g_growing,vertex.size=2,vertex.label=NA)
degree(g_growing)
plot(degree.distribution(g_growing))
hist(degree(g_growing),seq(-0.5, by=1, length.out = max(degree(g_growing))+2))
diameter(g_growing)
is_connected(g_growing)
get_diameter(g_growing)
farthest_vertices(g_growing)

# For pa.exp = 4
g_growing <- sample_pa_age(1000, pa.exp=4, aging.exp=-3, aging.bin = 100,directed = FALSE);
plot(g_growing,vertex.size=2,vertex.label=NA)
degree(g_growing)
plot(degree.distribution(g_growing))
hist(degree(g_growing),seq(-0.5, by=1, length.out = max(degree(g_growing))+2))
diameter(g_growing)
is_connected(g_growing)
get_diameter(g_growing)
farthest_vertices(g_growing)

#################### For varying Aging exponent but fixed pa.exp ##########################

# For aging.exp = -1
g_growing <- sample_pa_age(1000, pa.exp=1, aging.exp=-1, aging.bin = 100,directed = FALSE);
plot(g_growing,vertex.size=2,vertex.label=NA)
degree(g_growing)
plot(degree.distribution(g_growing))
hist(degree(g_growing),seq(-0.5, by=1, length.out = max(degree(g_growing))+2))
diameter(g_growing)
is_connected(g_growing)
get_diameter(g_growing)
farthest_vertices(g_growing)

# For aging.exp = -2
g_growing <- sample_pa_age(1000, pa.exp=1, aging.exp=-2, aging.bin = 100,directed = FALSE);
plot(g_growing,vertex.size=2,vertex.label=NA)
degree(g_growing)
plot(degree.distribution(g_growing))
hist(degree(g_growing),seq(-0.5, by=1, length.out = max(degree(g_growing))+2))
diameter(g_growing)
is_connected(g_growing)
get_diameter(g_growing)
farthest_vertices(g_growing)

# For aging.exp = -3
g_growing <- sample_pa_age(1000, pa.exp=1, aging.exp=-3, aging.bin = 100,directed = FALSE);
plot(g_growing,vertex.size=2,vertex.label=NA)
degree(g_growing)
plot(degree.distribution(g_growing))
hist(degree(g_growing),seq(-0.5, by=1, length.out = max(degree(g_growing))+2))
diameter(g_growing)
is_connected(g_growing)
get_diameter(g_growing)
farthest_vertices(g_growing)

# For aging.exp = -4
g_growing <- sample_pa_age(1000, pa.exp=1, aging.exp=-4, aging.bin = 100,directed = FALSE);
plot(g_growing,vertex.size=2,vertex.label=NA)
degree(g_growing)
plot(degree.distribution(g_growing))
hist(degree(g_growing),seq(-0.5, by=1, length.out = max(degree(g_growing))+2))
diameter(g_growing)
is_connected(g_growing)
get_diameter(g_growing)
farthest_vertices(g_growing)


# Creating the communities
#################### For fixed Aging exponent but varying pa.exp ##########################
# For pa.exp = 1
g_growing <- sample_pa_age(1000, pa.exp=1, aging.exp=-3, aging.bin = 100,directed = FALSE);
g_growingcomm <- cluster_fast_greedy(g_growing)
modularity(g_growingcomm)

# For pa.exp = 2
g_growing <- sample_pa_age(1000, pa.exp=2, aging.exp=-3, aging.bin = 100,directed = FALSE);
g_growingcomm <- cluster_fast_greedy(g_growing)
modularity(g_growingcomm)

# For pa.exp = 3
g_growing <- sample_pa_age(1000, pa.exp=3, aging.exp=-3, aging.bin = 100,directed = FALSE);
g_growingcomm <- cluster_fast_greedy(g_growing)
modularity(g_growingcomm)

# For pa.exp = 4
g_growing <- sample_pa_age(1000, pa.exp=4, aging.exp=-3, aging.bin = 100,directed = FALSE);
g_growingcomm <- cluster_fast_greedy(g_growing)
modularity(g_growingcomm)

#################### For varying Aging exponent but fixed pa.exp ##########################
# For aging.exp = -1
g_growing <- sample_pa_age(1000, pa.exp=1, aging.exp=-1, aging.bin = 100,directed = FALSE);
g_growingcomm <- cluster_fast_greedy(g_growing)
modularity(g_growingcomm)

# For aging.exp = -2
g_growing <- sample_pa_age(1000, pa.exp=1, aging.exp=-2, aging.bin = 100,directed = FALSE);
g_growingcomm <- cluster_fast_greedy(g_growing)
modularity(g_growingcomm)

# For aging.exp = -3
g_growing <- sample_pa_age(1000, pa.exp=1, aging.exp=-3, aging.bin = 100,directed = FALSE);
g_growingcomm <- cluster_fast_greedy(g_growing)
modularity(g_growingcomm)

# For aging.exp = -4
g_growing <- sample_pa_age(1000, pa.exp=1, aging.exp=-4, aging.bin = 100,directed = FALSE);
g_growingcomm <- cluster_fast_greedy(g_growing)
modularity(g_growingcomm)

cat(" Running Exercise #4\n")
############################### Exercise #4 ###################################
# Evolving forestfire random graph

# Varying fw.prob

g_f_1 <- sample_forestfire(1000, fw.prob = 0.1, bw.factor = 1, directed = TRUE)
g_f_2 <- sample_forestfire(1000, fw.prob = 0.2, bw.factor = 1, directed = TRUE)
g_f_3 <- sample_forestfire(1000, fw.prob = 0.3, bw.factor = 1, directed = TRUE)
#g_f_4 <- sample_forestfire(1000, fw.prob = 0.4, bw.factor = 1, directed = TRUE)

par(mfrow=c(2,1))
plot(degree.distribution(g_f_1,mode="in"))
hist(degree(g_f_1, mode="in"),seq(-0.5, by=1, length.out = max(degree(g_f_1, mode="in"))+2))
diameter(g_f_1, directed = TRUE)

par(mfrow=c(2,1))
plot(degree.distribution(g_f_1,mode="out"))
hist(degree(g_f_1, mode="out"),seq(-0.5, by=1, length.out = max(degree(g_f_1, mode="out"))+2))
diameter(g_f_1, directed = TRUE)


par(mfrow=c(2,1))
plot(degree.distribution(g_f_2,mode="in"))
hist(degree(g_f_2, mode="in"),seq(-0.5, by=1, length.out = max(degree(g_f_2, mode="in"))+2))
diameter(g_f_2, directed = TRUE)

par(mfrow=c(2,1))
plot(degree.distribution(g_f_2,mode="out"))
hist(degree(g_f_2, mode="out"),seq(-0.5, by=1, length.out = max(degree(g_f_2, mode="out"))+2))
diameter(g_f_2, directed = TRUE)

par(mfrow=c(2,1))
plot(degree.distribution(g_f_3,mode="in"))
hist(degree(g_f_3, mode="in"),seq(-0.5, by=1, length.out = max(degree(g_f_3, mode="in"))+2))
diameter(g_f_3, directed = TRUE)

par(mfrow=c(2,1))
plot(degree.distribution(g_f_3,mode="out"))
hist(degree(g_f_3, mode="out"),seq(-0.5, by=1, length.out = max(degree(g_f_3, mode="out"))+2))
diameter(g_f_3, directed = TRUE)
#
#par(mfrow=c(2,1))
#plot(degree.distribution(g_f_4,mode="in"))
#hist(degree(g_f_4, mode="in"),seq(-0.5, by=1, length.out = max(degree(g_f_4, mode="in"))+2))
#diameter(g_f_4, directed = TRUE)

#par(mfrow=c(2,1))
#plot(degree.distribution(g_f_4,mode="out"))
#hist(degree(g_f_4, mode="out"),seq(-0.5, by=1, length.out = max(degree(g_f_4, mode="out"))+2))
#diameter(g_f_4, directed = TRUE)

# Varying bw.factor

g_b_1 <- sample_forestfire(1000, fw.prob = 0.3, bw.factor = 0.6, directed = TRUE)
g_b_2 <- sample_forestfire(1000, fw.prob = 0.3, bw.factor = 0.8, directed = TRUE)
g_b_3 <- sample_forestfire(1000, fw.prob = 0.3, bw.factor = 1, directed = TRUE)
#g_b_4 <- sample_forestfire(1000, fw.prob = 0.3, bw.factor = 1.2, directed = TRUE)

par(mfrow=c(2,1))
plot(degree.distribution(g_b_1,mode="in"))
hist(degree(g_b_1, mode="in"),seq(-0.5, by=1, length.out = max(degree(g_b_1, mode="in"))+2))
diameter(g_b_1, directed = TRUE)

par(mfrow=c(2,1))
plot(degree.distribution(g_b_1,mode="out"))
hist(degree(g_b_1, mode="out"),seq(-0.5, by=1, length.out = max(degree(g_b_1, mode="out"))+2))
diameter(g_b_1, directed = TRUE)

par(mfrow=c(2,1))
plot(degree.distribution(g_b_2,mode="in"))
hist(degree(g_b_2, mode="in"),seq(-0.5, by=1, length.out = max(degree(g_b_2, mode="in"))+2))
diameter(g_b_2, directed = TRUE)

par(mfrow=c(2,1))
plot(degree.distribution(g_b_2,mode="out"))
hist(degree(g_b_2, mode="out"),seq(-0.5, by=1, length.out = max(degree(g_b_2, mode="out"))+2))
diameter(g_b_2, directed = TRUE)

par(mfrow=c(2,1))
plot(degree.distribution(g_b_3,mode="in"))
hist(degree(g_b_3, mode="in"),seq(-0.5, by=1, length.out = max(degree(g_b_3, mode="in"))+2))
diameter(g_b_3, directed = TRUE)

par(mfrow=c(2,1))
plot(degree.distribution(g_b_3,mode="out"))
hist(degree(g_b_3, mode="out"),seq(-0.5, by=1, length.out = max(degree(g_b_3, mode="out"))+2))
diameter(g_b_3, directed = TRUE)


#par(mfrow=c(2,1))
#plot(degree.distribution(g_b_4,mode="in"))
#hist(degree(g_b_4, mode="in"),seq(-0.5, by=1, length.out = max(degree(g_b_4, mode="in"))+2))
#diameter(g_b_4, directed = TRUE)

#par(mfrow=c(2,1))
#plot(degree.distribution(g_b_4,mode="out"))
#hist(degree(g_b_4, mode="out"),seq(-0.5, by=1, length.out = max(degree(g_b_4, mode="out"))+2))
#diameter(g_b_4, directed = TRUE)





# Creating the communities

g_f_comm_1 <- cluster_edge_betweenness(g_f_1)
modularity(g_f_comm_1)
length(g_f_comm_1)

g_f_comm_2 <- cluster_edge_betweenness(g_f_2)
modularity(g_f_comm_2)
length(g_f_comm_2)

g_f_comm_3 <- cluster_edge_betweenness(g_f_3)
modularity(g_f_comm_3)
length(g_f_comm_3)





g_b_comm_1 <- cluster_edge_betweenness(g_b_1)
modularity(g_b_comm_1)
length(g_b_comm_1)

g_b_comm_2 <- cluster_edge_betweenness(g_b_2)
modularity(g_b_comm_2)
length(g_b_comm_2)

g_b_comm_3 <- cluster_edge_betweenness(g_b_3)
modularity(g_b_comm_3)
length(g_b_comm_3)

