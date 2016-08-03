# EE 232E HW2
library("igraph")
library("netrw")
## Q1
#Function to calculate the shortest path
path_func <- function(network,num_nodes,damping_factor)
{
  path_avg <- rep(NA,num_nodes)
  path_dev <- rep(NA,num_nodes)
  
  for(i in (1:num_nodes))
  {
    rand_1 <- netrw(network,walker.num = num_nodes,start.node = sample(1:vcount(network)),damping = damping_factor, T = i, output.walk.path = TRUE)
    temp <- rep(NA,num_nodes)
    for(j in (1:num_nodes))
    {
      temp_var <- get.shortest.paths(network, from = rand_1$walk.path[1,j], to = rand_1$walk.path[i,j])
      temp[j] <- length(temp_var$vpath[[1]])-1
    }
    path_avg[i] <- mean(temp)
    path_dev[i] <- sd(temp)
  }
  
  plot(1:num_nodes,path_avg,type="line",xlab="Number of steps(t)",ylab = "Average Path length <s(t)>",main = "Plot of average path length")
  plot(1:num_nodes,path_dev,type="line",xlab="Number of steps(t)",ylab = "Standard deviation",main = "Plot of standard deviation of path length")
  print(mean(path_avg))
  
  # Plotting the degree distribution of graph
  
  hist(degree(network),seq(-0.5, by=1, length.out = max(degree(network))+2))
  
  # Plotting the degree distribution of the nodes reached at the end of the random walk
  degdist <- rep(NA,num_nodes)
  for(i in (1:num_nodes))
  {
    degdist[i] = degree(network,rand_1$walk.path[num_nodes,i])
  }
  hist(degdist,seq(-0.5, by=1, length.out = max(degdist)+2))
  
}

## Function to calculate shortest path for 10000 nodes. Only 200 iterations.
path_func_10000 <- function(network,num_nodes,damping_factor)
{
  path_avg <- rep(NA,200)
  path_dev <- rep(NA,200)
  
  for(i in (1:200))
  {
    print(i)
    rand_1 <- netrw(network,walker.num = num_nodes,start.node = sample(1:vcount(network)),damping = damping_factor, T = i, output.walk.path = TRUE)
    temp <- rep(NA,num_nodes)
    for(j in (1:num_nodes))
    {
      temp_var <- get.shortest.paths(network, from = rand_1$walk.path[1,j], to = rand_1$walk.path[i,j])
      temp[j] <- length(temp_var$vpath[[1]])-1
    }
    path_avg[i] <- mean(temp)
    path_dev[i] <- sd(temp)
  }
  
  plot(1:200,path_avg,type="line",xlab="Number of steps(t)",ylab = "Average Path length <s(t)>",main = "Plot of average path length")
  plot(1:200,path_dev,type="line",xlab="Number of steps(t)",ylab = "Standard deviation",main = "Plot of standard deviation of path length")
  print(mean(path_avg))
  
  # Plotting the degree distribution of graph
  
  hist(degree(network),seq(-0.5, by=1, length.out = max(degree(network))+2))
  
  # Plotting the degree distribution of the nodes reached at the end of the random walk
  degdist <- rep(NA,200)
  for(i in (1:200))
  {
    degdist[i] = degree(network,rand_1$walk.path[200,i])
  }
  hist(degdist,seq(-0.5, by=1, length.out = max(degdist)+2))
  
  
}

# Creating the 1000 node undirected random network with p=0.01

g_1_1000 <- random.graph.game(1000,0.01,type = "gnp",directed = FALSE)
diameter(g_1_1000,directed = FALSE)

# Plotting the average path length and sd of path length for 1000 node network

path_func(g_1_1000,1000,1)



# Generating a 100 node network
g_1_100 <- random.graph.game(100,0.01,type = "gnp",directed = FALSE)
diameter(g_1_100,directed = FALSE)

# Plotting the average path length and sd of path length for 100 node network

path_func(g_1_100,100,1)

# Generating a 10000 node network
g_1_10000 <- random.graph.game(10000,0.01,type = "gnp",directed = FALSE)
diameter(g_1_10000,directed = FALSE)
is.connected(g_1_10000)

# Plotting the average path length and sd of path length for 10000 node network

path_func_10000(g_1_10000,10000,1)


## Q2

# Generating a 1000 node network following power law degree distribution

g_2_1000 <- barabasi.game(n=1000,power = 1,zero.appeal = 1,directed = FALSE)
diameter(g_2_1000,directed = FALSE)
is.connected(g_2_1000)


# Plotting the average path length and sd of path length for 1000 node network

path_func(g_2_1000,1000,1) 

# Generating a 100 node network following power law degree distribution

g_2_100 <- barabasi.game(n=100,power = 1,zero.appeal = 1,directed = FALSE)
diameter(g_2_100,directed = FALSE)
is.connected(g_2_100)

# Plotting the average path length and sd of path length for 100 node network

path_func(g_2_100,100,1)

# Generating a 10000 node network following power law degree distribution

g_2_10000 <- barabasi.game(n=10000,power = 1,zero.appeal = 1,directed = FALSE)
diameter(g_2_10000,directed = FALSE)
is.connected(g_2_10000)

# Plotting the average path length and sd of path length for 10000 node network

path_func(g_2_10000,10000,1) 


## Q3

#  probability of visiting each node against the node degrees

plot_vprob_deg <- function(network,num_nodes,damping_factor)
{
  node_deg <- c()
  vprob <- c()
  
  rand_3 <- netrw(network,walker.num = num_nodes,start.node = sample(1:vcount(network)),damping = damping_factor, T=1000, output.walk.path = TRUE)
  
  for(i in rand_3$walk.path[,1])
  {
    node_deg <- append(node_deg,degree(network,i))
    vprob <- append(vprob,rand_3$ave.visit.prob[i])
  }
  plot(node_deg,vprob,xlab = "Node degree",ylab = "Visit Probability")
}

# Plotting for the network generated in Q1(a)

plot_vprob_deg(g_1_1000,1000,1)

# Generating a directed network with 1000 nodes

g_1_dir_1000 <- random.graph.game(1000,0.01,type = "gnp",directed = TRUE)

#  probability of visiting each node versus in-node degree

plot_vprob_in_deg <- function(network,num_nodes,damping_factor)
{
  node_in_deg <- c()
  vprob <- c()
  
  rand_3 <- netrw(network,walker.num = num_nodes,start.node = sample(1:vcount(network)),damping = damping_factor, T=1000, output.walk.path = TRUE)
  
  for(i in rand_3$walk.path[,1])
  {
    node_in_deg <- append(node_in_deg,degree(network,i,mode = "in"))
    vprob <- append(vprob,rand_3$ave.visit.prob[i])
  }
  plot(node_in_deg,vprob,xlab = "In-degree of nodes",ylab = "Visit Probability")
}

# Plotting for the directed network

plot_vprob_in_deg(g_1_dir_1000,1000,1)

#  visit probabiity versus node degree with teleportation

plot_vprob_deg_tel <- function(network,num_nodes,damping_factor,tel_vect)
{
  node_deg <- c()
  vprob <- c()
  
  rand_3 <- netrw(network,walker.num = num_nodes,start.node = sample(1:vcount(network)),damping = damping_factor, T=1000, output.walk.path = TRUE,teleport.prob = tel_vect)
  
  for(i in rand_3$walk.path[,1])
  {
    node_deg <- append(node_deg,degree(network,i))
    vprob <- append(vprob,rand_3$ave.visit.prob[i])
  }
  plot(node_deg,vprob,xlab = "Node degree",ylab = "Visit Probability")
}

# Plotting for the 1000 node undirected network with teleportation

tel <- rep(0,1000)
tel[c(5,6)] <- 1
plot_vprob_deg_tel(g_1_1000,1000,0.85,tel)


#Q4
#function for calculating rank

walk4 <- function(graph,nodes,damping_factor,vector)
{
  r1<-netrw(graph, walker.num=nodes,start.node=1:vcount(graph),damping=damping_factor,T=1000, output.walk.path=TRUE,teleport.prob=vector)
  #plot(r1$ave.visit.prob,xlab="Nodes",ylab="Visit Probablity",pch=1)
  
  degree_prob<- NULL
  visit_prob <- NULL
  for(i in r1$walk.path[,1])
  {
    degree_prob<-append(degree_prob,degree(graph,i,mode = "in"))
    visit_prob<-append(visit_prob,r1$ave.visit.prob[i])
  }
  rank<- cbind(degree_prob,visit_prob)
  
  
  return(rank)
  
}


#4a)
graph4 <- random.graph.game(1000, 0.01, directed=TRUE)
r <- walk4(graph4,1000,0.85,NULL)
pagerank<-page.rank(graph4,directed=TRUE,damping=0.85)
plot(r[,1],r[,2],xlab="Degree of node visited",ylab = "Probability of visting the node", main = "Random walk with equal teleportation probability");
plot(degree(graph4,mode = "in"),pagerank$vector,xlab="Degree of node visited",ylab = "Page Rank of Node", main = "Page Rank vs Node Degree");

#4b)
r2 <-walk4(graph4,1000,0.85,pagerank$vector)
plot(r2[,1],r2[,2],xlab="Degree of node visited",ylab = "Probability of visting the node", main = "Random walk with personalized teleportation probability");