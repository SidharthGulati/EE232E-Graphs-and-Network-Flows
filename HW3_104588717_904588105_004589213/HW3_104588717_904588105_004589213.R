# EE 232E HW3
library("igraph")
## Data Processing

# Reading in the data from the file and storing the edge set
data_list = scan("/Users/Sidharth/Desktop/OneDrive/Courses_UCLA/Spring16/EE232E/Homeworks/HW3_104588717_904588105_004589213/sorted_directed_net.txt",what = list(0,0,0))
edge_in <- data_list[[1]] + 1
edge_out <- data_list[[2]] + 1
edge_set = cbind(edge_in,edge_out)

# Creating the graph from the edge set

network <- graph.edgelist(el = edge_set, directed = TRUE)
E(network)$weight <- data_list[[3]]



## Question 1

# Checking for connectivity

is.connected(network)

# Sanity check

gcc_strong <- clusters(network,mode = "strong")
gcc_strong$csize[8]

# Creating the giant connected component

network_component_list <- decompose.graph(network)
gcc_index <- which.max(sapply(network_component_list,vcount))
gcc <- network_component_list[[gcc_index]]
vcount(gcc)

plot(gcc, main = "Greatest Connected Component",vertex.size=1,vertex.label=NA)

## Question 2

# Degree distribution of the GCC

# in-degree

plot(degree.distribution(gcc, mode = "in"),main = "In-degree distribution of the GCC")

# out-degree

plot(degree.distribution(gcc, mode = "out"),main = "Out-degree distribution of the GCC")


## Question 3

# Option 1

gcc_undirected_1 <- as.undirected(gcc,mode="each")
gcc_undirected_1_comm <- label.propagation.community(gcc_undirected_1)
modularity(gcc_undirected_1_comm)
print(sizes(gcc_undirected_1_comm))

# Option 2

gcc_undirected_2 <- as.undirected(gcc,mode = "collapse",edge.attr.comb = list(weight = "prod"))
E(gcc_undirected_2)$weight <- sqrt(E(gcc_undirected_2)$weight)

gcc_undirected_2_lpc_comm <- label.propagation.community(gcc_undirected_2)
modularity(gcc_undirected_2_lpc_comm)
print(sizes(gcc_undirected_2_lpc_comm))

gcc_undirected_2_fg_comm <- fastgreedy.community(gcc_undirected_2)
modularity(gcc_undirected_2_fg_comm)
print(sizes(gcc_undirected_2_fg_comm))