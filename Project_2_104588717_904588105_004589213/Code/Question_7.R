#!/usr/bin/env Rscript
library("igraph")
library("stringr")
library(plyr); 

system("cat movie_rating.txt | awk -F\"\t\t\" '{print $1\"|\"$2}' > movie_rating_pipe.txt" )
genre_txt <- "movie_rating_pipe.txt"
movie_rating <- fread(genre_txt,header = FALSE, sep="|")
rating_list <- c() 

edgelistFile = "movie_jaccard_index.txt"
data_edge <- do.call(rbind,strsplit(readLines(edgelistFile),'\t\t',fixed=T))
g <- graph.data.frame(data_edge, directed=FALSE)
E(g)$weight <-as.double(data_edge[,3])
E(g)$weight <- E(g)$weight/2
g <- simplify(g)
comm_fg <- fastgreedy.community(g)
count <-0
for (i in 1:vcount(g)){
  movie_name = str_trim(V(g)$name[i])
  if (movie_name %in% movie_rating$V1){
    rating <- movie_rating$V2[which(movie_rating$V1 == movie_name)]
  }else{
    rating <- 0
  }
  rating_list <- rbind(rating_list,rating)
  count <- count +1;
  print(count)
  print(rating)
}

mov_list <- c("Mr. Smith Goes to Washington (1939)  ","Killer McCoy (1947)  ","The Stranger Wore a Gun (1953)  ")
weight_list = {}
neighbor_list = {}

mov_list <- str_trim(mov_list)
movie_in_graph <- str_trim(V(g)$name)
prediction_of_rating_list <- c()
for(movie in mov_list){
  tmp_ind <- which(movie_in_graph==  movie)
  neighbor_list <- str_trim(neighbors(g,tmp_ind)$name)
  index <- c()
  for (i in 1:length(neighbor_list)){
    if(neighbor_list[i] %in% movie_rating$V1 ){
      index[i] <- which(movie_in_graph == neighbor_list[i])
    }
  }
  rating_movie_neighbor <- rating_list[index]
  rating_movie_neighbor <- rating_movie_neighbor[is.finite(rating_movie_neighbor)] 
  rating_pred_i <- mean(rating_movie_neighbor)
  prediction_of_rating_list <- append(prediction_of_rating_list,rating_pred_i)
}
