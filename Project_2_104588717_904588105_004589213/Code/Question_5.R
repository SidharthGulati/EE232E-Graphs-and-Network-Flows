#!/usr/bin/env Rscript
library("igraph")
library("stringr")
library(plyr); 
library(data.table)


comm_fg <- fastgreedy.community(network_mov_simp)

print(length(comm_fg))
print(modularity(comm_fg))

system("cat movie_genre.txt | awk -F\"\t\t\" '{print $1\"|\"$2}' > movie_genre_pipe.txt" )
genre_txt <- "movie_genre_pipe.txt"
movie_genre <- fread(genre_txt,header = FALSE, sep="|")
genre_list <- c()
count=1
for (i in 1:vcount(network_mov_simp)){
  if(stri_enc_isutf8(V(network_mov_simp)$name[i])){
    movie_name = str_trim(V(network_mov_simp)$name[i])
  }
  if (movie_name %in% movie_genre$V1){
    genre <- movie_genre$V2[which(movie_genre$V1 == movie_name)]
  }else{
    genre <- "NA"
  }
  genre_list <- rbind(genre_list,genre)
  count <- count +1;
  print(count)
}
genre_of_comm <- array(data = 0)
for (i in 1:length(comm_fg)){
  print(i)
  size_comm_i <- sizes(comm_fg)[i]
  genre_comm_i <- data.frame(genre_list[which(comm_fg$membership==i),1])
  colnames(genre_comm_i) <- "genre"
  genere_in_comm <- as.data.frame(table(genre_comm_i))
  rownames(genere_in_comm) <- genere_in_comm$genre_comm_i
  if (max(genere_in_comm$Freq) > 0.2*size_comm_i){
    index <- which((genere_in_comm$Freq==max(genere_in_comm$Freq)))[1]
    genre_of_comm[i] <-rownames(genere_in_comm[index,])
  }else{
    index <-which((genere_in_comm$Freq >  0.2*size_comm_i))[1]
    genre_of_comm[i] <-rownames(genere_in_comm[index,])
  }
}
save(genre_of_comm,file = 'genre_of_comm.Rdata')