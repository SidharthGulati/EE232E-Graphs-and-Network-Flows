# Code for project 2

import time
import re
import itertools
import pickle

# reading text file
text_file_1 = open("actress_movies.txt", "r")
text_file_2 = open("actor_movies.txt", "r")

lines1 = text_file_1.read().split('\n')
lines2 = text_file_2.read().split('\n')

################################################################################

print "Creating a [MOVIE-TO-ACTOR] dictionary ...."

movie_to_actor = dict()

for i in xrange(0,len(lines1)):

    l = lines1[i].split('\t\t')
    for movie in l[1:]:
        if movie in movie_to_actor:
            movie_to_actor[movie].append(l[0])
        else:
            movie_to_actor[movie] = [l[0]]


for i in xrange(0,len(lines2)):

    l = lines2[i].split('\t\t')
    for movie in l[1:]:
        if movie in movie_to_actor:
            movie_to_actor[movie].append(l[0])
        else:
            movie_to_actor[movie] = [l[0]]


print(len(movie_to_actor))

################################################################################
# Removing movies with less than 5 actors

print "Removing MOVIES with LESS than 5 ACTORS ...."

for k in movie_to_actor.keys():

    if (len(movie_to_actor[k]) < 5):
        del movie_to_actor[k]

print (len(movie_to_actor))

################################################################################
# Creating an actor to movie dictionary

print "Creating an [ACTOR-TO-MOVIE] dictionary"

actor_to_movie = dict()

for movie in movie_to_actor:
    for actor in movie_to_actor[movie]:
        if actor in actor_to_movie:
            actor_to_movie[actor].append(movie)
        else:
            actor_to_movie[actor] = [movie]

################################################################################
print (len(actor_to_movie))


print "Creating [MOVIE_PAIR_EDGE_WEIGHT] dictionary .... "

movie_pair_edge_weight = dict()

for count,actor in enumerate(actor_to_movie):
    print count
    movie_list = actor_to_movie[actor]
    pairs = itertools.combinations(movie_list,2)
    for t in pairs:
        if t in movie_pair_edge_weight:
            movie_pair_edge_weight[t] += 1
        else:
            movie_pair_edge_weight[t] = 1
pickle.dump( movie_pair_edge_weight, open( "movie_pair_edge_weight.p", "wb" ) )
new_file = open("movie_jaccard_index.txt","w")

################################################################################

movie_pair_edge_weight=pickle.load( open( "movie_pair_edge_weight.p", "rb" ) )

print "CREATING THE FILE..."

count = 0

for key,value in movie_pair_edge_weight.iteritems():
    count = count + 1
    print count

    jaccard_numerator = value

    union_ij = len(movie_to_actor[key[0]]) + len(movie_to_actor[key[1]]) - jaccard_numerator
    w_ij = jaccard_numerator/union_ij

    new_file.write(str(key[0])+"\t\t"+str(key[1])+"\t\t" + str(w_ij) + "\n")
    new_file.write(str(key[1])+"\t\t"+str(key[0])+"\t\t" + str(w_ij) + "\n")

################################################################################
