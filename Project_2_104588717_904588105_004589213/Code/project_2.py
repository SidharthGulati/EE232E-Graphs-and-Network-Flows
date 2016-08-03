# Code for project 2

import time
import re

# reading text file
text_file_1 = open("actress_movies.txt", "r")
text_file_2 = open("actor_movies.txt", "r")
lines1 = text_file_1.read().split('\n')
lines2 = text_file_2.read().split('\n')

data = list()

for i in xrange(0,len(lines1)):
    dict = {}
    l = lines1[i].split('\t\t')
    if (len(l[1:]) >= 5):
        dict['actor'] = l[0]
        dict['movie'] = set(l[1:])
        data.append(dict)

for i in xrange(0,len(lines2)):
    dict = {}
    l = lines2[i].split('\t\t')
    if (len(l[1:]) >= 5):
        dict['actor'] = l[0]
        dict['movie'] = set(l[1:])
        data.append(dict)

new_file = open('edge_list.txt','w')

for i in xrange(0,len(data)):
    print i
    for j in xrange(i+1, len(data)):
        w_ij = float(len(data[i]['movie'] & data[j]['movie']))/float(len(data[i]['movie']))
        w_ji = float(len(data[i]['movie'] & data[j]['movie']))/float(len(data[j]['movie']))

        if w_ij > 0.0:
            new_file.write(str(data[i]['actor'])+"\t\t"+str(data[j]['actor'])+"\t\t"+str(w_ij)+"\n")
            new_file.write(str(data[j]['actor'])+"\t\t"+str(data[i]['actor'])+"\t\t"+str(w_ji)+"\n")
