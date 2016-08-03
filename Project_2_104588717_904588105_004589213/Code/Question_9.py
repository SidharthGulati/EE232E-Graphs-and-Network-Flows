import pickle
m2a=pickle.load(open("mov_act.pickle",'r'))
rating=pickle.load(open("ratings_all.pickle",'r'))
a2r=dict()
key=list(m2a.keys())
rkeys=list(rating.keys())

#print(key[1:10])
#print("RATING")
#print(rkeys[1:10])

for movie in key:
	if movie in rating:
		rval=rating[movie]
        	for actor in m2a[movie]:
                	if actor not in a2r:
				a2r[actor]=[]
				a2r[actor].append(rval)
			else:
				a2r[actor].append(rval)

print("Length of the all map dictionary",len(a2r))

a2v=dict()
key1=a2r.keys()
for actor in key1:
	tmp=a2r[actor]
	#a2v[actor]=1.0*(sum(tmp)/len(tmp))
	#a2v[actor]=1.0*max(tmp)
	t1=1.0*(sum(tmp)/len(tmp))
	t2=1.0*max(tmp)
	a2v[actor]=0.2*t2+0.8*t1
movielist=["Batman v Superman: Dawn of Justice (2016)","Mission: Impossible - Rogue Nation (2015)","Minions (2015)"]

for i in movielist:
	if i in key:
		a=m2a[i]
		x=0
		count=0
		for j in a:
			if j in a2v:
				x=x+a2v[j]
				count=count+1
		if(count!=0):
			print("Number of actors are ",count)
			print("The rating of movie",i," is  ",x/count)
			if(i==movielist[0]):
				print("IMDb rating of the movie",i," is 7.1")
			elif(i==movielist[1]):
				print("IMDb rating of the movie",i," is 7.5")
			else:
				print("IMDb rating of the movie",i," is 6.4")
