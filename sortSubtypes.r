library(ConsensusClusterPlus)
library(ALL)
data(ALL)
d=exprs(ALL)
d[1:5,1:5]
mads=apply(d,1,mad)
d=d[rev(order(mads))[1:5000],]
d = sweep(d,1, apply(d,1,median,na.rm=T))

library(ConsensusClusterPlus)
results = ConsensusClusterPlus(d,maxK=6,reps=50,pItem=0.8,pFeature=1,clusterAlg="hc",distance="pearson",seed=1262118388.71279)
