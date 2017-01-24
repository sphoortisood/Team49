library("cluster")
setwd("/home/iob/Sphoorti_Work/R_practice")
trans1 <- read.table("t1.txt",header=TRUE,sep = "\t")
trans2 <- read.table ("t2.txt", header=TRUE,sep = "\t")
datExpr0 =as.matrix(trans1[,-c(1)])
ncol(datExpr0)
row.names(datExpr0)<-trans1$Genes
nrow(datExpr0)
ncol(trans2)
datExpr1 =as.matrix(trans2[,-c(1)])
ncol(datExpr1)
nrow(datExpr1)
row.names(datExpr1)<-trans2$Genes
wss <- (nrow(datExpr1)-1)*sum(apply(datExpr1,2,var))
for(i in 2:20)wss[i] <- sum(kmeans(datExpr1,centers = i)$withinss)
plot(1:20,wss,type = "b")
set.seed(20)
kc <- kmeans(datExpr0,3,nstart=20)
tree<- as.matrix(kc$cluster)
colnames(tree) <- "ClusterNumber"
clusplot(datExpr0, kc$cluster,col.p=kc$cluster,cex = 1, cex.txt ="cyan",shade=FALSE,
         labels=5,lines =2)
write.table(kc$cluster,file = "clusterProt.txt",sep="\t")
