library(reshape2)
library(ggplot2)
library(gplots)
library(plyr)
library(RColorBrewer)
setwd("/home/iob/Sphoorti_Work/R_practice")
trans1 <- read.table("tra.txt",header=TRUE,sep = "\t")
w1=table(trans1$First)
w2=table(trans1$Second)
D <-matrix(nrow=length(w1),ncol=length(w2)) 
colnames(D) <- paste("ClusterT", 1:length(w1), sep = "")
rownames(D) <- paste("ClusterP", 1:length(w2), sep = "")
for(i in 1:length(w1)){
  for(j in 1:length(w1)){
    value1 <-trans1[trans1$First==i,1]
    length(value1)
    value2 <- trans1[trans1$Second==j,1]
    length(value2)
    common <- intersect(value1,value2)
    length(common)
    per <- length(value1)-length(common)+length(value2)
    D[i,j] <-((length(common) / per)*100)
  }
}
val1 <- which(D == max(D), arr.ind = TRUE)
vall <-trans1[trans1$First==val1[1],1]
valu <- trans1[trans1$Second==val1[2],1]
comm <- intersect(vall,valu)
tt1 <-read.table("t1.txt",header=TRUE,sep = "\t")
tt2 <-read.table("t2.txt",header=TRUE,sep = "\t")
datExpr0 =as.matrix(tt1[,-c(1)])
ncol(datExpr0)
row.names(datExpr0)<-tt1$Genes
nrow(datExpr0)
datExpr1 =as.matrix(tt2[,-c(1)])
ncol(datExpr1)
row.names(datExpr1)<-tt2$Genes
nn1 <- datExpr0[match(comm, rownames(datExpr0)),, drop=FALSE]
nn2 <- datExpr1[match(comm, rownames(datExpr1)),, drop=FALSE]
write.table(nn1,file = "Protein_Genes.txt",sep="\t")
write.table(nn2,file = "Transcriptome_Genes.txt",sep="\t")
mdat <- melt(as.matrix(D))
head(mdat)
ggplot(mdat, aes(Var1, value,fill=Var2)) + geom_bar(stat="identity", position = "dodge",colour="white") + 
scale_fill_brewer(name="Transcriptome",palette = "Set1")+
ggtitle("Percentage of common genes between Transcriptome and Proteome clusters ") +labs(x="Proteomic Clusters",y="Percentage")








