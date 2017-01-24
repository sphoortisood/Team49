getwd()
library(ggplot2)
setwd("/home/iob/Sphoorti_Work/R_practice")
trans1 <- read.table("protein.txt",header=TRUE,sep = "\t")
trans2 <-read.table("trans.txt", header=TRUE,sep = "\t")
ncol(trans1)
datExpr0 =as.matrix(trans1[,-c(1)])
ncol(datExpr0)
ncol(trans2)
datExpr1 =as.matrix(trans2[,-c(1)])
ncol(datExpr1)
v1 <- sapply(1:nrow(datExpr0), function(i){cor(datExpr0[i,],datExpr1[i,],method = "spearman")})
head(v1)
l1=cbind(a = as.character(trans1[,1]), v1)
colnames(l1)<-c("Genes","Correlation")
write.table(l1,file="correlation.txt",sep = ",")
med <- median(v1,na.rm=TRUE)
m1<-round(med,2)
men <- mean(v1,na.rm=TRUE)
m2 <- round(men,2)
v11 <- melt(v1)
ggplot(v11,aes(x=value))+ geom_histogram(binwidth=.025,colour="black",fill=I("blue"),alpha=I(.2),aes(y=..density..))+
geom_density(fill=NA, colour="black")+
ggtitle("Genes correlation between Transcriptome and Proteome data ") +labs(x="Spearman Correlation",y="frequency")+
geom_vline(aes(xintercept=m1,colour="median"),linetype="dashed", size=.5,show.legend =TRUE)+
geom_vline(aes(xintercept=m2,colour="mean"), linetype="dashed", size=.5,show.legend = TRUE)+
geom_text(aes(label = m1),x=m1,y=1.7,size=6,color="blue")+
geom_text(aes(label= m2),x=m2,y=1.5,size=6,color="red")+
scale_color_manual(name = "Statistics", values = c(median ="blue",mean = "red"))+
theme(legend.key = element_rect(color = "black"))






