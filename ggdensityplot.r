getwd()
library(ggplot2)
setwd("/home/iob/Sphoorti_Work/colorectal/coloRprogram")
tran1 <- read.table("t1.txt",header=TRUE,sep = "\t")
trans2 <-read.table("trans2.txt", header=TRUE,sep = "\t")
df1 <- as.data.frame(t(tran1))
names(df1) <- as.matrix(df1[1, ])
df1 <- df1[-1, ]
df1[] <- lapply(df1, function(x) type.convert(as.character(x)))
df1
df2<-stack(df1)
head(df2)
colnames(df2)<- c("values","Genes")
df2
ggplot(df2, aes(x=values,group=Genes,col=Genes,fill=Genes))+
  geom_density(alpha=0.2)+ggtitle("Genes distribution across 119 samples") +
  labs(x="log2(FPKM values)",y="frequency") 
df <- as.data.frame(tran1[,-c(1)])
df
dfs <- stack(df)
colnames(dfs)<- c("values","Samples")
ggplot(dfs, aes(x=values,group=Samples,col=Samples,fill=Samples))+
  geom_density(alpha=0.3)+ggtitle("Samples distribution across genes") +
  labs(x="log2(FPKM values)",y="frequency") 
