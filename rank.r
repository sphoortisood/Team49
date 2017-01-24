setwd("/home/iob/Sphoorti_Work/R_practice")
trans1 <-read.table("Transcript_before_norm.txt",header=TRUE,sep = "\t")
expr <- as.matrix(trans1[,-c(1)])
newExpr<- apply(expr, 2, function(y) rank(y) / length(y))
write.table(newExpr,file="s1Trans.txt",sep="\t")
prote <-read.table("protein.txt",header=TRUE,sep = "\t")
expr1 <- as.matrix(prote[,-c(1)])
newExpr1<- apply(expr1, 2, function(y) rank(y) / length(y))
write.table(newExpr1,file="s1Prot.txt",sep="\t")
####################################################################################
v1 <- sapply(1:ncol(expr), function(i){cor(expr[,i],expr1[,i],method = "pearson")})
write.table(v1,file="WithoutRankCore.txt",sep="\t")
ncol(v1)
nrow(v1)
head(v1)
class(v1)
colnames(v1)
write(v1,file="corre.txt",sep = "\n")
med <- median(v1,na.rm=TRUE)
m1<-round(med,2)
m1
men <- mean(v1,na.rm=TRUE)
m2 <- round(men,2)
m2

v11 <- sapply(1:ncol(expr),function(i){cor(expr[,i],expr1[,i],method ="pearson" )})
head(v11)

print(v11)




######################################################################################
t <- cor(newExpr,newExpr1)
ttt <- round(t,2)
write.table(ttt,file="coormatonTrnaslogwhole.txt",sep = "\t")
get_upper_tri <- function(t){
  t[lower.tri(t)]<- NA
  return(t)
}
reorder_cormat <- function(t){
  # Use correlation between variables as distance
  dd <- as.dist((1-t)/2)
  hc <- hclust(dd)
  t <-t[hc$order, hc$order]
}
# Reorder the correlation matrix
t <- reorder_cormat(t)
upper_tri <- get_upper_tri(t)
upper <- round(upper_tri,2)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+ggtitle("Correlation matrix of Proteome data over 119 samples ") +labs(x="119 samples",y="119 samples")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 5, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)
write.table(upper,file="coormatontransorder.txt",sep = "\t")