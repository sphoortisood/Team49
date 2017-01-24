getwd()
library(ggplot2)
library(reshape2)
setwd("/home/iob/Sphoorti_Work/R_practice")
setwd("/home/iob/Sphoorti_Work/colorectal/coloRprogram")
trans1 <-read.table("prot.txt",header=TRUE,sep = "\t")
trans2 <-read.table("transZscore.txt", header=TRUE, sep = "\t")
ncol(trans1)
datExpr0 =as.matrix(trans1[,-c(1)])
ncol(datExpr0)
nrow(datExpr0)
ncol(trans2)
datExpr1 =as.matrix(trans2[,-c(1)])
ncol(datExpr1)
nrow(datExpr1)
t <- cor(datExpr0,datExpr1)
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
  geom_tile(color = "white")+ggtitle("Correlation matrix of transcriptome(z-score) data over 83 samples ") +labs(x="83 samples",y="83 samples")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = .0, space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 5, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)
write.table(upper,file="coormatontransorder.txt",sep = "\t")




































