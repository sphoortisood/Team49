getwd()
library(ggplot2)
library(reshape2)
setwd("/home/iob/Sphoorti_Work/R_practice")
trans1 <-read.table("trans.txt",header=TRUE,sep = "\t")
ncol(trans1)
datExpr1 =as.matrix(trans1[,-c(1)])
ncol(datExpr1)
nrow(datExpr1)
t <- cor(datExpr1)
head(t)
