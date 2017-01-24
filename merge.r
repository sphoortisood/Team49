setwd("/home/iob/Desktop/TCGA Ovarian Cancer/TCGA_36-1581-01A_23-1123-01A_24-1553-01A_Proteome_PNNL_20130518/PSMs/TSV")
mydata1 <-read.table("final1.txt", header=TRUE, sep="\t",fill=NA,row.names = NULL)
dataExp <- mydata1[,c(12,14,23,24,25,26)]
write.table(dataExp,file ="6fieldoutput.txt",sep="\t")
mydata2 <-read.table("6fieldoutput.txt", header=TRUE, sep=",")
colnames(mydata2)
keys <- colnames(mydata2)[!grepl('iTRAQ114','iTRAQ115',colnames(mydata2))]
X <- data.table(mydata2)
li<- X[,lapply(.SD,median), by=list(iTRAQ114,iTRAQ115,iTRAQ116,iTRAQ115)]
head(li)
nrow(li)
