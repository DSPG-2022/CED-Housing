library(ggplot2)
library(readxl)
library(tidycensus)
library(tidyverse)
library(readr)
library(cluster)
library(ClustOfVar)


oldData<- read_excel("R\\Cory\\indicator_summary_table_example.xlsx")

names(oldData)
oldData<-oldData[1:100,]

xquant <- oldData[3:ncol(oldData)]%>%
  
xqual  <- oldData[,c(1,2)]      # Value variables

tree <- hclustvar(X.quanti = as.data.frame(xquant), X.quali = NULL )
plot(tree)


stab <- stability(tree, B=50)


k.means <- kmeansvar(as.data.frame(xquant),NULL,init=21)

summary(k.means)

k.means$cluster



d <- daisy(xquant, metric="euclidean")

fit <- hclust(d=d, method="complete")    # Also try: method="ward.D"   
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=4)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=4, border="red")

##can change size of groups
kfit <- kmeans(d, 4)
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)


kfit$cluster
s


oldData[,"Cluster"]<- kfit$cluster

write.csv(oldData,"R\\Cory\\ClusterTesting.csv")
