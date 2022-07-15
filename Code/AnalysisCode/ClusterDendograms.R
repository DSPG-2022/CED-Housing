library(ggplot2)
library(readxl)
library(tidycensus)
library(tidyverse)
library(readr)
library(cluster)
library(ClustOfVar)

##Change To OverAll Dataset
##Dataset
oldData<- read_excel("R\\Cory\\indicator_summary_table_example.xlsx")


##NOT IMPORTANT, USED TO FORMAT OLD DATA
names(oldData)
oldData<-oldData[1:100,]

##Section Data off to two categories, 
##xquant will be all columns with numeric values
## xqual will be all columns with factors (strings) values
xquant <- oldData[3:ncol(oldData)] #number variables

##CHANGE do not count fips or county names
xqual  <- oldData[,c(1,2)]      # Value variables


##creates a tree showing similarities between the columns 
tree <- hclustvar(X.quanti = as.data.frame(xquant), X.quali = NULL )
plot(tree)


##sees at which number of clusters data grouped the ebst
stab <- stability(tree, B=50)


##clusters the indicators into similar groups 
##number of clusters ==init , choose number based on stab
k.means <- kmeansvar(as.data.frame(xquant),NULL,init=21)
summary(k.means)
k.means$cluster


##finds similarities by rows (counties)
d <- daisy(xquant, metric="euclidean") ##use metric "gower" if have factor values

##not used 
fit <- hclust(d=d, method="complete")    # Also try: method="ward.D"   
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=10)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=4, border="red")

##can change size of groups
##clusters counties into groups based on similar values
kfit <- kmeans(d, 7)
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)




##appends cluster to dataset
oldData[,"Cluster"]<- kfit$cluster
##saves
write.csv(oldData,"R\\Cory\\ClusterTesting.csv")
