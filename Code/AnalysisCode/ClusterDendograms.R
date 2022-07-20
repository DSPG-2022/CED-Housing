library(ggplot2)
library(readxl)
library(tidycensus)
library(tidyverse)
library(readr)
library(cluster)
library(ClustOfVar)
library(dendextend)
##Change To OverAll Dataset
##Dataset

Data<-read_csv("Data\\AllCountyData\\OverallDatabase.csv")

Data<-Data%>%
  mutate(HCVPercentPerHouseHold=HCVPercentPerHouseHold/100,PercentMultiFamAssistPerHouseold = PercentMultiFamAssistPerHouseold/100,PercentHudSec8Lost5Year= PercentHudSec8Lost5Year/100,
         PercentLIHTCperHousehold=PercentLIHTCperHousehold/100,Percent_515Properties=Percent_515Properties/100,PercentLILost5Year=PercentLILost5Year/100,PercentUSDAsec515Lost5Year=PercentUSDAsec515Lost5Year/100)%>%
  mutate(X5YearPctChange=X5YearPctChange/100,PctChange=PctChange/100,MultiFamHomeConstructPct=MultiFamHomeConstructPct/100,SingleFamHomeConstructPct=SingleFamHomeConstructPct/100,AverageEvictionFilingsPer1000Households=AverageEvictionFilingsPer1000Households/1000)
##Section Data off to two categories, 
##xquant will be all columns with numeric values
## xqual will be all columns with factors (strings) values
xquant <- Data[3:ncol(Data)] #number variables

##CHANGE do not count fips or county names
xqual  <- Data[,c(1,2)]      # Value variables


##creates a tree showing similarities between the columns 
tree <- hclustvar(X.quanti = as.data.frame(xquant), NULL) 
dend <- as.dendrogram(tree)

dend_labels <- labels(dend)
plot(dend)
text(x = 1:length(dend_labels), labels = dend_labels, srt = 90, adj = c(1,1), xpd = T)
plot(tree)


##sees at which number of clusters data grouped the ebst
stab <- stability(tree, B=50)


##clusters the indicators into similar groups 
##number of clusters ==init , choose number based on stab
k.means <- kmeansvar(as.data.frame(xquant),NULL,init=21)
summary(k.means)
k.means$cluster


##finds similarities by rows (counties)
d <- daisy(xquant, metric="gower") ##use metric "gower" if have factor values

##not used 
fit <- hclust(d=d, method="complete")    # Also try: method="ward.D"   
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=10)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=4, border="red")

##can change size of groups
##clusters counties into groups based on similar values
kfit <- kmeans(d,7)
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)




##appends cluster to dataset
Data[,"Cluster"]<- kfit$cluster
##saves
write.csv(Data,"Code\\TestCode\\Cory\\ClusterTesting.csv")
