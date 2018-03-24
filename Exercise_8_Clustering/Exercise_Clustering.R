library(cluster)
library(rattle.data)
library(NbClust)
library(flexclust)

data(wine, package="rattle.data")
head(wine)

df <- scale(wine[-1])

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}

wssplot(df)

# There is a distinct drop in within groups sum of squares when moving from 1 to 3 clusters. 
# After three clusters, this decrease drops off, suggesting that a 3-cluster 
# solution may be a good fit to the data. 

library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

# The NbClust package suggest a 3-cluster solution.


fit.km <- kmeans(df, 3, nstart=25)
fit.km$size

fit.km$centers 
aggregate(wine[-1], by=list(cluster=fit.km$cluster), mean)

ct.km <- table(wine$Type, fit.km$cluster)
ct.km  

clusplot(pam(df,3))

randIndex(ct.km)
# The adjusted Rand index provides a measure of the agreement between two partitions. 
# It ranges from -1 (no agreement) to 1 (perfect agreement). 
# Agreement between the wine varietal type and the cluster solution is 0.9.