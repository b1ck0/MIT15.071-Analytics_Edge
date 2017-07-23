## Read the dataset AirlinesCluster.csv into R and call it "airlines".
airlines = read.csv("AirlinesCluster.csv")

## Looking at the summary of airlines, which TWO variables have (on average) the smallest values?
summary(airlines)

## Which TWO variables have (on average) the largest values?
summary(airlines)

## Normalizing the Data
## In the normalized data, which variable has the largest maximum value?
library(caret)
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm)

## Compute the distances between data points (using euclidean distance) and then run the Hierarchical clustering algorithm (using method="ward.D") 
## on the normalized data. Then, plot the dendrogram of the hierarchical clustering process.
distance = dist(airlinesNorm, method = "euclidean")
Hcluster = hclust(distance, method = "ward.D")
plot(Hcluster)

## Suppose that after looking at the dendrogram and discussing with the marketing department, the airline decides to proceed with 5 clusters. 
## Divide the data points into 5 clusters by using the cutree function. How many data points are in Cluster 1?
clusterGroups = cutree(Hcluster, k = 5)
sum(clusterGroups == 1)

## Compared to the other clusters, Cluster 1 has the largest average values in which variables (if any)?
## Compared to the other clusters, Cluster 2 has the largest average values in which variables (if any)?
## Compared to the other clusters, Cluster 3 has the largest average values in which variables (if any)?
## Compared to the other clusters, Cluster 4 has the largest average values in which variables (if any)? 
## Compared to the other clusters, Cluster 5 has the largest average values in which variables (if any)? 
tapply(airlines$Balance, clusterGroups, mean)
tapply(airlines$QualMiles, clusterGroups, mean)
tapply(airlines$BonusMiles, clusterGroups, mean)
tapply(airlines$BonusTrans, clusterGroups, mean)
tapply(airlines$FlightMiles, clusterGroups, mean)
tapply(airlines$FlightTrans, clusterGroups, mean)

## K-Means Clustering
## Now run the k-means clustering algorithm on the normalized data, again creating 5 clusters. 
## Set the seed to 88 right before running the clustering algorithm, and set the argument iter.max to 1000.
k = 5
set.seed(88)
Kcluster = kmeans(airlinesNorm, centers = k, iter.max = 1000)
clusterGroups = Kcluster$cluster

## How many clusters have more than 1,000 observations?
table(clusterGroups)
