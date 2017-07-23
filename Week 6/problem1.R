
dailykos = read.csv("dailykos.csv", header=TRUE)
distance = dist(dailykos, method = "euclidean")
Hcluster = hclust(distance, method = "ward.D")

## Plot the dendrogram of your hierarchical clustering model. Just looking at the dendrogram, 
## which of the following seem like good choices for the number of clusters?
plot(Hcluster)

## Now, we don't really want to run tapply on every single variable when we have over 1,000 different variables. 
## Let's instead use the subset function to subset our data by cluster. Create 7 new datasets, each containing the observations from one of the clusters.
## How many observations are in cluster 3?
clusterGroups = cutree(Hcluster, k = 7)

clust.1 = subset(dailykos, clusterGroups == 1)
clust.2 = subset(dailykos, clusterGroups == 2)
clust.3 = subset(dailykos, clusterGroups == 3)
clust.4 = subset(dailykos, clusterGroups == 4)
clust.5 = subset(dailykos, clusterGroups == 5)
clust.6 = subset(dailykos, clusterGroups == 6)
clust.7 = subset(dailykos, clusterGroups == 7)

nrow(clust.3)

## What is the most frequent word in this cluster, in terms of average value?
tail(sort(colMeans(clust.1)))

## Which words best describe cluster 2?
tail(sort(colMeans(clust.2)))

## Which cluster could best be described as the cluster related to the Iraq war?
tail(sort(colMeans(clust.3)))
tail(sort(colMeans(clust.4)))
tail(sort(colMeans(clust.5)))
tail(sort(colMeans(clust.6)))
tail(sort(colMeans(clust.7)))

## Now, run k-means clustering, setting the seed to 1000 right before you run the kmeans function. Again, pick the number of clusters equal to 7. 
## Subset your data into the 7 clusters (7 new datasets) by using the "cluster" variable of your kmeans output.
k = 7
set.seed(1000)
Kcluster = kmeans(dailykos, centers = k)
clusterGroups = Kcluster$cluster
Kclust.1 = subset(dailykos, clusterGroups == 1)
Kclust.2 = subset(dailykos, clusterGroups == 2)
Kclust.3 = subset(dailykos, clusterGroups == 3)
Kclust.4 = subset(dailykos, clusterGroups == 4)
Kclust.5 = subset(dailykos, clusterGroups == 5)
Kclust.6 = subset(dailykos, clusterGroups == 6)
Kclust.7 = subset(dailykos, clusterGroups == 7)

nrow(Kclust.3)

## Now, output the six most frequent words in each cluster, like we did in the previous problem, for each of the k-means clusters.
## Which k-means cluster best corresponds to the Iraq War?
tail(sort(colMeans(Kclust.1)))
tail(sort(colMeans(Kclust.2)))
tail(sort(colMeans(Kclust.3)))
tail(sort(colMeans(Kclust.4)))
tail(sort(colMeans(Kclust.5)))
tail(sort(colMeans(Kclust.6)))
tail(sort(colMeans(Kclust.7)))

## For the rest of this problem, we'll ask you to compare how observations were assigned to clusters in the two different methods. 
## Use the table function to compare the cluster assignment of hierarchical clustering to the cluster assignment of k-means clustering.
## Which Hierarchical Cluster best corresponds to K-Means Cluster 2?
clusterGroupsH = cutree(Hcluster, k = 7)
clusterGroupsK = Kcluster$cluster
table(clusterGroupsH == 1, clusterGroupsK == 2)
table(clusterGroupsH == 2, clusterGroupsK == 2)
table(clusterGroupsH == 3, clusterGroupsK == 2)
table(clusterGroupsH == 4, clusterGroupsK == 2)
table(clusterGroupsH == 5, clusterGroupsK == 2)
table(clusterGroupsH == 6, clusterGroupsK == 2)
table(clusterGroupsH == 7, clusterGroupsK == 2)

## Which Hierarchical Cluster best corresponds to K-Means Cluster 3?
table(clusterGroupsH, clusterGroupsK)

## Which Hierarchical Cluster best corresponds to K-Means Cluster 7?
