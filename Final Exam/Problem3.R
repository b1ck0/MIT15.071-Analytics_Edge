# UNDERSTANDING GROCERY SHOPPING BEHAVIOR

## In Unit 6, we saw how clustering can be used for market segmentation, the idea of dividing airline passengers into small, more similar groups, 
## and then designing a marketing strategy specifically for each group.  In this problem, we'll see how this idea can be applied to online grocery 
## order data.
## In this problem, we'll use the dataset from Instacart.com (https://www.instacart.com/datasets/grocery-shopping-2017), a grocery delivery service 
## that connects customers with Personal Shoppers who pick up and deliver the groceries from local stores. The open data contains order, product, 
## and aisles detailed information. In the data we prepared, each row (observation) represents a unique order, where the different product information 
## was aggregated. 

setwd("C:/Users/vay/Documents/GitHub/MIT15.071-Analytics_Edge/Final Exam")
orders = read.csv("orders.csv")
str(orders)

## What time of day are most orders placed?
table(orders$order_hour_of_day)

## What is the average days since prior order?
mean(orders$days_since_prior_order)

## What's the correlation between the orders of "fresh.fruits" and "fresh.vegetables"?
cor(orders$fresh.fruits, orders$fresh.vegetables)

## In the dataset, what proportion of orders have at least one item from the frozen.pizza aisle?
sub1 = subset(orders, frozen.pizza >= 1)
nrow(sub1)/nrow(orders)

## Normalizing the Data
orders.aisle = orders[, 5:ncol(orders)]
library(caret)
preproc = preProcess(orders.aisle)
ordersNorm = predict(preproc, orders.aisle)

## What is the maximum value of frozen.dessert after normalization?
max(ordersNorm$frozen.dessert)

## What is the minimum value of soft.drinks in the normalized dataset?
min(ordersNorm$soft.drinks)

## Creating dendogram of the data
distances <- dist(ordersNorm, method = "euclidean")
ClusterProducts <- hclust(distances, method = "ward.D")
plot(ClusterProducts, labels = FALSE)

## k-means clustering
set.seed(200)
k = 4
Kcluster = kmeans(ordersNorm, centers = k)
clusterGroups = Kcluster$cluster
Kclust.1 = subset(ordersNorm, clusterGroups == 1)
Kclust.2 = subset(ordersNorm, clusterGroups == 2)
Kclust.3 = subset(ordersNorm, clusterGroups == 3)
Kclust.4 = subset(ordersNorm, clusterGroups == 4)

## How many observations are in the smallest cluster?
nrow(Kclust.4)

## How many observations are in the largest cluster?
nrow(Kclust.3)

tail(sort(colMeans(Kclust.1)))
tail(sort(colMeans(Kclust.2)))
tail(sort(colMeans(Kclust.3)))
tail(sort(colMeans(Kclust.4)))

## Which cluster best fits the description "orders mostly consistents of cleaning supplies, beauty, and some pantry foods"?
### Cluster 1 [CORRECT]

## Which cluster best fits the description "frozen desserts"?
### Cluster 4 [CORRECT]

## Which cluster on average has the smallest amount of items ordered?


## Which cluster has the latest average hour of the day?
tapply(orders$order_hour_of_day, clusterGroups == 1, mean)
tapply(orders$order_hour_of_day, clusterGroups == 2, mean)
tapply(orders$order_hour_of_day, clusterGroups == 3, mean)
tapply(orders$order_hour_of_day, clusterGroups == 4, mean)

## Which cluster has the longest average days since prior order?
tapply(orders$days_since_prior_order, clusterGroups == 1, mean)
tapply(orders$days_since_prior_order, clusterGroups == 2, mean)
tapply(orders$days_since_prior_order, clusterGroups == 3, mean)
tapply(orders$days_since_prior_order, clusterGroups == 4, mean)

## Which of the following visualizations could be used to observe the distribution of days_since_prior_order, broken down by cluster?
### A box plot of the variable days_since_prior_order, subdivided by cluster [MAYBE]
### A box plot of the clusters, subdivided by days_since_prior_order values [NO NO NO]
### ggplot with the cluster number on the x-axis and days_since_prior_order on the y-axis, plotting with geom_histogram() [ NO ]
### ggplot with the cluster number on the x-axis and days_since_prior_order on the y-axis, cluster number as group, plotting with geom_boxplot() [ YES ]
orders$cluster = clusterGroups

boxplot(days_since_prior_order ~ cluster, data = orders)
boxplot(cluster ~ days_since_prior_order, data = orders)

library(ggplot2)
ggplot() + 
    geom_histogram(mapping = aes(x = orders$cluster, y = orders$order_hour_of_day))
