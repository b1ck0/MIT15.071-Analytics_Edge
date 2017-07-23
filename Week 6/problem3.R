## Load StocksCluster.csv into a data frame called "stocks". How many observations are in the dataset?
stocks = read.csv("StocksCluster.csv")
nrow(stocks)

## What proportion of the observations have positive returns in December?
mean(stocks$PositiveDec == TRUE)

## What is the maximum correlation between any two return variables in the dataset?
cor(stocks)

## Which month (from January through November) has the largest mean return across all observations in the dataset?
## Which month (from January through November) has the smallest mean return across all observations in the dataset?
summary(stocks)

## Initial Logistic Regression Model
library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)

StocksModel = glm(PositiveDec ~ ., data = stocksTrain, family=binomial)

## What is the overall accuracy on the training set, using a threshold of 0.5?
StocksModel.pred = predict(StocksModel, type = "response")
table(stocksTrain$PositiveDec, StocksModel.pred >= 0.5)
(990+3640)/length(StocksModel.pred)

## Now obtain test set predictions from StocksModel. 
## What is the overall accuracy of the model on the test, again using a threshold of 0.5?
StocksModel.predTest = predict(StocksModel, newdata = stocksTest, type = "response")
table(stocksTest$PositiveDec, StocksModel.predTest >= 0.5)
(417+1553)/length(StocksModel.predTest)

## What is the accuracy on the test set of a baseline model that always predicts the most common outcome (PositiveDec = 1)?
table(stocksTest$PositiveDec)
1897/(1897+1577)

## Clustering Stocks
limitedTrain = stocksTrain
limitedTest = stocksTest
limitedTrain$PositiveDec = NULL
limitedTest$PositiveDec = NULL

## Normalizing the Data
library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)

mean(normTrain$ReturnJan)
mean(normTest$ReturnJan)

## K-means clustering in 3 clusters
set.seed(144)
km = kmeans(normTrain, centers = 3)
table(km$cluster)

library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
table(clusterTest)

stocksTrain1 = subset(stocksTrain, clusterTrain == 1)
stocksTrain2 = subset(stocksTrain, clusterTrain == 2)
stocksTrain3 = subset(stocksTrain, clusterTrain == 3)

stocksTest1 = subset(stocksTest, clusterTest == 1)
stocksTest2 = subset(stocksTest, clusterTest == 2)
stocksTest3 = subset(stocksTest, clusterTest == 3)

mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)

## LOgistics Regression on the clusters
StocksModel1 = glm(PositiveDec ~ ., data = stocksTrain1, family = binomial)
StocksModel2 = glm(PositiveDec ~ ., data = stocksTrain2, family = binomial)
StocksModel3 = glm(PositiveDec ~ ., data = stocksTrain3, family = binomial)

StocksModel1$coefficients
StocksModel2$coefficients
StocksModel3$coefficients

PredictTest1 = predict(StocksModel1, newdata = stocksTest1, type = "response")
PredictTest2 = predict(StocksModel2, newdata = stocksTest2, type = "response")
PredictTest3 = predict(StocksModel3, newdata = stocksTest3, type = "response")

table(stocksTest1$PositiveDec, PredictTest1 > 0.5)
(30+774)/(30+774+23+471)

table(stocksTest2$PositiveDec, PredictTest2 > 0.5)
(388+757)/(388+757+626+309)

table(stocksTest3$PositiveDec, PredictTest3 > 0.5)
(49+13)/(49+13+13+21)

AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)

table(AllPredictions  > 0.5, AllOutcomes)
(1544+467)/(467+353+1110+1544)
