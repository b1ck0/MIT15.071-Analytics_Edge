census = read.csv("census.csv")

library(caTools)
set.seed(2000)
section = sample.split(census$over50k, SplitRatio = 0.6)

train = subset(census, section == TRUE)
test = subset(census, section == FALSE)

##  build a logistic regression model to predict the dependent variable "over50k", using all of the other variables in the dataset as 
## independent variables. Which variables are significant, or have factors that are significant?

LogModel1 = glm(over50k ~ .,data = train, family = binomial)
summary(LogModel1)

## What is the accuracy of the model on the testing set? Use a threshold of 0.5. 
LogModel1.pred = predict(LogModel1, newdata = test, type = "response")

a = table(test$over50k, LogModel1.pred >= 0.5)
(a[1,1]+a[2,2])/sum(a) #accuracy

## What is the baseline accuracy for the testing set?
table(test$over50k)
9713/nrow(test)

## What is the area-under-the-curve (AUC) for this model on the test set?
library(ROCR)

ROCRpred = prediction(LogModel1.pred, test$over50k)
ROCRpred.perf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRpred.perf)
as.numeric(performance(ROCRpred, "auc")@y.values)

## Let us now build a classification tree to predict "over50k".
Tree1 = rpart(over50k ~ .,data=train, method="class")
prp(Tree1)

## What is the accuracy of the model on the testing set? Use a threshold of 0.5.
Tree1.pred = predict(Tree1, newdata = test, type = "class")

a = table(test$over50k, Tree1.pred)
(a[1,1]+a[2,2])/sum(a) #accuracy

## Plot the ROC curve for the CART model you have estimated.
## What is the AUC of the CART model on the test set?
Tree1.pred = predict(Tree1, newdata = test)

ROCRpred2 = prediction(Tree1.pred[,2], test$over50k)
ROCRpred2.perf = performance(ROCRpred2, "tpr", "fpr")
plot(ROCRpred2.perf)
as.numeric(performance(ROCRpred2, "auc")@y.values)

## Build random forest
## What is the accuracy of the model on the test set, using a threshold of 0.5?
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]

set.seed(1)
Forest1 = randomForest(over50k ~ .,data=trainSmall)

Forest1.pred = predict(Forest1, newdata = test)

a = table(test$over50k, Forest1.pred)
(a[1,1]+a[2,2])/sum(a) #accuracy

## Which variable is the most important in terms of the number of splits?
vu = varUsed(Forest1, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(Forest1$forest$xlevels[vusorted$ix]))

library(ggplot2)
ggplot() + 
    geom_point(mapping = aes(y = names(Forest1$forest$xlevels[vusorted$ix]), x = vusorted$x))

## Which variable is the most important in terms of mean reduction in impurity?
varImpPlot(Forest1)

## cross-validation
set.seed(2)
numFolds = trainControl(number = 50 )
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))

train(over50k ~ ., data = train, method = "rpart", trControl = numFolds, tuneGrid = cartGrid)

## Fit a CART model to the training data using this value of cp. What is the prediction accuracy on the test set?
Tree2 = rpart(over50k ~ .,data=train, method = "class", cp = 0.002)
prp(Tree2)

Tree2.pred = predict(Tree2, newdata = test, type = "class")

a = table(test$over50k, Tree2.pred)
(a[1,1]+a[2,2])/sum(a) #accuracy
