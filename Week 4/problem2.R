library(caTools)

letters = read.csv("letters_ABPR.csv")
letters$isB = as.factor(letters$letter == "B")

set.seed(1000)
section = sample.split(letters$isB, SplitRatio = 0.5)

train = subset(letters, section == TRUE)
test = subset(letters, section == FALSE)

## Before building models, let's consider a baseline method that always predicts the most frequent outcome, which is "not B". What is the accuracy 
## of this baseline method on the test set?

table(test$isB)
(1175)/(383+1175)

## What is the accuracy of the CART model on the test set?
CARTb = rpart(isB ~ . - letter, data=train, method="class")
CARTb.pred = predict(CARTb, newdata = test, type = "class")

a = table(test$isB, CARTb.pred)

TP = a[2,2]
TN = a[1,1]
FP = a[1,2]
FN = a[2,1]

(TP+TN)/(TP+TN+FP+FN) #accuracy

## Now, build a random forest model to predict whether the letter is a B or not (the isB variable) using the training set
## What is the accuracy of the model on the test set?
library(randomForest)
set.seed(1000)

ForestModel = randomForest(isB ~ . - letter, data = train)
FOrestModel.pred = predict(ForestModel, newdata = test)

a = table(test$isB, FOrestModel.pred)

TP = a[2,2]
TN = a[1,1]
FP = a[1,2]
FN = a[2,1]

(TP+TN)/(TP+TN+FP+FN) #accuracy

## Now, generate new training and testing sets of the letters data frame using letters$letter
## What is the baseline accuracy on the testing set?
letters$letter = as.factor( letters$letter )
set.seed(2000)
section2 = sample.split(letters$letter, SplitRatio = 0.5)

train2 = subset(letters, section2 == TRUE)
test2 = subset(letters, section2 == FALSE)

table(test2$letter)
401/(395+383+401+379)

## Now build a classification tree to predict "letter", using the training set to build your model.
## What is the test set accuracy of your CART model?
CART1 = rpart(letter ~ . - isB, data=train2, method="class")
CART1.pred = predict(CART1, newdata = test2, type ="class")

a = table(test2$letter, CART1.pred)
(a[1,1]+a[2,2]+a[3,3]+a[4,4])/(sum(a)) #accuracy

## Now build a random forest model on the training data, using the same independent variables as in the previous problem
## What is the test set accuracy of your random forest model?
set.seed(1000)
ForestModel2 = randomForest(letter ~ . - isB, data=train2)
ForestModel2.pred = predict(ForestModel2, newdata = test2)

a = table(test2$letter, ForestModel2.pred)
(a[1,1]+a[2,2]+a[3,3]+a[4,4])/(sum(a)) #accuracy
