# PREDICTING BANK TELEMARKETING SUCCESS
# The success of marketing campaigns can be highly specific to the product, the target audience, and the campaign methods. In this problem, 
# we examine data from direct marketing campaigns of a Portuguese banking institution between May 2008 and November 2010. The marketing 
# campaigns were based on phone calls. Often, more than one contact to the same client was required, in order to access if the product 
# (bank term deposit) would be or not subscribed.
# In this analysis, the goal would be predicting the dependent variable y, which takes value 1 if the the client subscribed to a term 
# deposit, and 0 otherwise. The data we will be using bank.csv is a subset of the original data, containing 5000 examples and 20 input 
# variables.

setwd("~/GitHub/MIT15.071-Analytics_Edge/Final Exam")

## What is the average age in the data set?
bank = read.csv("bank.csv")
mean(bank$age)

##  Which three jobs have the longest average call durations?
sort(tapply(bank$duration, bank$job, mean))

## Examine the correlation between the following variables: emp.var.rate, cons.price.idx, cons.conf.idx, euribor3m, and nr.employed.
library(dplyr)
bank = tbl_df(bank)
sub1 = select(bank, c(emp.var.rate, cons.price.idx, cons.conf.idx, euribor3m, nr.employed))
cor(sub1)

## Splitting into a Training and Testing Set
library(caTools)
set.seed(201)
spl = sample.split(bank$y, 0.7)
training = subset(bank, spl == TRUE)
testing = subset(bank, spl == FALSE)

## Train a logistic regression model using independent variables age, job, marital, education, default, housing, loan, contact, month, 
## day_of_week, campaign, pdays, previous, poutcome, emp.var.rate, cons.price.idx, and cons.conf.idx, using the training set to obtain 
## the model. 
logReg1 = glm(y ~ age +
                  job + 
                  marital +
                  education +
                  default +
                  housing +
                  loan +
                  contact +
                  month +
                  day_of_week +
                  campaign +
                  pdays +
                  previous +
                  poutcome +
                  emp.var.rate +
                  cons.price.idx + 
                  cons.conf.idx, data = training, family = binomial)
summary(logReg1)

## Using your logistic regression model, obtain predictions on the test set. Then, using a probability threshold of 0.5, 
## create a confusion matrix for the test set.
## We would like to compare the predictions obtained by the logistic regression model and those obtained by a naive baseline model. 
## Remember that the naive baseline model we use in this class always predicts the most frequent outcome in the training set for all 
## observations in the test set.
## What is the number of test set observations where the prediction from the logistic regression model is different than the prediction
## from the baseline model?
prediction1 = predict(logReg1, newdata = testing, type="response")
table(testing$y, prediction1 >= 0.5)
table(testing$y)
50+44

## What is the test-set AUC of the logistic regression model?
library(ROCR)
ROCRpred = prediction(prediction1, testing$y)
ROCRpred.perf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRpred.perf)
as.numeric(performance(ROCRpred, "auc")@y.values)

## Which logistic regression threshold is associated with the upper-right corner of the ROC plot (true positive rate 1 and false positive rate 1)?
prediction1 = predict(logReg1, newdata = testing, type="response")
a = table(testing$y, prediction1 >= 0.01)

TP = a[2,2]
TN = a[1,1]
FP = a[1,2]
FN = a[2,1]

TP/sum(a)
FP/sum(a)

## Plot the colorized ROC curve for the logistic regression model's performance on the test set.
## At roughly which logistic regression cutoff does the model achieve a true positive rate of 60% and a false positive rate of 25%?
ROCRpred = prediction(prediction1, testing$y)
ROCRpred.perf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRpred.perf, colorize=TRUE)

## Which of the following best describes how 10-fold cross-validation works when selecting between 4 different parameter values?
# 40 models are trained on subsets of the training set and evaluated on a portion of the training set correct

## Set the random seed to 201 (even though you have already done so earlier in the problem). Then use the caret package and the 
## train function to perform 10-fold cross validation with the training data set to select the best cp value for a CART model that 
## predicts the dependent variable y using the same set of independent variables as in the logistic regression (Problem 5). 
## Select the cp value from a grid consisting of the 50 values 0.001, 0.002, ..., 0.05.
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
numFolds = trainControl(method = "cv", number = 10)
cartGrid = expand.grid(.cp = seq(from = 0.001,to = 0.05, by = 0.001))
set.seed(201)
train(y ~ age +
          job + 
          marital +
          education +
          default +
          housing +
          loan +
          contact +
          month +
          day_of_week +
          campaign +
          pdays +
          previous +
          poutcome +
          emp.var.rate +
          cons.price.idx + 
          cons.conf.idx, data = training, method = "rpart", trControl = numFolds, tuneGrid = cartGrid)

## Build and plot the CART model trained with the parameter identified in Problem 13, again predicting the dependent variable using 
## the same set of independent variables. What variable is used as the first (upper-most) split in the tree?
tree1 = rpart(y ~ age +
                  job + 
                  marital +
                  education +
                  default +
                  housing +
                  loan +
                  contact +
                  month +
                  day_of_week +
                  campaign +
                  pdays +
                  previous +
                  poutcome +
                  emp.var.rate +
                  cons.price.idx + 
                  cons.conf.idx, data = training, cp = 0.009, method = "class")
prp(tree1)

## Using the CART model you created in Problem 14, obtain predictions on the test set (using the parameter type="class" with the 
## predict function). Then, create a confusion matrix for the test set.
## What is the accuracy of your CART model?
prediction2 = predict(tree1, newdata = testing, type="class")

a = table(testing$y, prediction2)

TP = a[2,2]
TN = a[1,1]
FP = a[1,2]
FN = a[2,1]

accuracy = (TN + TP)/(TN + TP + FP + FN)
