setwd("~/GitHub/AnalyticsEdge_week2_problem2")

pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")

## 1.1 How many students are there in the training set?
nrow(pisaTrain)

## 1.2 Using tapply() on pisaTrain, what is the average reading test score of males?
##     Of females?
tapply(pisaTrain$readingScore, pisaTrain$male, mean)

## 1.3 Which variables are missing data in at least one observation in the training set?
summary(pisaTrain)

## 1.4 How many observations are now in the training set?
##     How many observations are now in the testing set?
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

## 2.1 Which of the following variables is an unordered factor with at least 3 levels? 
str(pisaTrain)

## 2.2 Which binary variables will be included in the regression model?

## 2.3 For a student who is Asian, which binary variables would be set to 0?
##     For a student who is white, which binary variables would be set to 0? All remaining variables will be set to 1

## 3.1 What is the Multiple R-squared value of lmScore on the training set?
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

lmScore = lm(readingScore ~. , data = pisaTrain)
summary(lmScore)$r.squared

## 3.2 What is the training-set root-mean squared error (RMSE) of lmScore?
SSE = sum(lmScore$residuals^2)
RMSE = sqrt(SSE / nrow(pisaTrain))
# alternative: summary(lmScore)$sigma

## 3.3 Consider two students A and B. They have all variable values the same, except that student A is in grade 11 and student
## B is in grade 9. What is the predicted reading score of student A minus the predicted reading score of student B?
summary(lmScore)$coefficients
29.542707092*(11-9)

## 3.4 What is the meaning of the coefficient associated with variable raceethAsian?

## 3.5 Based on the significance codes, which variables are candidates for removal from the model? Select all that apply.

## 4.1 Using the "predict" function and supplying the "newdata" argument, use the lmScore model to predict the reading scores of 
## students in pisaTest. Call this vector of predictions "predTest". Do not change the variables in the model 
## for example, do not remove variables that we found were not significant in the previous part of this problem). 
## Use the summary function to describe the test set predictions.
## What is the range between the maximum and minimum predicted reading score on the test set?

pisaPredict = predict(lmScore, newdata = pisaTest)
max(pisaPredict) - min(pisaPredict)

## 4.2 What is the sum of squared errors (SSE) of lmScore on the testing set?
##     What is the root-mean squared error (RMSE) of lmScore on the testing set?

SSE = sum((pisaPredict - pisaTest$readingScore)^2)
RMSE = sqrt(SSE / nrow(pisaTest))

## 4.3 What is the predicted test score used in the baseline model?
##     What is the sum of squared errors of the baseline model on the testing set?

baseline = mean(pisaTrain$readingScore)
SST = sum((pisaTest$readingScore - baseline)^2)

## 4.4 What is the test-set R-squared value of lmScore?
1 - SSE/SST
