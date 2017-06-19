setwd("~/GitHub/AnalyticsEdge_week2_problem3")

fluTrain = read.csv("FluTrain.csv")

## 1.1 Looking at the time period 2004-2011, which week corresponds to the highest percentage of ILI-related physician visits? 
##     Which week corresponds to the highest percentage of ILI-related query fraction?

fluTrain$Week[which.max(fluTrain$ILI)]
fluTrain$Week[which.max(fluTrain$Queries)]

## 1.2 Let us now understand the data at an aggregate level. Plot the histogram of the dependent variable, ILI. What best describes 
## the distribution of values of ILI?
hist(fluTrain$ILI)

## 1.3 Plot the natural logarithm of ILI versus Queries.
plot(fluTrain$Queries, log(fluTrain$ILI))

## 2.2 What is the training set R-squared value for FluTrend1
fluTrend1 = lm(log(ILI) ~ Queries, data = fluTrain)
summary(fluTrend1)$r.squared

## 2.3 For a single variable linear regression model, there is a direct relationship between the R-squared and the correlation between
## the independent and the dependent variables. What is the relationship we infer from our problem?
correlation = with(fluTrain,cor(Queries,log(ILI)))
exp(-0.5*correlation)
log(1/correlation)
correlation^2

## 3.1 What is our estimate for the percentage of ILI-related physician visits for the week of March 11, 2012?
fluTest = read.csv("FluTest.csv")
predTest = exp(predict(fluTrend1, newdata = fluTest))

predTest[which(fluTest$Week == "2012-03-11 - 2012-03-17")]

## 3.2 What is the relative error betweeen the estimate (our prediction) and the observed value for the week of March 11, 2012?
(fluTest$ILI[11] - predTest[11])/fluTest$ILI[11]

## 3.3 What is the Root Mean Square Error (RMSE) between our estimates and the actual observations for the percentage of ILI-related 
## physician visits, on the test set?

SSE = sum((fluTest$ILI - predTest)^2)
RMSE = sqrt(SSE/length(predTest))

## 4.1
ILILag2 = lag(zoo(fluTrain$ILI), -2, na.pad=TRUE)
fluTrain$ILILag2 = coredata(ILILag2)
summary(fluTrain$ILILag2)

## 4.2 Use the plot() function to plot the log of ILILag2 against the log of ILI.
plot(log(fluTrain$ILI),log(fluTrain$ILILag2))

## 4.3 Train a linear regression model on the FluTrain dataset to predict the log of the ILI variable using the Queries variable as 
## well as the log of the ILILag2 variable.

fluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2), data = fluTrain)
summary(fluTrend2)$coefficients
summary(fluTrend2)$r.squared