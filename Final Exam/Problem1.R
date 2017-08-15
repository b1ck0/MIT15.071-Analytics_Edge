# FORECASTING NATIONAL PARKS VISITS
# The U.S. National Parks System includes 417 areas including national parks, monuments, battlefields, military parks, historical parks,
# historical sites, lakeshores, seashores, recreation areas, scenic rivers and trails, and the White House (see map in Figure 1). 
# Every year, hundreds of millions of recreational visitors come to the parks. What do we know about the parks that can affect the 
# visitor counts? Can we forecast the monthly visits to a given park accurately? To derive insights and answer these questions, we take 
# a look at the historical visits data and the parks information released by the National Parks Service (NPS). 

setwd("~/GitHub/MIT15.071-Analytics_Edge/Final Exam")
visits = read.csv("park_visits.csv")

str(visits)

## Subsetting visits from July 2016
visits2016jul = subset(visits, Year == 2016 & Month == 7)

## Which park type has the most number of parks in visits2016jul?
table(visits2016jul$ParkType)

## Which specific park has the most number of visitors in visits2016jul?
visits2016jul[order(-visits2016jul$logVisits),1][1]

## Which region has the highest average log visits in July 2016?
## What is the average log visits for the region in July 2016 with:
## 1. the highest average log visits?
## 2. the lowest average log visits?
tapply(visits2016jul$logVisits, visits2016jul$Region, mean)
max(tapply(visits2016jul$logVisits, visits2016jul$Region, mean))
min(tapply(visits2016jul$logVisits, visits2016jul$Region, mean))

## What is the correlation between entrance fee (the variable cost) and the log visits in July 2016?
cor(visits2016jul$cost, visits2016jul$logVisits)

## What observations do you make for Yellowstone NP visits over the years?
ys = subset(visits, ParkName == "Yellowstone NP")
ys_ts=ts(ys$logVisits,start=c(2010,1),freq=12)
plot(ys_ts)
### Between the years, the shapes are largely similar.
### The log visits are highly cyclical, with the peaks in the summer time.

## Remove the observations with the missing values
## How many observations are there in visits now?
visits = visits[rowSums(is.na(visits)) == 0, ]
nrow(visits)

## We are interested in predicting the log visits. Before doing the split, let's also make Month a factor variable.
## Subset our dataset into a training and a testing set by splitting based on the year: training would contain 2010-2014 years of data, 
## and testing would be 2015-2016 data.
visits$Month = as.factor(visits$Month)

train = subset(visits, Year <= 2014)
test = subset(visits, Year > 2014)

## Let's build now a simple linear regression model "mod" using the training set to predict the log visits. As a first step, we only use 
## the laglogVisits variable 

mod = lm(logVisits ~ laglogVisits, data = train)

## What's the coefficient of the laglogVisits variable?
summary(mod)

## What's the out-of-sample R2 in the testing set for this simple model?
prediction = predict(mod, newdata = test)
SSE = sum((prediction - test$logVisits)^2)
SST = sum((test$logVisits - mean(train$logVisits))^2)
R2 = 1 - SSE/SST

## We see that the model achieves good predictive power already simply using the previous month's visits. To see if the other knowledge 
## we have about the parks can improve the model, let's add these variables in a new model.
## The new model would have the following variables:
## laglogVisits, laglogVisitsYear, Year, Month, Region, ParkType, and cost

mod2 = lm(logVisits ~ laglogVisits + laglogVisitsYear + Year + Month + Region + ParkType + cost, data=train)

## Looking at the model summary, which of the following statements are correct (significance at 0.05 level)?
## [CORRECT]Both the log visits from last month and last year are significant and are positively associated with the current log visits.
## None of the regions are significant from the baseline region (Alaska).
## [CORRECT]None of the park types are significant from the baseline park type (National Battlefield).
## The cost is no longer significant.
summary(mod2)

## In the new model, what's the out-of-sample R2 in the testing set?
prediction2 = predict(mod2, newdata = test)
SSE = sum((prediction2 - test$logVisits)^2)
SST = sum((test$logVisits - mean(train$logVisits))^2)
R2 = 1 - SSE/SST

## In addition to the logistic regression model, we can also train a regression tree. Use the same set of variables as the previous 
## problem (laglogVisits, laglogVisitsYear, Year, Month, Region, ParkType, and cost), train a regression tree with cp = 0.05.
library(rpart)
library(rpart.plot)
tree1 = rpart(logVisits ~ laglogVisits + 
                          laglogVisitsYear + 
                          Year + 
                          Month + 
                          Region + 
                          ParkType + 
                          cost, data=train, cp = 0.05)
## Looking at the plot of the tree, how many different predicted values are there?
prp(tree1)

## What is the out-of-sample R2 on the testing set?
prediction3 = predict(tree1, newdata = test)
SSE = sum((prediction3 - test$logVisits)^2)
SST = sum((test$logVisits - mean(train$logVisits))^2)
R2 = 1 - SSE/SST

## The out-of-sample R2 does not appear to be very good under regression trees, compared to a linear regression model. 
## We could potentially improve it via cross validation.
## Set seed to 201, run a 10-fold cross-validated cart model, with cp ranging from 0.0001 to 0.005 in increments of 0.0001. 
## What is optimal cp value on this grid?
library(caret)
library(e1071)
set.seed(201)
numFolds = trainControl(method = "cv", number = 10)
cartGrid = expand.grid(.cp = seq(from = 0.0001,to = 0.005,by = 0.0001))
train(logVisits ~ laglogVisits + 
                  laglogVisitsYear + 
                  Year + 
                  Month + 
                  Region + 
                  ParkType + 
                  cost, data = train, method = "rpart", trControl = numFolds, tuneGrid = cartGrid)

## Rerun the regression tree on the training data, now using the cp value equal to the one selected in the previous problem 
## (under the original range). Note: do not get the tree from the cross-validation directly.
tree2 = rpart(logVisits ~ laglogVisits + 
                          laglogVisitsYear + 
                          Year + 
                          Month + 
                          Region + 
                          ParkType + 
                          cost, data=train, cp = 0.0001)

## What is the out-of-sample R2 in the testing set?
prediction4 = predict(tree2, newdata = test)
SSE = sum((prediction4 - test$logVisits)^2)
SST = sum((test$logVisits - mean(train$logVisits))^2)
R2 = 1 - SSE/SST

## We can potentially further improve the models by using a random forest. Set seed to 201 again. Train a random forest model with 
## the same set of covariates, and using just default parameters (no need to specify)
library(randomForest)
set.seed(201)
forest1 = randomForest(logVisits ~ laglogVisits + 
                                   laglogVisitsYear + 
                                   Year + 
                                   Month + 
                                   Region + 
                                   ParkType + 
                                   cost, data=train)

## What is the R2 on the testing set for the random forest model?
prediction5 = predict(forest1, newdata = test)
SSE = sum((prediction5 - test$logVisits)^2)
SST = sum((test$logVisits - mean(train$logVisits))^2)
R2 = 1 - SSE/SST