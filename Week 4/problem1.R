full = read.csv("gerber.csv")

## What proportion of people in this dataset voted in this election?
mean(full$voting)
    
## Which of the four "treatment groups" had the largest percentage of people who actually voted (voting = 1)?
tapply(full$voting, full$civicduty, mean)
tapply(full$voting, full$hawthorne, mean)
tapply(full$voting, full$self, mean)
tapply(full$voting, full$neighbors, mean)

## Build a logistic regression model for voting using the four treatment group variables as the independent 
## variables (civicduty, hawthorne, self, and neighbors). Use all the data to build the model (DO NOT split the data into a training 
## set and testing set). Which of the following coefficients are significant in the logistic regression model?

LogModel = glm(voting ~ civicduty + 
                        hawthorne +
                        self + 
                        neighbors, data = full, family = binomial)

summary(LogModel)

## Using a threshold of 0.3, what is the accuracy of the logistic regression model?
LogPrediction = predict(LogModel, type = "response")
a = table(full$voting, LogPrediction >= 0.3)

TP = a[2,2]
TN = a[1,1]
FP = a[1,2]
FN = a[2,1]

LogAcc = (TP+TN)/(TP+TN+FP+FN)

## Using a threshold of 0.5, what is the accuracy of the logistic regression model?
table(full$voting, LogPrediction >= 0.5)
235388/(108696+235388)

## Compare your previous two answers to the percentage of people who did not vote (the baseline accuracy) and compute the AUC of the model. 
## What is happening here?
table(full$voting)
baseline_accuracy = 235388/(108696+235388)

library(ROCR)
ROCRPred = prediction(LogPrediction, full$voting)
as.numeric(performance(ROCRPred, "auc")@y.values)

LogPerf = performance(ROCRPred, measure = "tpr", x.measure = "fpr")
plot(LogPerf)

## Build a CART tree for voting using all data and the same four treatment variables we used before.
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=full)
prp(CARTmodel)

## Force the complete tree to be built
CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=full, cp=0.0)
prp(CARTmodel2)

## Make a new tree that includes the "sex" variable, again with cp = 0.0.
CARTmodel3 = rpart(voting ~ sex + civicduty + hawthorne + self + neighbors, data=full, cp=0.0)
prp(CARTmodel3)
mean(full$voting[full$control==1 & full$sex==1]) ## fraction of women in control group who voted
mean(full$voting[full$control==1 & full$sex==0]) ## fraction of men in control group who voted

## Create a regression tree using just the "control" variable, then create another tree with the "control" and "sex" variables, both with cp=0.0.
RegTree1 = rpart(voting ~ control, data = full, cp=0.0)
RegTree2 = rpart(voting ~ control + sex, data = full, cp=0.0)
prp(RegTree1, digits = 6)
prp(RegTree2, digits = 6)

## Going back to logistic regression now, create a model using "sex" and "control". Interpret the coefficient for "sex":
LogReg2 = glm(voting ~ control + sex, data = full, family = binomial)
summary(LogReg2)

## What is the absolute difference between the tree and the logistic regression for the (Woman, Control) case?
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(LogReg2, newdata=Possibilities, type="response")
abs(0.2908065 - 0.290456)

## How do you interpret the coefficient for the new variable in isolation? 
LogModel3 = glm(voting ~ sex + control + sex:control, data=full, family="binomial")
summary(LogModel3)

## Now what is the difference between the logistic regression model and the CART model for the (Woman, Control) case? 
predict(LogModel3, newdata=Possibilities, type="response")
abs(0.2904558 - 0.290456)

