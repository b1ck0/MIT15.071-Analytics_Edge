polling = read.csv("PollingData.csv")
str(polling)
table(polling$Year)

#identifying missing data
summary(polling)

#multiple imputation
library(mice)

simple = polling[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount")]
set.seed(144)

imputed = complete(mice(simple))

polling$Rasmussen = imputed$Rasmussen
polling$SurveyUSA = imputed$SurveyUSA

# start building models

train = subset(polling, Year == 2004 | Year == 2008)
test = subset(polling, Year == 2012)

table(train$Republican)
table(sign(train$Rasmussen))
table(train$Republican, sign(train$Rasmussen))

cor(train[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount", "Republican")])

mod1 = glm(Republican~PropR, data=train, family=binomial)
summary(mod1)

pred1 = predict(mod1, type="response")

table(train$Republican, pred1 >= 0.5)

mod2 = glm(Republican ~ SurveyUSA + DiffCount, data = train, family = binomial)

pred2 = predict(mod2, type="response")

table(train$Republican, pred2 >= 0.5)

summary(mod2)

# test set

table(test$Republican, sign(test$Rasmussen)) # baseline prediction
testPrediction = predict(mod2, newdata=test, type="response")

table(test$Republican, testPrediction >= 0.5)

subset(test, testPrediction >= 0.5 & Republican == 0)
