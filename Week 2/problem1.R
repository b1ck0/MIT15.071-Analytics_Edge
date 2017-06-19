setwd("~/GitHub/AnalyticsEdge_week2_problem1")

## reading the dataframe
rawData = read.csv("climate_change.csv")

## dividing our data into train and test subsets
trainData  = subset(rawData, Year <= 2006)
testData = subset(rawData, Year > 2006)

## building linear regression model to predict Temp
model1 = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = trainData)

## 1.1 the "Multiple R-squared" value = ?
summary(model1)$r.squared

## 1.2 Which variables are significant in the model? We will consider a variable signficant only if the p-value is below 0.05.
summary(model1)$coefficients

## 2.2 Which of the following independent variables is N2O highly correlated with (absolute correlation greater than 0.7)?
##     Which of the following independent variables is CFC.11 highly correlated with?
cor(trainData$N2O, trainData) > abs(0.7)
cor(trainData$CFC.11, trainData) > abs(0.7)

## 3 building a reduced model
model2 = lm(Temp ~ MEI + N2O + TSI + Aerosols, data = trainData)
summary(model2)$coefficients
summary(model2)$r.squared

## 4 Building a reduced model using the step funciton
model3 = step(model1)

## 5 Testing the model on unseen data
prediction3 = predict(model3, newdata = testData)

SSE = sum((prediction3 - testData$Temp)^2)
SST = sum((testData$Temp - mean(trainData$Temp))^2)

R2 = 1 - SSE/SST
