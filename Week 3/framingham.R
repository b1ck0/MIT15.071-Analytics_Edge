framingham = read.csv("framingham.csv")
str(framingham)

library(caTools)
set.seed(1000)

split = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)

train = subset(framingham, split == TRUE)
test = subset(framingham, split == FALSE)

framinghamLog = glm(TenYearCHD ~ ., data = train, family = binomial)

summary(framinghamLog)

predictTest = predict(framinghamLog, type="response", newdata = test)
table(test$TenYearCHD, predictTest > 0.5)

# TRUE POSITIVES
# TRUE NEGATIVES
# FALSE POSITIVES
# FALSE NEGATIVES
TP = 11
TN = 1069
FP = 6
FN = 187

sensitivity = TP/(TP+FN)
specificity = TN/(TN+FP)

accuracy = (TN + TP)/(TN + TP + FP + FN)
baseline_accuracy = (TN + FP)/(TN + TP + FP + FN)

library(ROCR)

ROCRpred = prediction(predictTest, test$TenYearCHD)
as.numeric(performance(ROCRpred, "auc")@y.values)
