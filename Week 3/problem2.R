## Predicting Parole Violators

# 1.1 Loading the Dataset
#     How many parolees are contained in the dataset?
parole = read.csv("parole.csv")
nrow(parole)

# 1.2 How many of the parolees in the dataset violated the terms of their parole?
nrow(subset(parole, violator == 1))

# 1.3 Which variables in this dataset are unordered factors with at least three levels?
str(parole)
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)
parole$violator = as.factor(parole$violator)

# 2.2 How does the output of summary() change for a factor variable as compared to a numerical variable?
summary(parole)

# 3.1 Roughly what proportion of parolees have been allocated to the training and testing sets?
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)
nrow(train)/(nrow(train)+nrow(test))

# 4.1 Building a Logistic Regression Model
#     What variables are significant in this model? 
model1 = glm(violator ~ ., data=train, family=binomial)
summary(model1)

# 4.2 What can we say based on the coefficient of the multiple.offenses variable?
#     The following two properties might be useful to you when answering this question:
#     1) If we have a coefficient c for a variable, then that means the log odds (or Logit) are increased by c for a unit increase in the variable.
#     2) If we have a coefficient c for a variable, then that means the odds are multiplied by e^c for a unit increase in the variable.

# 4.3 Consider a parolee who is male, of white race, aged 50 years at prison release, from the state of Maryland, served 3 months, had a maximum sentence 
# of 12 months, did not commit multiple offenses, and committed a larceny. Answer the following questions based on the model's predictions for this individual.
#     According to the model, what are the odds this individual is a violator?
#     According to the model, what is the probability this individual is a violator?

male = 1
race = 1
age = 50
state = 1
time.served = 3
max.sentence = 12
multiple.offenses = 0
crime = 2
violator = 0

state = as.factor(state)
crime = as.factor(crime)

logit = 0.3869904*male + 0.8867192*race -0.0001756*age - 0.1238867*time.served + 0.0802954*max.sentence + 1.6119919*multiple.offenses + 0.6837143 - 4.2411574 
odds = exp(logit)
prob = 1/(1+exp(-logit))

# 5.1 Evaluating the Model on the Testing Set
#     What is the maximum predicted probability of a violation?
predict1 = predict(model1, newdata = test, type="response")
summary(predict1)

# 5.2 In the following questions, evaluate the model's predictions on the test set using a threshold of 0.5.
#     What is the model's sensitivity?
#     What is the model's specificity?
#     What is the model's accuracy?
table(test$violator, predict1 >= 0.5)

TP = 12 #true positives
TN = 167 #true negatives
FP = 12 #false positives
FN = 11 #false negatives

sensitivity = TP/(TP+FN)
specificity = TN/(TN+FN)
accuracy = (TN + TP)/(TN + TP + FP + FN)

# 5.3 Evaluating the Model on the Testing Set
#     What is the accuracy of a simple model that predicts that every parolee is a non-violator?
table(test$violator)
accuracy_baseline = 179/(179+23)

# 5.6 Using the ROCR package, what is the AUC value for the model?
#     The probability the model can correctly differentiate between a randomly selected parole violator and a randomly selected parole non-violator.
library(ROCR)

ROCRpred = prediction(predict1, test$violator)
as.numeric(performance(ROCRpred, "auc")@y.values)
