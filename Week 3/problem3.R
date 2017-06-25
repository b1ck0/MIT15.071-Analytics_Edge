## Predicting Loan Repayment

# 1.1 Preparing the Dataset
loans = read.csv("loans.csv")
mean(loans$not.fully.paid)

# 1.2 Which of the following variables has at least one missing observation?
summary(loans)

# 1.3 Which of the following is the best reason to fill in the missing values for these variables instead of removing observations with missing data?
NAs = subset(loans, 
             is.na(log.annual.inc) == TRUE | 
             is.na(days.with.cr.line) == TRUE | 
             is.na(revol.util) == TRUE | 
             is.na(inq.last.6mths) == TRUE | 
             is.na(delinq.2yrs) == TRUE | 
             is.na(pub.rec) == TRUE)

# 1.4 Dealing with NAs
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed
# my imputation is different from theirs regardless of the random seed, this I will read the imputed database
loans = read.csv("loans_imputed.csv")

# 2.1 Prediction Models
#     Which independent variables are significant in our model?
set.seed(144)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)

train = subset(loans, split == TRUE)
test = subset(loans, split == FALSE)

Log1 = glm(not.fully.paid ~ ., data=train, family = binomial)
summary(Log1)

# 2.2 Consider two loan applications, which are identical other than the fact that the borrower in Application A has FICO credit score 700 while the borrower i
# n Application B has FICO credit score 710. Let Logit(A) be the log odds of loan A not being paid back in full, according to our logistic regression model, 
# and define Logit(B) similarly for loan B. What is the value of Logit(A) - Logit(B)?

# Now, let O(A) be the odds of loan A not being paid back in full, according to our logistic regression model, and define O(B) similarly for loan B. What is 
# the value of O(A)/O(B)? (HINT: Use the mathematical rule that exp(A + B + C) = exp(A)*exp(B)*exp(C). Also, remember that exp() is the exponential function in R.)

dLogit = -9.317e-03*(700-710)
OddsFraction = exp(-9.317e-03*(700))/exp(-9.317e-03*(710))

# 2.3 Predict the probability of the test set loans not being paid back in full (remember type="response" for the predict function). Store these predicted 
# probabilities in a variable named predicted.risk and add it to your test set (we will use this variable in later parts of the problem). Compute the confusion 
# matrix using a threshold of 0.5.
#     What is the accuracy of the logistic regression model?
#     What is the accuracy of the baseline model?

predicted.risk = predict(Log1, newdata = test, type = "response")
test$predicted.risk = predicted.risk

table(test$not.fully.paid, predicted.risk >= 0.5)
TP = 3
TN = 2400
FP = 13
FN = 457

accuracy = (TN + TP)/(TN + TP + FP + FN)

table(test$not.fully.paid)
accuracy_baseline = 2413/(2413+460)

# 2.4 Use the ROCR package to compute the test set AUC.
library(ROCR)

ROCRpred = prediction(predicted.risk, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

# 3.1 Using the training set, build a bivariate logistic regression model (aka a logistic regression model with a single independent variable) 
# that predicts the dependent variable not.fully.paid using only the variable int.rate.

Log2 = glm(not.fully.paid ~ int.rate, data=train, family=binomial)

# 3.2 Make test set predictions for the bivariate model. What is the highest predicted probability of a loan not being paid in full on the testing set?
#     With a logistic regression cutoff of 0.5, how many loans would be predicted as not being paid in full on the testing set?
predict2 = predict(Log2, newdata = test, type = "response")
summary(predict2)
table(test$not.fully.paid, predict2 >= 0.5)

# 3.3 What is the test set AUC of the bivariate model?
ROCRpred = prediction(predict2, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

# 4.1 How much does a $10 investment with an annual interest rate of 6% pay back after 3 years, using continuous compounding of interest?
10*exp(0.06*3)

# 5.1 What is the maximum profit of a $10 investment in any loan in the testing set (do not include the $ sign in your answer)?
test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1
max(test$profit)*10

# 6.1 What is the average profit of a $1 investment in one of these high-interest loans 
#     What proportion of the high-interest loans were not paid back in full?
highInterest = subset(test, int.rate >= 0.15)
mean(highInterest$profit)
mean(highInterest$not.fully.paid)

# 6.2 What is the profit of the investor, who invested $1 in each of these 100 loans
#     How many of 100 selected loans were not paid back in full?
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
selectedLoans = subset(highInterest, predicted.risk <= cutoff)

sum(selectedLoans$profit)
nrow(subset(selectedLoans, not.fully.paid == 1))
