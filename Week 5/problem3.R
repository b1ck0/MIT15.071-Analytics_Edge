emails = read.csv("emails.csv", stringsAsFactors=FALSE)

## How many emails are in the dataset?
nrow(emails)

## How many of the emails are spam?
sum(emails$spam)

## Which word appears at the beginning of every email in the dataset? 
## Respond as a lower-case word with punctuation removed.
emails$text[1]

## The nchar() function counts the number of characters in a piece of text. 
## How many characters are in the longest email in the dataset (where longest is measured in terms of the maximum number of characters)?
max(nchar(emails$text))

## Which row contains the shortest email in the dataset? 
## (Just like in the previous problem, shortest is measured in terms of the fewest number of characters.)
emails$len = nchar(emails$text)
which(emails$len == min(nchar(emails$text)))

## How many terms are in dtm?
library(tm)
library(SnowballC)
Sys.setlocale("LC_ALL", "C")
corpus = VCorpus(VectorSource(emails$text)) 
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)

dtm = DocumentTermMatrix(corpus)

## To obtain a more reasonable number of terms, limit dtm to contain terms appearing in at least 5% of documents, and store this result as 
## spdtm (don't overwrite dtm, because we will use it in a later step of this homework). How many terms are in spdtm?

spdtm = removeSparseTerms(dtm, 0.95)

## Build a data frame called emailsSparse from spdtm, and use the make.names function to make the variable names of emailsSparse valid.
emailsSparse = as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))

## What is the word stem that shows up most frequently across all the emails in the dataset?
sort(colSums(emailsSparse))

## Add a variable called "spam" to emailsSparse containing the email spam labels
## How many word stems appear at least 5000 times in the ham emails in the dataset?
emailsSparse$spam = emails$spam
sum(sort(colSums(subset(emailsSparse, spam == 0))) > 5000)

## How many word stems appear at least 1000 times in the spam emails in the dataset?
sum(sort(colSums(subset(emailsSparse, spam == 1))) > 1000)

## Building machine learning models
library(caTools)
library(ROCR)
library(rpart)
library(rpart.plot)
library(randomForest)
emailsSparse$spam = as.factor(emailsSparse$spam)

set.seed(123)
section = sample.split(emailsSparse$spam , SplitRatio = 0.7)
train = subset(emailsSparse, section == TRUE)
test = subset(emailsSparse, section == FALSE)

spamLog = glm(spam ~., data = train, family = binomial)
spamCART = rpart(spam ~., data = train, method="class")
set.seed(123)
spamRF = randomForest(spam ~., data = train, type="prob")

spamLogPrediction = predict(spamLog, type = "response")
spamCARTPrediction = predict(spamCART)
spamRFPrediction = predict(spamRF)

table(train$spam, spamLogPrediction > 0.5)
## How many of the training set predicted probabilities from spamLog are less than 0.00001?
sum(spamLogPrediction < 0.00001)

## How many of the training set predicted probabilities from spamLog are more than 0.99999?
sum(spamLogPrediction > 0.99999)

## How many of the training set predicted probabilities from spamLog are between 0.00001 and 0.99999?
sum(spamLogPrediction <= 0.99999 & spamLogPrediction >= 0.00001)

## How many variables are labeled as significant (at the p=0.05 level) in the logistic regression summary output?
options(max.print=1000000)
summary(spamLog)$coefficients
    
## How many of the word stems "enron", "hou", "vinc", and "kaminski" appear in the CART tree?
prp(spamCART)

## What is the training set accuracy of spamLog, using a threshold of 0.5 for predictions?
a = table(train$spam, spamLogPrediction > 0.5)
(a[1,1]+a[2,2])/sum(a) #accuracy

## What is the training set AUC of spamLog?
ROCRpred = prediction(spamLogPrediction, train$spam)
ROCRpred.perf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRpred.perf)
as.numeric(performance(ROCRpred, "auc")@y.values)

## What is the training set accuracy of spamCART, using a threshold of 0.5 for predictions? 
a = table(train$spam, spamCARTPrediction[,2] > 0.5)
(a[1,1]+a[2,2])/sum(a) #accuracy

## What is the training set AUC of spamCART?
ROCRpred = prediction(spamCARTPrediction[,2], train$spam)
ROCRpred.perf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRpred.perf)
as.numeric(performance(ROCRpred, "auc")@y.values)

## What is the training set accuracy of spamRF, using a threshold of 0.5 for predictions? 
a = table(train$spam, spamRFPrediction)
(a[1,1]+a[2,2])/sum(a) #accuracy

## What is the training set AUC of spamRF?
ROCRpred = prediction(spamRFPrediction[,2], train$spam)
ROCRpred.perf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRpred.perf)
as.numeric(performance(ROCRpred, "auc")@y.values)

## Evaluating on the Test Set
spamLogPrediction.2 = predict(spamLog, type = "response", newdata = test)
spamCARTPrediction.2 = predict(spamCART, newdata = test)
spamRFPrediction.2 = predict(spamRF, newdata = test, type="prob")

## What is the testing set accuracy of spamLog, using a threshold of 0.5 for predictions?
a = table(test$spam, spamLogPrediction.2 > 0.5)
(a[1,1]+a[2,2])/sum(a) #accuracy

## What is the testing set AUC of spamLog?
ROCRpred = prediction(spamLogPrediction.2, test$spam)
ROCRpred.perf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRpred.perf)
as.numeric(performance(ROCRpred, "auc")@y.values)

## What is the testing set accuracy of spamCART, using a threshold of 0.5 for predictions?
a = table(test$spam, spamCARTPrediction.2[,2] > 0.5)
(a[1,1]+a[2,2])/sum(a) #accuracy

## What is the testing set AUC of spamCART?
ROCRpred = prediction(spamCARTPrediction.2[,2], test$spam)
ROCRpred.perf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRpred.perf)
as.numeric(performance(ROCRpred, "auc")@y.values)

## What is the testing set accuracy of spamRF, using a threshold of 0.5 for predictions?
a = table(test$spam, spamRFPrediction.2[,2] > 0.5)
(a[1,1]+a[2,2])/sum(a) #accuracy

## What is the testing set AUC of spamRF?
ROCRpred = prediction(spamRFPrediction.2[,2], test$spam)
ROCRpred.perf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRpred.perf)
as.numeric(performance(ROCRpred, "auc")@y.values)