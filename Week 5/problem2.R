library(tm)
library(SnowballC)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ROCR)

## Load clinical_trial.csv into a data frame called trials
## How many characters are there in the longest abstract?
trials = read.csv("clinical_trial.csv", stringsAsFactors = FALSE)
max(nchar(trials$abstract))

## How many search results provided no abstract?
sum(nchar(trials$abstract) == 0)

## Find the observation with the minimum number of characters in the title
## What is the text of the title of this article?
trials$title[which.min(nchar(trials$title))]

## Preparing the Corpus
Sys.setlocale("LC_ALL", "C")
corpusTitle = VCorpus(VectorSource(trials$title))
corpusTitle = tm_map(corpusTitle, content_transformer(tolower))
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusTitle = tm_map(corpusTitle, stemDocument)
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmTitle = as.data.frame(as.matrix(dtmTitle))

corpusAbstract = VCorpus(VectorSource(trials$abstract))
corpusAbstract = tm_map(corpusAbstract, content_transformer(tolower))
corpusAbstract = tm_map(corpusAbstract, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, stemDocument)
dtmAbstract= DocumentTermMatrix(corpusAbstract)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))

## What is the most frequent word stem across all the abstracts?
sort(colSums(dtmAbstract))

## 
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

## How many columns are in this combined data frame?
dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial = trials$trial
ncol(dtm)

## What is the accuracy of the baseline model on the training set?
set.seed(144)
section = sample.split(dtm$trial, SplitRatio = .7)
train = subset(dtm, section == TRUE)
test = subset(dtm, section == FALSE)

table(train$trial)    
726/(726+574)

## What is the name of the first variable the model split on?
trialCART = rpart(trial ~., data = train, method="class")
rpart.plot(trialCART)

## What is the maximum predicted probability for any result?
trialCART.pred = predict(trialCART)[,2]
summary(trialCART.pred)

## What is the training set accuracy of the CART model?
a = table(train$trial, trialCART.pred >= 0.5)

TP = a[2,2] #true positives
TN = a[1,1] #true negatives
FP = a[1,2] #false positives
FN = a[2,1] #false negatives

sensitivity = TP/(TP+FN)
specificity = TN/(TN+FP)
accuracy = (TN + TP)/(TN + TP + FP + FN)

##What is the testing set accuracy, assuming a probability threshold of 0.5 for predicting that a result is a clinical trial?
predTest = predict(trialCART, newdata = test)[,2]
a = table(test$trial, predTest >= 0.5)

TP = a[2,2] #true positives
TN = a[1,1] #true negatives
FP = a[1,2] #false positives
FN = a[2,1] #false negatives

sensitivity = TP/(TP+FN)
specificity = TN/(TN+FP)
accuracy = (TN + TP)/(TN + TP + FP + FN)

## Using the ROCR package, what is the testing set AUC of the prediction model?
ROCRpred = prediction(predTest, test$trial)
ROCRpred.perf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRpred.perf)
as.numeric(performance(ROCRpred, "auc")@y.values)