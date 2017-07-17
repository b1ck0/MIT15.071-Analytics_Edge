library(tm)
library(SnowballC)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ROCR)

## Load the data wiki.csv, calling the data frame "wiki". Convert the "Vandal" column to a factor
## How many cases of vandalism were detected in the history of this page?
wiki = read.csv("wiki.csv", stringsAsFactors=FALSE)
wiki$Vandal = as.factor(wiki$Vandal)
sum(wiki$Vandal == 1)

## Bags of Words
## How many terms appear in dtmAdded?
Sys.setlocale("LC_ALL", "C")
corpusAdded = VCorpus(VectorSource(wiki$Added))
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded = tm_map(corpusAdded, stemDocument)
dtmAdded = DocumentTermMatrix(corpusAdded)

## Filter out sparse terms by keeping only terms that appear in 0.3% or more of the revisions, and call the new matrix sparseAdded. 
## How many terms appear in sparseAdded?
sparseAdded = removeSparseTerms(dtmAdded, 0.997)

## Convert sparseAdded to a data frame called wordsAdded, and then prepend all the words with the letter A
wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

## Now repeat all of the steps we've done so far (create a corpus, remove stop words, stem the document, create a sparse document 
## term matrix, and convert it to a data frame) to create a Removed bag-of-words dataframe, called wordsRemoved, except this time, 
## prepend all of the words with the letter R. How many words are in the wordsRemoved data frame?
Sys.setlocale("LC_ALL", "C")
corpusRemoved = VCorpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)
sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)

wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

ncol(wordsRemoved)

## What is the accuracy on the test set of a baseline method that always predicts "not vandalism" (the most frequent outcome)?
wikiWords = cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal = wiki$Vandal

set.seed(123)
section = sample.split(wikiWords, SplitRatio = 0.7)
train = subset(wikiWords, section == TRUE)
test = subset(wikiWords, section == FALSE)

table(test$Vandal)
(622)/(622+542)

## Build a CART model to predict Vandal, using all of the other variables as independent variables.
## What is the accuracy of the model on the test set, using a threshold of 0.5?
wikiCART = rpart(Vandal ~., data = train)
wikiCART.pred = predict(wikiCART, newdata = test)

a = table(test$Vandal, wikiCART.pred[,2] > 0.5)
(a[1,1]+a[2,2])/sum(a) #accuracy

## Plot the CART tree. How many word stems does the CART model use?
rpart.plot(wikiCART)

## Based on this new column, how many revisions added a link?
wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
sum(wikiWords2$HTTP)

## What is the new accuracy of the CART model on the test set, using a threshold of 0.5?
wikiTrain2 = subset(wikiWords2, section == TRUE)
wikiTest2 = subset(wikiWords2, section == FALSE)

wikiCART2 = rpart(Vandal ~ ., data = wikiTrain2)
wikiCART2.pred = predict(wikiCART2, newdata = wikiTest2)
rpart.plot(wikiCART2)

a = table(wikiTest2$Vandal, wikiCART2.pred[,2] > 0.5)
(a[1,1]+a[2,2])/sum(a) #accuracy

## What is the average number of words added?
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded)

## 
wikiTrain3 = subset(wikiWords2, section == TRUE)
wikiTest3 = subset(wikiWords2, section == FALSE)

wikiCART3 = rpart(Vandal ~ ., data = wikiTrain3)
wikiCART3.pred = predict(wikiCART3, newdata = wikiTest3)
rpart.plot(wikiCART3)

a = table(wikiTest2$Vandal, wikiCART3.pred[,2] > 0.5)
(a[1,1]+a[2,2])/sum(a) #accuracy

## 
wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

wikiTrain4 = subset(wikiWords3, section == TRUE)
wikiTest4 = subset(wikiWords3, section == FALSE)

wikiCART4 = rpart(Vandal ~ ., data = wikiTrain4)
wikiCART4.pred = predict(wikiCART4, newdata = wikiTest4)
rpart.plot(wikiCART4)

a = table(wikiTest2$Vandal, wikiCART4.pred[,2] > 0.5)
(a[1,1]+a[2,2])/sum(a) #accuracy
