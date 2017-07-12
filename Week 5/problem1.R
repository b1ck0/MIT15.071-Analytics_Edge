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
