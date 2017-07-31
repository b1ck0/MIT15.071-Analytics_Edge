tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)

library(tm)
library(SnowballC)
Sys.setlocale("LC_ALL", "C")

corpus = VCorpus(VectorSource(tweets$Tweet)) 
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c(stopwords("english")))

frequencies = DocumentTermMatrix(corpus)

allTweets = as.data.frame(as.matrix(frequencies))

library(wordcloud)

cloud = wordcloud(colnames(allTweets), colSums(allTweets))

##### removing the "apple"

corpus = tm_map(corpus, removeWords, c("apple",stopwords("english")))
frequencies1 = DocumentTermMatrix(corpus)
allTweets1 = as.data.frame(as.matrix(frequencies1))
cloud1 = wordcloud(colnames(allTweets1), colSums(allTweets1))

cloud1 = wordcloud(colnames(allTweets1), colSums(allTweets1), colors=brewer.pal(9, "Blues")[c(-5, -6, -7, -8, -9)])
cloud1 = wordcloud(colnames(allTweets1), colSums(allTweets1), colors=brewer.pal(9, "Blues")[c(5, 6, 7, 8, 9)])