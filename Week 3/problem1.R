## Popularity of Music Records

# 1.1 Understanding the Data
# How many observations (songs) are from the year 2010?
songs = read.csv("songs.csv")
nrow(subset(songs, year == 2010))

# 1.2 How many songs does the dataset include for which the artist name is "Michael Jackson"?
nrow(subset(songs, artistname == "Michael Jackson"))

# 1.3 Which of these songs by Michael Jackson made it to the Top 10?
subset(songs, artistname == "Michael Jackson" & Top10 == TRUE)$songtitle

# 1.4 The variable corresponding to the estimated time signature (timesignature) is discrete, meaning that it only takes integer values (0, 1, 2, 3, . . . ). 
# What are the values of this variable that occur in our dataset?
table(songs$timesignature)

# 1.5 Out of all of the songs in our dataset, the song with the highest tempo is one of the following songs. Which one is it?
head(songs[order(-songs$tempo),])$songtitle

# 2.1 Creating Our Prediction Model
# We wish to predict whether or not a song will make it to the Top 10. To do this, first use the subset function to split the data into a training set "SongsTrain" 
# consisting of all the observations up to and including 2009 song releases, and a testing set "SongsTest", consisting of the 2010 song releases.
# How many observations (songs) are in the training set?

train = subset(songs, year <= 2009)
test = subset(songs, year == 2010)

nrow(train)

# 2.2 Looking at the summary of your model, what is the value of the Akaike Information Criterion (AIC)?

nonvars = c("year", "songtitle", "artistname", "songID", "artistID") # variables we do NOT want to use in our regression model
train = train[!(names(train) %in% nonvars)]
test = test[!(names(test) %in% nonvars)]

SongsLog1 = glm(Top10 ~ ., data=train, family=binomial)
summary(SongsLog1)

# 3.1 Beware of Multicollinearity Issues!
# What is the correlation between the variables "loudness" and "energy" in the training set?

cor(train$loudness, train$energy)

# 3.2 Create Model 2, which is Model 1 without the independent variable "loudness".
SongsLog2 = glm(Top10 ~ . - loudness, data=train, family=binomial)
summary(SongsLog2)

# 3.3 Now, create Model 3, which should be exactly like Model 1, but without the variable "energy".
SongsLog3 = glm(Top10 ~ . - energy, data=train, family=binomial)
summary(SongsLog3)

# 4.1 Validating Our Model
# Make predictions on the test set using Model 3. What is the accuracy of Model 3 on the test set, using a threshold of 0.45? 
# (Compute the accuracy as a number between 0 and 1.)

predict1 = predict(SongsLog3, newdata = test, type="response")
table(test$Top10, predict1 >= 0.45)

TP = 19
TN = 309
FP = 5
FN = 40

accuracy = (TN + TP)/(TN + TP + FP + FN)

# 4.2 Let's check if there's any incremental benefit in using Model 3 instead of a baseline model. Given the difficulty of guessing 
# which song is going to be a hit, an easier model would be to pick the most frequent outcome (a song is not a Top 10 hit) for all songs. 
# What would the accuracy of the baseline model be on the test set? (Give your answer as a number between 0 and 1.)

table(test$Top10)
accuracy1 = 314/(314+59)

# 4.3 How many songs does Model 3 correctly predict as Top 10 hits in 2010 (remember that all songs in 2010 went into our test set), using a threshold of 0.45?
TP
FP

# 4.4 What is the sensitivity of Model 3 on the test set, using a threshold of 0.45?
#     What is the specificity of Model 3 on the test set, using a threshold of 0.45?
sensitivity = TP/(TP+FN)
specificity = TN/(TN+FP)