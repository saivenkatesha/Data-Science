# Step 1 - collecting data

setwd("E://All About Big Data/Data Science/ML with R/R_Practice/Naive Bayes")
sms_raw <- read.csv("sms_spam.csv", stringsAsFactors = FALSE)

# Step 2 - exploring and preparing the data
str(sms_raw)

sms_raw$type <- factor(sms_raw$type)
str(sms_raw$type)
table(sms_raw$type)

# Data preparation - processing text data for analysis

library(tm)

# The first step in processing text data involves creating a corpus, which refers to a collection of text documents.


# The Corpus() function is extremely flexible and can read documents from many different sources such as PDFs and
# Microsoft Word documents. To learn more, examine the Data Import section in the tm package vignette using the
# command: print(vignette("tm"))

sms_corpus <- Corpus(VectorSource(sms_raw$text))
sms_corpus

print(sms_corpus)

# To look at the contents of the corpus, we can use the inspect() function.
inspect(sms_corpus[1:3])

# First, we will convert all of the SMS messages to lowercase and remove any numbers:

corpus_clean <- tm_map(sms_corpus, tolower)

corpus_clean <- tm_map(corpus_clean, PlainTextDocument)

# Removing numbers  from the object
corpus_clean <- tm_map(corpus_clean, removeNumbers)

stopwords()  # list of stop words in the tm library
# Removing stop words from the object
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())

# Removing punctuations from the object

corpus_clean <- tm_map(corpus_clean, removePunctuation)

# The last step then is to remove additional whitespace, leaving only a single space between words.

corpus_clean <- tm_map(corpus_clean, stripWhitespace)


sms_dtm <- DocumentTermMatrix(corpus_clean,PlainTextDocument())
                              
# Data preparation - creating training and test datasets

# We'll begin by splitting the raw data frame:
sms_raw_train <- sms_raw[1:4169, ]
sms_raw_test <- sms_raw[4170:5559, ]         

# Splitting data from  the document-term matrix:

sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test <- sms_dtm[4170:5559, ]

# And finally, the corpus:

sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test <- corpus_clean[4170:5559]

# compare the proportion of spam in the training and test data frames

prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))

# Visualizing text data - word clouds

library(wordcloud)

wordcloud(sms_corpus_train, min.freq = 40, random.order = FALSE)

# Let's use R's subset() function to take a subset of the sms_raw_train data by SMS
# type. First, we'll create a subset where type is equal to spam:

spam <- subset(sms_raw_train, type == "spam")

# Next, we'll do the same thing for the ham subset:

ham <- subset(sms_raw_train, type == "ham")


wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))           

# Data preparation - creating indicator features for frequent words
data(crude)


myTerms <- findFreqTerms(sms_dtm_train, 5)



# To save this list of frequent terms for use later, we'll use the Dictionary() function:
sms_dict <- findFreqTerms(sms_dtm_train, 5)
sms_dict <- inspect(DocumentTermMatrix(crude, list(dictionary = myTerms)))
sms_dict
sms_train <- DocumentTermMatrix(sms_corpus_train,list(dictionary = sms_dict))
sms_train
sms_test <- DocumentTermMatrix(sms_corpus_test,list(dictionary = sms_dict))
sms_test

convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
  return(x)
}

sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test <- apply(sms_test, MARGIN = 2, convert_counts)


# Step 3 - training a model on the data

library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)

# Step 4 - evaluating model performance
library(gmodels)

# The predict() function is used to make the predictions. We will store these in a vector named sms_test_pred:
sms_test_pred <- predict(sms_classifier, sms_test)

CrossTable(sms_test_pred, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE,dnn = c('predicted', 'actual'))
