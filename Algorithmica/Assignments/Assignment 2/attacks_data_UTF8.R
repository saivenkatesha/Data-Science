# Assignment Two

library(formattable) # nice output
library(ggplot2)
library(lubridate)
library(rworldmap)
library(dplyr)
library(reshape)

getwd()
setwd("F://Algorithmica/Assignments/Assignment 2")
# Loading the Train Data Set

terrorism_train = read.csv("attacks_data_UTF8.csv",stringsAsFactors = FALSE)
terrorism_train$Description = as.character(terrorism_train$Description)
terrorism_train$City = as.factor(terrorism_train$City)
terrorism_train$Country = as.factor(terrorism_train$Country)
str(terrorism_train)
names(terrorism_train)[1]="S.No"

  
# Date as factor

terrorism_train$Date = as.Date(terrorism_train$Date,"%Y-%m-%d")

#explore multi-variate relationships numerically & graphically
# Explore all the attributes individually using univariate numeric and graphics
summary(terrorism_train)
xtabs(~Country,terrorism_train)
x11()
ggplot(terrorism_train) + geom_bar(aes(x=Country))


# d. Do the following:
#  a. Find top-10 countries with most attacks, most injured and most killed respectively and show them with plots

# 10 countries with most attacks - Countrywise Frequency



# Need the 'dplyr' package to do this efficiently

select(terrorism_train,Country) # Show Country

head(select(terrorism_train, -Description), n =10) # Show everything other than Description

select(terrorism_train, -Description) %>% arrange(Country) %>% filter(Killed > 1) %>% head

# top-10 countries with most attacks countrywise

top_country_attacks = terrorism_train %>%  group_by(Country) %>% summarise(totalAttacks=n()) %>% arrange(desc(totalAttacks)) %>% head(n=10)
top_country_attacks
X11()
ggplot(top_country_attacks) + geom_bar(aes(x=Country, y=totalAttacks), stat="identity") + coord_flip()

top10CountryFreq <- as.data.frame(head(sort(table(terrorism_train$Country, dnn = "Country"),
                                        decreasing = TRUE), n = 10))
top10CountryFreq
dim(top10CountryFreq)
str(top10CountryFreq)
x11()
ggplot(top10CountryFreq) + geom_bar(aes(x=Country, y=Freq), stat = "identity") #+ coord_flip()

# Top 10 countries with most attacks - Countrywise most Injured

x = tapply(terrorism_train$Injured,terrorism_train$Country,sum)
x = sort(x,decreasing = TRUE)
x = data.frame(x)
head(x,n = 10)


top_country_injured = as.data.frame(
                        head(
                          sort(
                          tapply(
                                  terrorism_train$Injured, terrorism_train$Country, sum
                                ),  
                              decreasing = TRUE
                              ), 
                         n = 10
                         )
                        )




dim(top_country_injured)
top_country_injured
names(top_country_injured)[1]="Most Injured"
# Trying something like excel pivot

basic_summary = group_by(terrorism_train,Killed,Country)

basic_summary = summarise(terrorism_train,Killed = sum(terrorism_train$Killed),Injured=sum(Injured))
basic_summary


# Satish Code
top_country_injured = terrorism_train %>%  group_by(Country) %>% summarise(totalInjured=sum(Injured)) %>% arrange(desc(totalInjured)) %>% head(n=10)
top_country_injured
str(top_country_injured)
top_country_injured$totalInjured = as.integer(top_country_injured$totalInjured)
levels(top_country_injured)
top_country_injured = as.data.frame(top_country_injured)

top_country_injured <- transform(top_country_injured, 
                          Country = reorder(Country, -totalInjured))

X11()
ggplot(top_country_injured) + geom_bar(aes(x=Country, y=totalInjured), stat="identity") #+ coord_flip()

top10CountryFreq
top_country_injured
# Top 10 countries with most attacks - Countrywise most Killed
top_Killed = terrorism_train %>%  group_by(Country) %>% summarise(totalkilled=sum(Killed)) %>% arrange(desc(totalkilled)) %>% head(n=10)
str(top_Killed)
top_Killed
top_Killed <- transform(top_Killed, 
                                 Country = reorder(Country, -totalkilled))

X11()
ggplot(top_Killed) + geom_bar(aes(x=Country, y=totalkilled), stat="identity") #+ coord_flip()


top_Killed_A = tapply(terrorism_train$Killed, terrorism_train$Country, sum)
head(top_Killed[order(-top_Killed)], 10)

# Find top-10 cities with most attacks, most injured and most killed respectively and show them with plots

# Top City Injured
top_city_injured = terrorism_train %>%  group_by(City) %>% summarise(totalInjured=sum(Injured)) %>% arrange(desc(totalInjured)) %>% head(n=10)
top_city_injured <- transform(top_city_injured, 
                                 City = reorder(City, -totalInjured))
top_city_injured
x11()
ggplot(top_city_injured) + geom_bar(aes(x=City, y = totalInjured), stat = "identity")


# Top City Killed
top_city_killed = terrorism_train %>%  group_by(City) %>% summarise(totalKilled=sum(Killed)) %>% arrange(desc(totalKilled)) %>% head(n=10)
top_city_killed <- transform(top_city_killed, 
                              City = reorder(City, -totalKilled))
top_city_killed
x11()
ggplot(top_city_killed) + geom_bar(aes(x=City, y = totalKilled), stat = "identity")
class(top_Killed)
dim(top_Killed)


# Top City Frequency
top_city_Freq = as.data.frame(head(sort(table(terrorism_train$City, dnn = "City"),
                                        decreasing = TRUE), n = 10))
top_city_Freq

x11()
ggplot(top_city_Freq) + geom_bar(aes(x=City, y = Freq), stat = "identity")
class(top_Killed)

# Draw a plot that shows the relationship between killed and top-4 countries
dim(top_Killed)
top_Killed$totalkilled
x11()
ggplot(top_Killed) + geom_bar(aes(x=Country, y = totalkilled), stat = "identity")
ggplot(top_Killed) + geom_density(aes(x=totalkilled, group=Country, color=Country))

ggplot(top_Killed) + geom_density(aes(x=log2(totalkilled), group=Country, color=Country))

top4Killed_attacks_train = terrorism_train[terrorism_train$Country %in% top_Killed$Country[1:4],]
x11()
ggplot(top4Killed_attacks_train) + geom_density(aes(x=Killed, group=Country, color=Country))
x11()
ggplot(top4Killed_attacks_train) + geom_density(aes(x=log2(Killed), group=Country, color=Country))

ggplot(top4Killed_attacks_train, aes(x=Killed, y=Injured)) + geom_point(size=2, shape=23)

ggplot(top4Killed_attacks_train, aes(x=Killed, y=Injured)) + geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)


str(top4Killed_attacks_train)

# Clean the description column using following steps:

# attack_description = terrorism_train[,7]
# 
# attack_description = as.character(attack_description)
# str(attack_description)

# Normalize text: convert entire text to lower case
# attack_description = tolower(attack_description)

# Remove numbers
library(tm)
library(SnowballC)
# The first step in processing text data involves creating a corpus, which refers to a
# collection of text documents.

str(terrorism_train)

########################################################################################
terrorism_train$Description = tolower(terrorism_train$Description) # To Lower
terrorism_train$Description = removeNumbers(terrorism_train$Description) # Remove numbers
terrorism_train$Description = removePunctuation(terrorism_train$Description) # Remove punctuations
terrorism_train$Description = stripWhitespace(terrorism_train$Description) # removing additional whitespaces
terrorism_train$Description = trimws(terrorism_train$Description)
words_all = unlist(strsplit(terrorism_train$Description, split="\\s"))
stem_words = wordStem(words_all, language = "en")
# Trying with dplyr package
stem.df = data.frame(stem_words)
stem.df$stem_words = as.character(stem.df$stem_words)

# Get the top most frequent word stems
stem_words_freq = stem.df %>%  group_by(stem_words) %>% summarise(freq=n()) %>% arrange(desc(freq))

ggplot(head(stem_words_freq, 30)) + geom_bar(aes(x=stem_words, y=freq), stat="identity") + coord_flip()

########################################################################################
str(terrorism_train$Description)
descript_text = terrorism_train$Description
descrip_corpus <- Corpus(VectorSource(terrorism_train$Description))
descrip_corpus
print(descrip_corpus)
inspect(descrip_corpus[1:3])
# Normalize text: convert entire text to lower case
corpus_clean <- tm_map(descrip_corpus, tolower)
# Remove numbers

corpus_clean <- tm_map(corpus_clean, PlainTextDocument)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
# Remove stopwords
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())

# Remove Punctuations
corpus_clean <- tm_map(corpus_clean, removePunctuation)
# Removing additional white spaces
corpus_clean <- tm_map(corpus_clean, stripWhitespace)

descrip_dtm <- DocumentTermMatrix(corpus_clean,PlainTextDocument())
library(wordcloud)

# Draw the wordcloud showing the frequencies of the words used across attacks and also show them via barplot
wordcloud(corpus_clean, min.freq = 20, random.order = FALSE)

# Stem the words
words_all = unlist(strsplit(corpus_clean, split="\\s"))

# Find out the most frequent words across all the attacks from the cleaned description column
# Finding frequent words requires use of the findFreqTerms() function in the tm
# package. This function takes a document term matrix and returns a character vector
# containing the words appearing at least a specified number of times. For instance, the
# following command will display a character vector of the words appearing at least 5
# times in the sms_dtm_train matrix:

wordfreq = as.data.frame(findFreqTerms(descrip_dtm, 5))
names(wordfreq)[1] = "Words"



# To save this list of frequent terms for use later, we'll use the Dictionary() function:

descrip_dict <- DocumentTermMatrix(corpus_clean,list(dictionary = descrip_dtm))

# Car Data
# Problem 2: Explore Car dataset
#   b. Explore all the attributes individually using univariate numerics and graphics using xtabs.
car_data = read.csv("car.data.csv",stringsAsFactors = TRUE)
str(car_data)
names(car_data) = c("Buying","Maint","Doors","Persons","Lug_Boot","Safety","Class")
# Exploring Class
xtabs(~Class, car_data)
prop.table(xtabs(~Class, car_data)) # acc 22%, good 3.9%, unacc 70%, vgood 3.7%
ggplot(car_data) + geom_bar(aes(x=Class))


# Exploring Buying
xtabs(~Buying, car_data)
prop.table(xtabs(~Buying, car_data))
ggplot(car_data) + geom_bar(aes(x=Buying)) # The proportion is same across all the categories

# Exploring Maint

xtabs(~Maint,car_data)
prop.table(xtabs(~Maint,car_data))
ggplot(car_data) + geom_bar(aes(x = Maint))  # The proportion is same across all the categories

xtabs(~Doors,car_data)
prop.table(xtabs(~Doors,car_data)) # The proportion is same across all the categories
ggplot(car_data) + geom_bar(aes(x = Doors))

xtabs(~Persons,car_data)
prop.table(xtabs(~Persons,car_data)) # The proportion is equal across all the 3 categories i.e 1/3rd

xtabs(~Lug_Boot,car_data)
prop.table(xtabs(~Lug_Boot,car_data)) # The proportion is equal across all the 3 categories i.e 1/3rd

xtabs(~Safety,car_data)
prop.table(xtabs(~Safety,car_data)) # The proportion is equal across all the 3 categories i.e 1/3rd


#   c. Explore all the bivariate relationships numerically and graphically.

xtabs(~Buying + Class, car_data)

xtabs(~Safety + Class, car_data) # People are buying low safety cars with unacc only
prop.table(xtabs(~Safety + Class, car_data))
# names(car_data) = c("Buying","Maint","Doors","Persons","Lug_Boot","Safety","Class")

xtabs(~Safety + Lug_Boot + Class,car_data)
prop.table(xtabs(~Maint + Safety,car_data))

ggplot(car_data) + geom_dotplot(aes(x = Buying, y = Class)) 

X11()
ggplot(car_data) + geom_bar(aes(x=Buying, fill=Class)) + facet_grid(Maint~.)

