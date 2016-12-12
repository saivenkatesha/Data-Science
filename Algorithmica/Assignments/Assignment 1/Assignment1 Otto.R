# Problem Statement
# Identical products to be classified accurately clusterwise  with similar products.  
# The better the classification, the more insights we can generate about our product range.
# Problem5: A First attempt at Predictive Analytics Problems

setwd("F:/Algorithmica/Assignments/Assignment 1/Otto Group Product Classification Challenge")
train = read.csv("Otto_train.csv" )
test = read.csv("test.csv")
submission <- data.frame(id=test$id, Class_1=NA, Class_2=NA, Class_3=NA, Class_4=NA, Class_5=NA, Class_6=NA, Class_7=NA, Class_8=NA, Class_9=NA)
submission
class(train)
dim(train)
str(train)
summary(train)

# Solution

# The first variable is  named an id. This needs to be assumed as unique product id for each observation in the data, 
# we need convert this into character.

train$target = as.factor(train$target)
train$id = as.character(train$id)

test$id = as.character(test$id)
str(test)
# Provided a dataset with 93 features for more than 200,000 products. The objective is to build a predictive model 
# which is able to distinguish between our main product categories. 

# The  variable, target, is of particular interest, as it is the outcome we hope to predict. 
table(train$target)
train[4,6]
# v = colSums(train[,2:93])
# b = data.frame(colSums(train[,2:93],which(train$target == "Class_2")))
# names(b)[1] = "Class_2"
# b
# x_vec[which(y_vec > 600)]
# v
# v = sort(v,decreasing = TRUE)
# v['feat_82']
# Looking at the relative distribution of the variable Target
class_p = round(prop.table(table(train$target)) * 100, digits = 1)
class_p[1]
# summary(ottoGroup_Train)
# max(ottoGroup_Train$feat_1)


# Transformation - normalizing numeric data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
normalize(c(1, 2, 3, 4, 5))

ottoGroup_Train_n <- as.data.frame(lapply(train[2:94], normalize))

#summary(ottoGroup_Train_n[c("feat_1","feat_1","feat_40","feat_60")])

dim(ottoGroup_Train_n)
dim(train)
# Data preparation - creating training and test datasets

library("class")
library(gmodels)
ottoGroup_Train_labels <- train[, 95]
ottoGroup_Train_labels
table(train$target)
table(ottoGroup_Train_labels)
sqrt(61878) # 249
train = train[,-95]
dim(train)
dim(test)
ottoGroup_test_pred <- knn(train = train, test = test,
                      cl = ottoGroup_Train_labels, k=249)

dim(ottoGroup_Train_n)
dim(ottoGroup_Test)        
dim(ottoGroup_Train)
str(ottoGroup_Train_n)
