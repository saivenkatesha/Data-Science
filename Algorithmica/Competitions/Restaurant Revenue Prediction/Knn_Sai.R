
setwd("F://Algorithmica/Competitions/Restaurant Revenue Prediction")

restaurant_train = read.csv("train.csv", header = TRUE, na.strings=c("NA",""))
dim(restaurant_train)
str(restaurant_train)
levels(restaurant_train$City.Group)
#restaurant_train <- restaurant_train[,-c(2,3,5)] # droping the features open date, city, Type

restaurant_test = read.csv("test.csv", header = TRUE, na.strings=c("NA",""))
str(restaurant_test)
levels(restaurant_test$City.Group)
#restaurant_test1 = restaurant_test[,-c(2,3,5)] 

library(class)
sqrt(137)
# As our training data includes 137 instances, we might try k = 12, an odd numberroughly equal to the square root of 137

restaurant_train_class <- restaurant_train[, 4]
str(restaurant_train_class)
length(restaurant_train_class)
length(restaurant_test$City.Group)

restaurant_test_class <- restaurant_test[, 4]
str(restaurant_test_class)
class(restaurant_test_class)
restaurant_test$revenue=0

length(restaurant_train)
length(restaurant_test)
length(restaurant_test_class)


restaurant_train_pred <- knn(train = restaurant_train, test = restaurant_test,cl = restaurant_test_class, k=12)


# Step 4 - evaluating model performance

install.packages("gmodels")
library(gmodels)

CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,prop.chisq=FALSE)



# Step 5 - improving model performance

# We will attempt two simple variations on our previous classifier. First, we will employ an alternative method 
# for rescaling our numeric features. Second, we will try several different values for k.

# Transformation - z-score standardization

# Let's see whether z-score standardization can improve our predictive accuracy.

wbcd_z <- as.data.frame(scale(wbcd[-1]))


# To confirm that the transformation was applied correctly, we can look at the summary statistics:
summary(wbcd_z$area_mean)

# As we had done before, we need to divide the data into training and test sets, then classify the test instances 
# using the knn() function.

wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE)

str(wbcd_test)
wbcd_test
wbcd_verify["diagnosisv"] = NA
str(wbcd_verify)
wbcd_verify$diagnosisv = wbcd_test_pred
write.csv(wbcd_verify, file = "verify.csv", row.names=TRUE)

# Testing alternative values of k

