
setwd("E://All About Big Data/Data Science/ML with R/R_Practice")
wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)
str(wbcd)
wbcd <- wbcd[-1] # droping the id feature altogether
table(wbcd$diagnosis)
wbcd
# Many R machine learning classifiers require that the target feature is coded as a factor
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),labels = c("Benign", "Malignant"))

# Proportionating the Benign and Malignant observations in terms of %
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)


summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])
# Looking at the features side-by-side,the distance calculation for kNN is heavily dependent upon the
# measurement scale of the input features. As smoothness_mean ranges from 0.05 to  0.16, while area_mean 
# ranges from 143.5 to 2501.0, the impact of area is going to be much larger than smoothness in the distance calculation. 
# This could potentially cause problems for our classifier, so let's apply normalization to rescale the features to a
# standard range of values.


# Transformation - normalizing numeric data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Let's test the normalize function on a couple of vectors:

normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))

# Normalizing the wbcd dataset excluding the first column diagnosis which is a factor variable

wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))

#  To confirm that the transformation was applied correctly, let's look at one variable's summary statistics:
summary(wbcd_n$area_mean)

# Data preparation - creating training and test datasets

# To validate our model fitting we will split the given data into two portions. Once we will consider for training and the other
# for testing the model fitness.

wbcd_train <- wbcd_n[1:469, ] # Training data set
wbcd_test <- wbcd_n[470:569, ] # Test Data Set
wbcd_verify <- wbcd[470:569,]
head(wbcd_verify,n=10) # displays top 10 rows
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]
wbcd_train_labels
wbcd_test_labels
# Step 3 - training a model on the data

# To classify our test instances, we will use a kNN implementation from the class package, which provides a set of 
# basic R functions for classification.
#install.packages("class")
library(class)

# As our training data includes 469 instances, we might try k = 21, an odd numberroughly equal to the square root of 469.
length(wbcd_train)
length(wbcd_test)
length(wbcd_train_labels)
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,cl = wbcd_train_labels, k=21)


# Step 4 - evaluating model performance

#install.packages("gmodels")
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

