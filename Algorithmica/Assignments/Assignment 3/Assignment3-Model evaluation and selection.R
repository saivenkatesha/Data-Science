library(caret)
library(ggplot2)

# Id Age Survived
# 1 25 1
# 2 23 0
# 3 30 1
# 4 35 1
# 5 32 0
# 6 28 1
# 7 13 0
# 8 12 0
# Problem 1: Model Evaluation with different resampling schemes. Do the following

# Create a data frame with above sample data and do required type conversions
Id = seq(1:8)
Age = c(25,23,30,35,32,28,13,12)
Survived = c(1,0,1,1,0,1,0,0)
Model_Eval = data.frame(Id,Age,Survived)
Model_Eval$Survived = as.factor(Model_Eval$Survived)

str(Model_Eval)

# a. Model summary: Find the summary of model to understand the default evaluation strategy used and accuracy of model

summary(Model_Eval)
Model_Eval
# b) Build tree model with gender feature using caret train method(without trainControl argument)

set.seed(10)
tree_model = train(Model_Eval[,c("Id","Age")],Model_Eval$Survived,method = "rpart",tuneGrid = expand.grid(.cp=0.1))

# c) Explore the object returned by train method
  # a. Model summary: Find the summary of model to understand the default evaluation strategy used and accuracy of model
tree_model$results # Accuracy is 0.326 when cp = 0 with warning messages
tree_model$results # Accuracy is 0.364 when cp = 0.1 without warning messages
tree_model$results # Accuracy is 0.408 when cp = 0.5 with warning messages

tree_model$resample
tree_model  # Default is "bootstrap" with 25 repetitions for each tuning parameter. Accuracy = 0.326

# b. Final model: Find out the actual model built by accessing finalModel field of returned object and also check how 
# many observations used in building final model
tree_model$finalModel
# Model says everyone is 'non-survivor'. All 8 observations were used.
# 1) root 8 4 0 (0.5000000 0.5000000)

#    c. Bias and variance: Find out the bias and variance of finalModel across resampling iterations by accessing resample field of the returned object
# This is surely a bug and is worth reporting. This perhaps happens only when there is very less data
tree_model$resample
tree_model$resample$Accuracy

accuracy = mean(tree_model$resample$Accuracy) # 0.326 
bias = 1 - accuracy # 0.674 Bias is the difference between accuracy and 1
variance = var(tree_model$resample$Accuracy) #0.094 Default is "bootstrap" with 25 repetitions for each tuning parameter.Accuracy
# of each tuning parameter from the 25 repititions forms a vector.
# d. Train and validation data: Find the details of train and validation used across resampling iterations via 
# control$index and control$indexout fields of the returned object

tree_model$control$index # train data for best tuning parameter; indices used for training
tree_model$control$indexOut # validation data for best tuning parameter; indices used for validation

# e. Confusion matrices: Find out more deeper details about accuracy of models across resampling iterations via resampledCM field of returned object

tree_model$resampledCM # Gives the number of true+ve, true-ve, false+ve, false-ve predictions in the 'validation' set

# d) Explore all the details expected by part-c above for each of the following options by using train control object in train 
# method:

# i. Repeated holdout(LGOCV) with 3 iterations and 75% train data.

resample_strategy2 = trainControl(method="LGOCV", number = 3, p = 0.75)
tree_model2 = train(Model_Eval[,c("Id","Age")], Model_Eval$Survived, method="rpart", trControl = resample_strategy2)
tree_model2
tree_model2$resample
tree_model2$finalModel
accuracy = mean(tree_model2$resample$Accuracy)
bias = 1 - accuracy
bias
variance = var(tree_model2$resample$Accuracy)
variance

tree_model2$control$index
tree_model2$control$indexOut

# ii. 4-fold cross validation(cv with number =4)

resample_strategy3 = trainControl(method = "cv", number = 4, repeats = 3)
tree_model3 = train(Model_Eval[,c("Id","Age")],Model_Eval$Survived,method = "rpart",trControl = resample_strategy3)
tree_model3
tree_model3$finalModel
tree_model3$resample
tree_model3$resample$Accuracy
accuracy = mean(tree_model3$resample$Accuracy)
bias = 1 - accuracy
bias # 0.5

variance = var(tree_model3$resample$Accuracy)
variance
# iii. 4-fold cross validation with 3 repeats(repeatedcv with number=4 and repeats=3)
resample_strategy4 = trainControl(method = "repeatedcv", number = 4, repeats = 3)
tree_model4 = train(Model_Eval[,c("Id","Age")],Model_Eval$Survived, method = "rpart", trControl = resample_strategy4)
tree_model4
tree_model4$finalModel
tree_model4$resample
tree_model4$resample$Accuracy
accuracy = mean(tree_model4$resample$Accuracy)
bias = 1 - accuracy
bias # 0.5
variance = var(tree_model4$resample$Accuracy)
variance # 0

# iv. Leave one out cross validation(LOOCV)

resample_strategy5 = trainControl(method="LOOCV")
tree_model5 = train(Model_Eval[,c("Id","Age")],Model_Eval$Survived,method="rpart",trControl = resample_strategy5)
tree_model5$finalModel
tree_model5$resample # NULL Value
tree_model5$resample

# v. Bootstrapping with 3 iterations(boot with number = 3)

resample_strategy6 = trainControl(method="boot", number = 3)
tree_model6 = train(Model_Eval[,c("Id","Age")],Model_Eval$Survived,method = "rpart",trControl = resample_strategy6)
tree_model6 # Accuracy is 0.333
tree_model6$resample
tree_model6$resample$Accuracy
tree_model6$finalModel
tree_model6$results
tree_model6$control
tree_model6$control$index
tree_model6$control$indexOut


# Problem 2: Model Selection
# Given the following data with Age, Gender, Location as predictor variables and Survived as target variable:
#   Id Age Gender Location Survived
# 1 25 F S 1
# 2 23 M Q 0
# 3 30 F Q 1
# 4 35 M C 1
# 5 32 F S 0
# 6 28 F S 1
# 7 13 M Q 0
# 8 12 F C 0

# Do the following:
  # a) Create a data frame with above sample data and do required type conversions

Age = c(25,23,30,35,32,28,13,12)
Id = 1:8
Gender = c('F','M','F','M','F','F','M','F')
Location = c('S','Q','Q','C','S','S','Q','C')
survivals = c(1,0,1,1,0,1,0,0)

Model_Selection = data.frame(Id,Age,Gender,Location,survivals)
Model_Selection
str(Model_Selection)
Model_Selection$survivals = as.factor(Model_Selection$survivals)
Model_Selection$Id = as.numeric(Model_Selection$Id)

# b) Build tree model with
# a. Features: gender
strategy = trainControl(method="cv", number = 3)
tree_model = train(Model_Selection[,c("Id","Age")],Model_Selection$survivals,method = "rpart",trControl = strategy)
tree_model$resample
tree_model$results
tree_model$finalModel
# b. Evalution strategy: 10-fold cross validation

tree_model2 = train(Model_Selection[,c("Age", "Gender")], Model_Selection$survivals, method="rpart",cp=0.1, trControl = strategy)
tree_model2$finalModel
Model_Selection$survivals

# e) Find out the bias and variance of the above 3 models. Suggest the final model for deployment.

accuracy = mean(tree_model2$resample$Accuracy) # 0.38888

bias = 1 - accuracy
bias # 0.61111

# f) Use the deployed model to predict the outcomes for following test data:
#   Id Age Gender Location
# 9 26 F S
# 10 36 F Q

Id2= 9:10
Age2=c(26,36)
Gender2=c('F', 'F')
Location2=c('S', 'Q')
person_test = data.frame(Id2,Age2,Gender2,Location2)
names(person_test) = c("Id", "Age", "Gender", "Location")
# Predicts all outcomes as 'not survived'
person_test$Survived = predict(tree_model2, person_test)
person_test
