titanic_test = read.csv("F:/Algorithmica/Excercises/Kaggle-I/test.csv")
dim(titanic_test)
titanic_test$Survived=sample(c(0,1),418,replace=T)
write.csv(titanic_test[,c("PassengerId","Survived")],"F:/Algorithmica/Excercises/Kaggle-I/submission.csv", row.names = F)
