titanic_test = read.csv("F:/Algorithmica/Excercises/Kaggle-I/test.csv")
class(titanic_test)
dim(titanic_test)
titanic_test$Survived=0
write.csv(titanic_test[,c("PassengerId","Survived")],"F:/Algorithmica/Excercises/Kaggle-I/submission.csv", row.names = F)
