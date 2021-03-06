
# This script creates a sample submission using Random Forests
# and also plots the feature importance from the trained model.
#
# To submit the sample, download 1_random_forest_benchmark.csv.gz
# from the Output tab and submit it as normal to the competition
# (through https://www.kaggle.com/c/otto-group-product-classification-challenge/submissions/attach)
#
# Click "fork" to run this script yourself and make tweaks,
# there's many ways you can improve on this!

library(ggplot2)
library(randomForest)
library(readr)

set.seed(1)
setwd("F:/Algorithmica/Assignments/Assignment 1/Otto Group Product Classification Challenge")
train <- read.csv("Otto_train.csv" )
test  <- read_csv("test.csv")

submission <- data.frame(id=test$id, Class_1=NA, Class_2=NA, Class_3=NA, Class_4=NA, Class_5=NA, Class_6=NA, Class_7=NA, Class_8=NA, Class_9=NA)
submission
rf <- randomForest(train[,c(-1,-95)], as.factor(train$target), ntree=25, importance=TRUE)
submission[,2:10] <- (predict(rf, test[,-1], type="prob"))# +0.01)/1.09

write.csv(submission, file = "submission.csv", row.names=FALSE)
#gz_out <- gzfile("1_random_forest_benchmark.csv.gz", "w")
#gz_out
#writeChar(write_csv(submission, ""), gz_out, eos=NULL)
#close(gz_out)

imp <- importance(rf, type=1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])

p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("Importance") +
  ylab("") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=18))

ggsave("2_feature_importance.png", p, height=20, width=8, units="in")
