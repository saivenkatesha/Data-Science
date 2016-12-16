# Problem 1: Mean and Standard Deviation
# I. A teacher adjusts the marks of an examination by raising each score by 5 percent. What happens to mean and standard deviation?

marks = c(80,81,89,75,50)
marks_5Perc = 1.05 * marks # Adding 5% to each student
marks # Before adding "80 81 89 75 50"
marks_5Perc # After adding 5% "84.00 85.05 93.45 78.75 52.50"
mean(marks) # 75
mean(marks_5Perc) # 78.75

# Observation on mean is that, the mean increases by 5% post adding 5% to each student

a1 = sd(marks) # standard deviation is 14.84 before adding
b1 = sd(marks_5Perc)  # standard deviation is 15.59 before adding

# Observation on sd is that, the sd also increases by 5% post adding 5% to each student

# II. Would you expect the following marks of an examination to have small or large standard deviation? 92 93 92 94 92 91 92


# If the mean of a dataset is 92 and its standard deviation is 1.6, then

# 68% of the values in the dataset will lie between MEAN-1SD (92-1.29=90.70) and MEAN+1SD (92 +1.29=93.29)
# 99% of the values will lie between MEAN-3SD (25-4.8=20.2) and MEAN+3SD (25+4.8=29.8).
marks2 = c(90,93,92,94,92,91,92)
SD = sd(marks2) # SD is 1.290994
Mean = mean(marks2) # 92
sd1 = .683 * Mean
sd2 = 0.954 *  Mean

# Most of the values lie in between 1SD except 94
Mean - SD # 90.70
Mean + SD # 93.29

x = length(marks2) # 7
# 6 values are in 1SD
z = 6/7*100 # 85.71%
Mean - 2 * SD # 89.41
Mean + 2 * SD # 94.58
z1 = 7/7*100 # 
z1

# III. Which set of data would have the smallest and largest standard deviation?

GraphA = c(20,20,20,20,20)
sdGraphA = sd(GraphA)
sdGraphA # Standard Deviations is 0       --------- This is smallest SD

GraphB = c(10,20,40,20,10)
sdGraphB = sd(GraphB)
sdGraphB # Standard Deviations is 12.2474 --------- This is largest SD

GraphC = c(5,25,40,25,5)
sdGraphC = sd(GraphC)
sdGraphC # Standard Deviations is 15

GraphD = c(30,15,10,15,30)
sdGraphD = sd(GraphD)
sdGraphD # Standard Deviations is 9.354

# IV. If a set of data has a standard deviation of 0, then:
#   A. the mean of the data must be 0
# B. all of the data values are the same. "This is the correct answer"
# C. the data values collected had a sum of 0
# D. the z-score of the mean of the data is equal to 1

mean(GraphA)

# V. Which of the two normal distributions graphed in the diagrams below has a higher standard deviation?

# Answer: The first curve has higher standard deviation as the values are widely spread out.


# VI. Henry played 24 golf games on the same course during each of two seasons. In the first season, his mean score was 75 
# with a standard deviation of 2.1. In the second season, his mean score was 74 with a standard deviation of 3.8. Examining the
# standard deviation of Henry's score for the two seasons, one could conclude which one of the following would be true:
# A. scores were more consistent in the first season

# I. The average mark on a test was 58.3 with a standard deviation of 6.7.
# The zscore of a particular mark was -1.3, what was the mark on test?

# Answer is 49.59
# Answer is 84.75%

14/12
9/7

# Problem 4: Normality Test
# Load data "income.csv" available under datasets(github of algorithmica) branch into R data frame. Do the following things:




setwd("F:/Algorithmica/Assignments/Assignment 4")
getwd()
income_train = read.csv("income.csv",stringsAsFactors = F)
# a) Obtain descriptive statistics for "income", "edu", and "expr". The statistics should include number of observations, 
# min, max, mean, median, std, skewness, kurtosis, quantile(0.25), quantile(0.75).

str(income_train)
dim(income_train)

# There are 5778 Observations with 38 Features

summary(income_train[,c("income","edu","expr")]) # gives min, max, mean, median, quantile(0.25 - 1st quantile), quantile(0.75 - 3rd quantile)
library(e1071)

sd(income_train[,c("income")]) # Standard Deviation of Income is 14259.29
sd(income_train[,c("edu")]) # Standard Deviation of edu is 4.234
sd(income_train[,c("expr")]) # Standard Deviation of expr is 10.4
# Standard deviation values = 14259.3, 4.23, 10.4
lapply(income_train[,c("income","edu","expr")], FUN=sd) # single statement command with lapply()
lapply(income_train[,c("income","edu","expr")], FUN=skewness) # single statement command with lapply()
lapply(income_train[,c("income","edu","expr")], FUN=kurtosis) # single statement command with lapply()

kurtosis(income_train$income) # 129.0978
kurtosis(income_train$edu) #-0.4371895
kurtosis(income_train$expr) # -0.7864796

skewness(income_train$income) # 7.54789
skewness(income_train$edu) # -0.3859875
skewness(income_train$expr) # 0.07529347

# b) Does income, education and expr of all people follows normal distribution individually?

# c) Does income, education and expr of male people follows normal distribution individually?

lapply(income_train[income_train$female == 0, c("income","edu","expr")], FUN=skewness)
# Kurtosis values = 38.2, -0.035, -0.846
lapply(income_train[income_train$female != 0, c("income","edu","expr")], FUN=kurtosis)
# d) Does income, education and expr of female people follows normal distribution individually?


