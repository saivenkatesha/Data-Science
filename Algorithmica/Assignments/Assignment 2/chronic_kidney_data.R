# Problem 3: Exploring Kidney data
# Load the dataset into frame and convert all the attributes to factor type.
setwd("F://Algorithmica/Assignments/Assignment 2")
kidney_train = read.csv("chronic_kidney_data.csv", header=FALSE, stringsAsFactors = TRUE, sep=c(","), na.strings = c("?"))
str(kidney_train)
names(kidney_train) = c("Age","BP","SG","AL","SU","RBC","PC","PCC","Bacteria","BGR","BU","SC","Sod","Pot","Hemo","PCV","WC","RC","HTN","DM","CAD","Appet","PE","ANE","Class")
# Data Evaluation

# Data Set Information:
#   
#   We use the following representation to collect the dataset 
# age	-	age	
# bp	-	blood pressure 
# sg	-	specific gravity 
# al	- albumin 
# su	-	sugar 
# rbc	-	red blood cells 
# pc	-	pus cell 
# pcc	-	pus cell clumps 
# ba	-	bacteria 
# bgr	-	blood glucose random 
# bu	-	blood urea 
# sc	-	serum creatinine 
# sod	-	sodium 
# pot	-	potassium 
# hemo	-	hemoglobin 
# pcv	-	packed cell volume 
# wc	-	white blood cell count 
# rc	-	red blood cell count 
# htn	-	hypertension 
# dm	-	diabetes mellitus 
# cad	-	coronary artery disease 
# appet	-	appetite 
# pe	-	pedal edema 
# ane	-	anemia 
# class	-	class


kidney_train$Age = as.numeric(kidney_train$Age)
kidney_train$BP = as.numeric(kidney_train$BP)
kidney_train$BGR = as.numeric(kidney_train$BGR)
kidney_train$BU = as.numeric(kidney_train$BU)
kidney_train$SC = as.numeric(kidney_train$SC)
kidney_train$Sod = as.numeric(kidney_train$Sod)
kidney_train$Pot = as.numeric(kidney_train$Pot)
kidney_train$Hemo = as.numeric(kidney_train$Hemo)
kidney_train$PCV = as.numeric(kidney_train$PCV)
kidney_train$WC = as.numeric(kidney_train$WC)
kidney_train$RC = as.numeric(kidney_train$RC)

str(kidney_train)

library(mice)
library(VIM)
library(ggplot2)

# list rows of data that have missing values 
kidney_train[!complete.cases(kidney_train),]

md.pairs(kidney_train)
levels(kidney_train$Class)
kidney_plot <- aggr(kidney_train, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(kidney_train), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))
# b. Explore all the attributes individually using univariate numerics and graphics.

mean(kidney_train$Age)
mean(kidney_train$BP)
mean(kidney_train$BGR)
mean(kidney_train$BU)
mean(kidney_train$SC)
mean(kidney_train$Sod)
mean(kidney_train$Pot)
mean(kidney_train$Hemo)
mean(kidney_train$PCV)
mean(kidney_train$WC)
mean(kidney_train$RC)

# There is missing data in numerical values as well


kidney_train[!complete.cases(kidney_train$PCC),]

# xtabs(~SG, kidney_train) # There are 47 missing values
# xtabs(~AL, kidney_train) # There are 46 missing values
# xtabs(~SU, kidney_train) # There are 49 missing values
# xtabs(~RBC, kidney_train) # There are 151 missing values
# xtabs(~PCC, kidney_train) # There are 4 missing values
# xtabs(~PC, kidney_train) # There are 65 missing values
# xtabs(~Bacteria, kidney_train) # There are 4 missing values
# xtabs(~HTN, kidney_train) # There are 2 missing values
# xtabs(~DM, kidney_train) # There are 4 missing values
# xtabs(~CAD, kidney_train) # There are 65 missing values
# xtabs(~Appet, kidney_train) # There are 4 missing values
# xtabs(~PE, kidney_train) # There are 65 missing values
# xtabs(~ANE, kidney_train) # There are 4 missing values
# xtabs(~Class, kidney_train) # There are 65 missing values

# Imputing the missing values
imputed_Data <- mice(kidney_train, m=5, maxit = 25, method = 'pmm', seed = 500)
summary(imputed_Data)
# Multiply imputed data set

imputed_Data$imp$Class

completeData <- complete(imputed_Data,5)

completeData
kidney_train
kidney_train$Class
completeData$Class[360]

kidney_plot2 <- aggr(completeData, col=c('navyblue','yellow'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(kidney_train), cex.axis=.7,
                    gap=3, ylab=c("Missing data","Pattern"))

xtabs(~Class + ANE + PE + SG, completeData)
ggplot(completeData)+ geom_bar(aes(x = completeData$Class))
ggplot(completeData)+ geom_bar(aes(x = completeData$SU,fill=Class))
ggplot(completeData)+ geom_bar(aes(x = PC))
ggplot(completeData) + geom_bar(aes(x = Age))  + facet_grid(Class ~ .)

completeData$ANE
