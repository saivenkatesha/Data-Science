getwd() # Gets the Current working directory
setwd("F://R Lessons/Basics") # Sets the Current working directory to the given path
NDND=read.table("NDND1.csv",header=TRUE,sep=",")
View(NDND) # TO VIEW THE DATA IN THE OBJECT NDND
NDND # By executing this command it will directly print the data in the console
str(NDND) # Displays the structure of the object compactly
NDND$MSISDN<-as.factor(NDND$MSISDN) # Here we are converting the numerical variable into factor

# Changing the integer data to precision data
NDND$ARPU<-as.double(NDND$ARPU)

# Finding the Average or Mean Value of ARPU variable
avg = mean(NDND$ARPU)
avg

