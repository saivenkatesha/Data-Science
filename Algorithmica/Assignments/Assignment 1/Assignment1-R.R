# Problem1: Working with Numeric Vectors

# Create the following vectors using R:
# a. (1, 2, 3,...19, 20)

library(ggplot2)

setwd("F://Algorithmica/Assignments/Assignment 1")

vector_a = c(1:20)
vector_a
v1 = 1:20
v1
# Alternate Method using seq()
vector_a1 = seq(1:20)
vector_a1

# b. (20, 19,... 2, 1)
vector_b = c(20:1)
vector_b
v2 = 20:1
v2
# Alternate Method using seq()
vector_b1 = seq(20,1)
vector_b1
# c. (1, 2, 3,...19, 20, 19,... 2, 1)

vector_c = c(1:20,19:1)
vector_c

# Alternate Method using existing objects
vector_c1 = c(vector_a[1:19],vector_b[1:20])
vector_c1


# d. (4, 6, 3, 4, 6, 3,..4, 6, 3) where there are 10 occurrences of 4

d = c(4,6,3)
vector_d = rep(d,times=10)
vector_d
v4 = rep(c(4,6,3), 10)
v4
# e. (4, 6, 3, 4, 6, 3,..4, 6, 3, 4) where there are 11 occurrences of 4, 10 occurrences of 6 and 10 occurences of 3.
vector_e = c(vector_d,4)
vector_e

# f. (4, 4,.4, 6, 6,..6,3,3,..3) where there are 10 occurrences of 4, 20 occurrences of 6 and 30 occurrences of 3.

f4 = rep(4,times=10)
f6 = rep(6,times=20)
f3 = rep(3,times=30)
vector_f = c(f4,f6,f3)
vector_f

v6 = c(rep(4,10), rep(6,20), rep(3,30))
v6
# g. Create a vector of the values of ex cos(x) at x = 3,3.1, 3.2,...,6
v7 = cos(seq(3,6,0.1))
v7
vector_g = seq(length=31,from=3,by=.1)
vector_g



# Problem2: Working with Character Vectors
# Use the function paste to create the following character vectors of length 30:

#a. ("label 1", "label 2", ....., "label 30") Note that there is a single space between label and the number following.


charvector_a = paste(c("label"),1:30,sep = " ")
charvector_a

#b. ("fn1", "fn2", ..., "fn30") In this case, there is no space between fn and the number following.

charvector_b = paste(c("fn"),1:30,sep = "")
charvector_b

# Problem3: Working with Random Vectors

# Execute the following lines which create two vectors of random integers which are chosen with replacement from the integers 0, 1, ... , 999. Both vectors have length 250.

set.seed(50)
x_vec = sample(0:999, 250, replace=T)
y_vec = sample(0:999, 250, replace=T)
x_vec
y_vec

# a. Suppose x = (x1, x2, .., xn) denotes the vector x_vec and y = (y1,y2,...,yn) denotes the vector y_vec. 
# Create the vector (y2 - x1,..., yn - xn???1).

result_vec = y_vec[2:250]-x_vec[1:249]
result_vec
xy_vec = sample(0:999, 250, replace=T)
for(i in 1:length(x_vec)) 
  {
    xy_vec[i]=y_vec[i] - x_vec[i]
  }
xy_vec

# b. Pick out the values in y_vec which are > 600.
y_vecgt600 = y_vec[y_vec>600]
y_vecgt600


# c. What are the index positions in y_vec of the values which are > 600?


y_index_mask = (y_vec > 600) * (1:250)
y_index_mask
y_gt600_index = y_index_mask[y_index_mask > 0]
y_gt600_index

# Easier approach
which(y_vec > 600)


v = c(1,2,3,4,5)
j = 0
for(i in 1:length(y_vec)) 
{
  
  if(y_vec[i]>600){
    print(i) # index positions
    v[j]=i
    j = j + 1}
}
v
# d. What are the values in x_vec which correspond to the values in y_vec which are > 600? (By correspond, we mean at the 
# same index positions)

# d. What are the values in x_vec which correspond to the values in y_vec which are > 600? (By correspond, we mean at the same index positions)
x_vec[y_vec > 600]
# Less elegant approach
x_vec[which(y_vec > 600)]

for(i in 1:length(y_vec)) 
{
  if(y_vec[i]>600)
    print(x_vec[i])
  
}

# e. How many values in y_vec are within 200 of the maximum value of the terms in y_vec? 
bool_vec = (max(y_vec) - y_vec) < 200
bool_vec
length(bool_vec[bool_vec == T])
range1 = c(max(y_vec)-200)
b_vec = y_vec[which(y_vec > range1)]
b_vec
length(b_vec)



for(i in 1:length(y_vec)) 
{
  if(y_vec[i]>range1 && y_vec[i]<max(y_vec))
    print(y_vec[i])
  
}

# f. How many numbers  in x_vec are divisible by 2? (Note that the modulo operator is denoted %%.)

x_vec
mod=0
for(i in 1:length(x_vec)) 
{
  if(x_vec[i]%%2==0)
    mod=mod+1
}
mod
# single line execution
length(x_vec[x_vec %%2 == 0])

# g. Sort the numbers in the vector x_vec in the order of increasing values in y_vec.

p=x_vec[order(y_vec)]
p

# h. Pick out the elements in y_vec at index positions 1, 4, 7, 10, 13,..
y_vec
j=1

for(i in 1:length(y_vec)) 
{
  if(j<=length(y_vec)) 
  {
    print(y_vec[j])
    j=j+3
  }
}
# single line statement
y_vec[seq(1,250,3)]
getwd()

# Problem4: Working with Data Frames

# a. Load the data set into R and make it a dataframe called rain.df. What command did you use?

# Loading the Rain Data Set. I used read.delim() function

rain.df = read.delim("rainfall.dat",header = TRUE, sep = "",skip = 2, as.is = TRUE)


# b. How many rows and columns does rain.df have? How do you know? 
str(rain.df)  

rain.df = read.table("rainfall.dat", header = FALSE, sep="", na.strings = c(-999))
# There are 5069 obs of 27 variables

# c. What command would you use to get the names of the columns of rain.df? What are those names?

columnNames=colnames(rain.df)
columnNames
names(rain.df)
# d. What command would you use to get the value at row 2, column 4? What is the value?

rain.df[2,4] # The value is zero
head(rain.df)

# e. What command would you use to display the whole second row? What is the content of that row?

rain.df[2,]
# f. What does names(rain.df) = c('year', 'month', 'day', 0:23) do? This command changes the headers of the dataframe

t = names(rain.df) = c('year','month','day',0:23)
t

# g. Create a new column called daily which is the sum of the 24 hourly columns

#data$new=sum(data$[,43:167])
rain.df$daily=rowSums(rain.df[,4:27])
dim(rain.df)
str(rain.df)
rain.df$daily
rain.df$date = paste(rain.df$day,rain.df$month,rain.df$year,sep="/")
# h. Make a histogram of the daily rainfall amounts.

qplot(rain.df$daily,geom = "histogram")

ggplot(rain.df, aes(x = rain.df$daily)) + geom_histogram() # Simple Version
  
  
ggplot(rain.df, aes(x = rain.df$daily)) + 
  geom_histogram(bins = 29, col="red",fill = "green", alpha = .2) +
  scale_fill_gradient("Count", low = "green", high = "red") + 
  geom_density(col = "blue") + 
  labs(title = "Histogram of Daily ")


