subject_name <- c("John Doe", "Jane Doe", "Steve Graves") # Creating a string vector
temperature <- c(98.1, 98.6, 101.4) # Creating a numeric vector
flu_status <- c(FALSE, FALSE, TRUE) # Creating a boolean vector
temperature[2] # Retrieving 2nd element from the vector
temperature[2:3]  # Retrieving 2nd to 3rd range of values from the vector
temperature[-2]  # Retrieving all the elements excluding 2nd element from the vector
temperature[c(TRUE, TRUE, FALSE)] # Retrieving the elements whose values are true
gender <- factor(c("MALE", "FEMALE", "MALE")) # creating a categorical data by factorising
gender  # Since gender is categorical variable, while displaying it shows the levels of categories i.e Male Female
blood <- factor(c("O", "AB", "A"),
                levels = c("A", "B", "AB", "O")) # We can also explicitly give additional levels while factorizing the data
blood # Even we have not assigned blood group B in the data, the levels show B as category that is available in the blood variable
subject1 <- list(fullname = subject_name[1],
                 temperature = temperature[1],
                 flu_status = flu_status[1],
                 gender = gender[1],
                 blood = blood[1])
subject1
A = c("Hello", 1, 'C',2.1) # When you try to create different types of values in a vector, it automatically converts the values into strings
A
flu_status
temperature
subject1$temperature # A list elements can be accessed either by giving the index number or by variable name using $
subject1[c("temperature", "flu_status")] # 


# Creating a data structures in R with data.frame
pt_data <- data.frame(subject_name, temperature, flu_status,
                      gender, blood, stringsAsFactors = FALSE)

# parameter: stringsAsFactors = FALSE. If we do not specify this option, R will
# automatically convert every character vector to a factor; this a feature which is
# occasionally useful, but is also sometimes excessive.
pt_data
str(pt_data)
pt_data$subject_name
pt_data[c("temperature", "flu_status")]  # extracting the column data of temperature and flu_status coloumns
pt_data[2:3] # extracting the column data of second row and third column
pt_data[1, 2] # extracting the data of first row and second column
pt_data[c(1, 3), c(2, 4)] # extracting the data from rows 1 and 3, and columns 2 and 4:
pt_data[, 1] # To extract all of the rows or columns, rather than listing every one, simply leave the
# row or column portion blank.
# To extract all columns for the first row:
pt_data[1, ]
pt_data[ , ] # extract everything
pt_data # extract everything

# Matrices in R

# To create a matrix, simply supply a vector of data to the matrix() function, along with a parameter specifying the number of rows (nrow) or number of columns (ncol).
# we can use the nrow parameter to request the data to be divided into two rows:

m <- matrix(c('a', 'b', 'c', 'd'), nrow = 2)
m
m <- matrix(c('a', 'b', 'c', 'd'), ncol = 2)
m
# notice that R loaded the first column of the matrix first, then loaded the second column. This is called column-major order.
# With six values, requesting two rows creates a matrix with three columns:

m <- matrix(c('a', 'b', 'c', 'd', 'e', 'f'), nrow = 2)
m
m <- matrix(c('a', 'b', 'c', 'd', 'e', 'f'), ncol = 2)
m
m[1, ] # extracting first row
m[, 1] # extracting first column
write.csv(pt_data, file = "pt_data.csv")
getwd()
setwd("E://All About Big Data/Data Science/ML with R/R_Practice")
