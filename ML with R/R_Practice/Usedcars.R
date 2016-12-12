getwd()
usedcars <- read.csv("usedcars.csv", stringsAsFactors = FALSE)
str(usedcars)
summary(usedcars$year)
summary(usedcars$price)
summary(usedcars$color)
summary(usedcars$mileage)
summary(usedcars$model)
summary(usedcars$transmission)
mean(c(36000, 44000, 56000))
median(c(36000, 44000, 56000))
range(usedcars$price) # range() function returns both the minimum and maximum value.

# Combining range() with the difference function, diff() allows you to examine the range of
# data with a single command:

diff(range(usedcars$price))
quantile(usedcars$price, probs = c(0.01, 0.99))
IQR(usedcars$mileage) # The difference between Q1 and Q3 is known as the interquartile range (IQR), and can be calculated with the IQR() function:
IQR(usedcars$price)
quantile(usedcars$price)
quantile(usedcars$price, seq(from = 0, to = 1, by = 0.20))

# Visualizations

boxplot(usedcars$price, main="Boxplot of Used Car Prices",
        ylab="Price ($)")
boxplot(usedcars$mileage, main="Boxplot of Used Car Mileage",
        ylab="Odometer (mi.)")


# Histograms
hist(usedcars$price, main = "Histogram of Used Car Prices",
     xlab = "Price ($)")
hist(usedcars$mileage, main = "Histogram of Used Car Mileage",
     xlab = "Odometer (mi.)")

# Statistical Functions
# Variance
var(usedcars$price)
# standard deviation
sd(usedcars$price)
sqrt(var(usedcars$price))
var(usedcars$mileage)
sd(usedcars$mileage)

b = IQR(usedcars$price)
b
# Dealing with Categorical Variables
c = table(usedcars$year)
c
table(usedcars$model)
table(usedcars$color)

model_table <- table(usedcars$model)
prop.table(model_table)


color_table <- table(usedcars$color)
color_pct <- prop.table(color_table) * 100
round(color_pct, digits = 1)
plot(x = usedcars$mileage, y = usedcars$price,
     main = "Scatterplot of Price vs. Mileage",
     xlab = "Used Car Odometer (mi.)",
     ylab = "Used Car Price ($)")
install.packages("gmodels")
library(gmodels)
usedcars$conservative <-
  usedcars$color %in% c("Black", "Gray", "Silver", "White")
usedcars$conservative

table(usedcars$conservative) # two-thirds of cars have conservative colors while one-third do not have conservative colors:

# we're assuming that the model of car dictates the choice of color, we'll treat conservative as the dependent (y) variable.

# The proportions indicate that cell's proportion relative to the Chi-square statistic,the row's total, the columns total, and the table's total.

CrossTable(x = usedcars$model, y = usedcars$conservative)



