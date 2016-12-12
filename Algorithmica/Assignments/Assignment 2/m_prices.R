library(formattable) # nice output
library(ggplot2)
library(lubridate)
library(rworldmap)
library(dplyr)
library(reshape2)

setwd("F://Algorithmica/Assignments/Assignment 2")

m_prices = read.csv("m_prices.csv",stringsAsFactors = FALSE)
str(m_prices)
m_prices$state = as.factor(m_prices$state)
m_prices$location=as.factor(m_prices$location)
m_prices$quantity=as.factor(m_prices$quantity)
m_prices$date_old=as.Date(m_prices$date_old,"%Y-%m-%d")

str(m_prices)
m_prices
head(m_prices)

# 1: filter to keep three states. 
basic_summ = filter(m_prices, state %in% c("California", "New York", "Illinois"))

# 2: set up data frame for by-group processing. 
basic_summ = group_by(basic_summ, quality, state)


# 3: calculate the three summary metrics
basic_summ = summarise(basic_summ,
                       sum_amount = sum(amount),
                       avg_ppo = mean(ppo),
                       avg_ppo2 = sum(price) / sum(amount))
basic_summ
basic_summ = group_by(basic_summ, quality, state)
basic_summ_t = dcast(basic_summ, state ~ quality, value.var = "avg_ppo2")
basic_summ_t