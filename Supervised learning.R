# Load libraries
installed.packages("dplyr")
install.packages("tidyverse")
install.packages("lattice")
library(dplyr)
library(tidyverse)
library(lattice)

#identify the currrent working directory
getwd()

#set your working directory
setwd("C:/Users/Ank/Desktop/MoM/Term 3/BSMM 8710 - Intro to Data Analytics/Assignment/Lab Assignment 02")

#read csv file
cardata <- read.csv("CarPrice_Assignment.csv", stringsAsFactors = TRUE)

#refine data
modeldata <- cardata[ ,c("price","curbweight","carwidth","carbody","horsepower")]

#summary of data
summary(modeldata)
str(modeldata)

#create charts
pairs(~price+curbweight+carwidth+carbody+horsepower,data=modeldata)

#check Correlation 
cor(modeldata$price, modeldata$curbweight) #0.8539515
cor(modeldata$price, modeldata$carwidth) #0.7822618
cor(modeldata$price, modeldata$horsepower) #0.8020561

#create Linear regression model
result <- lm(price ~ curbweight + carwidth + horsepower + carbody, data = modeldata)
summary(result)

#Linear Assumtion test
splom(~modeldata, groups = NULL, data = modeldata,axis.line.tck = 0, axis.text.alpha = 0)

#variance
plot(predict(result), residuals(result))
points(c(min(result$fitted.values), max(result$fitted.values)), c(0,0), type = "l")

#Normality
hist(result$residuals, main = "")
qqnorm(result$residuals, ylab = "Residuals", main ="")
qqline(result$residuals)


