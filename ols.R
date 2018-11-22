#install.packages('naniar')

#load libraries
library('broom') #
library('ggplot2') #plotting
library('naniar') #visualizing NA's


#load data
getwd()
load('data_model.Rdata')
load('data/newdata/testData.Rdata')
load('data/newdata/trainData.Rdata')

summary(trainData)

#one more check for missing values
gg_miss_var(trainData) + labs(y = "Vars with missing values")

#check missing data and patterns
library(naniar)
library(UpSetR)

gg_miss_upset(trainData)


#==============================
#fit MLR: all variables
#==============================

fitAll <- lm(PRICE~., data=trainData)
summary(fitAll)

#predict on existing data
trainData$prediction <-  predict(fitAll, trainData)


#==============================
#==============================



#==============================
#==============================



#==============================
#==============================



#==============================
#==============================



#==============================
#==============================



#==============================
#==============================



#==============================
#==============================



#==============================
#==============================
