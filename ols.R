#load libraries
library('broom') #
library('ggplot2') #plotting
library('naniar') #visualizing NA's
library('tidyverse') #data manipulation
library('corrplot') #for corrplot.mixed
library('psych') #to select vars by skewness
library('caret') #for split train/test

#load data
getwd()
load('data_final1122.Rdata')

#do some final data preparation
#drop some columns
data_final <- data_final %>% select(-one_of('ZIP','LOCATION', 'URL', 'community_no'))

#Fix naming conflicts
colnames(data_final)[colnames(data_final)=="perc_16+_unempl"] = "perc_16plus_unempl"
colnames(data_final)[colnames(data_final)=="perc_25+_no_school_diploma"] = "perc_25plus_no_school_diploma"

#Calculate building age instead of year built variable
data_final$age <- 2018 - data_final$YEAR_BUILT
#for buildings being built - set age to zero
data_final$age[data_final$age<0] <- 0
data_final$YEAR_BUILT <- NULL

#save not transformed data
#save(data_final, file = 'data/df_orig.Rdata')


#==============================
#Define functions
#==============================

#define function to calculate RMSE
rmseMetric <- function(pred, act){
  #inputs:
  #pred - vector of predicted values
  #act - vector of actual values
  resid <- pred - act
  resid2 <- resid^2
  rmse <- sqrt(mean(resid2))
  return (rmse)
}


#define function to calculate RMSE.relative
rmseRelMetric <- function(pred, act){
  #inputs:
  #pred - vector of predicted values
  #act - vector of actual values
  resid <- pred - act
  relerr <- resid/act
  relerr2 <- relerr^2
  rmseRel <- sqrt(mean(relerr2))
  return (rmseRel)
}

#==============================
#Split data: train/test
#==============================
#setting seed
set.seed(11222018)

#Doing stratified sampling
trainInd <- createDataPartition(data_final$PRICE, p = 0.8, list = FALSE)
priceTrain <- data_final[trainInd,]
priceTest <- data_final[-trainInd,]
stopifnot(nrow(priceTrain) + nrow(priceTest) == nrow(data_final))


#==============================
#Create DF for models parameters and metrics achieved
#==============================
modelsSummary <- data.frame(model_no=numeric(), RMSE = double(), RMS_relative_err = double(),
                            adjR2 = double(), num_of_pred = numeric(), comment = character(), 
                            stringsAsFactors = FALSE)

#add new observations template
#res <- data.frame(model_no=1, 
#                  RMSE = 0.24, 
#                  RMS_relative_err = 0.02, 
#                  adjR2 = 0.87, 
#                  num_of_pred = 20, 
#                  comment = 'All vars')
#modelsSummary <- rbind(modelsSummary, res)


#==============================
#Model 1
#fit MLR: 
#all variables (-'community_no') 
#without transformation of target
#==============================

#copy train/test data for model 1
priceTrain1 <- priceTrain
priceTest1 <- priceTest

fit1 <- lm(PRICE~., data=priceTrain1)
fit1Sum <- summary(fit1)
fit1Sum


#predict on test
priceTest1$predictions <-  predict(fit1, priceTest1)

#plot predictions vs actual
ggplot(priceTest1, aes(x = predictions, y = PRICE)) + 
  geom_point() + geom_abline(color = "blue") +
  ggtitle('Model1: House prices prediction vs actual') + 
  scale_x_continuous(labels = scales::comma) + scale_y_continuous(labels = scales::comma)

#residuals <- actual outcome - predicted outcome
priceTest1$residuals <- priceTest1$PRICE-priceTest1$predictions

#plot predictions vs residuals
ggplot(priceTest1, aes(x = predictions, y = residuals)) + 
  geom_pointrange(aes(ymin = 0, ymax = residuals)) + 
  geom_hline(yintercept = 0, linetype = 3) + 
  ggtitle("Model1: residuals vs. linear model prediction") + 
  scale_x_continuous(labels = scales::comma) + scale_y_continuous(labels = scales::comma)

#calculate rmse and rmse.relative for training
rmse1 <- rmseMetric(priceTest1$predictions, priceTest1$PRICE)
rmseRel1 <- rmseRelMetric(priceTest1$predictions, priceTest1$PRICE)



#add results to models summary
res <- data.frame(model_no=1, 
                  RMSE = rmse1, 
                  RMS_relative_err = rmseRel1, 
                  adjR2 = fit1Sum$adj.r.squared, 
                  num_of_pred = 21, 
                  comment = 'All vars, no transformations')
modelsSummary <- rbind(modelsSummary, res)

#==============================
#Model 2
#fit MLR: 
#all variables 
#with log transformation of target
#==============================

#get price distribution on original data
ggplot(data_final, aes(x=PRICE)) + geom_histogram(fill="steelblue", alpha = .8, show.legend = T) + 
  ggtitle('House prices distribution') + scale_x_continuous(labels = scales::comma) 

#predicting mean with the right skewed price distribution will overpredict typical values ->
#we will do log-transformation of target variable

#plot price distribution on log scale
ggplot(data_final, aes(x=log(PRICE))) + geom_histogram(fill="steelblue", alpha = .8, show.legend = T) + 
  ggtitle('Log of House prices distribution') + scale_x_continuous(labels = scales::comma) 

#copy train/test data for model 2
priceTrain2 <- priceTrain
priceTest2 <- priceTest

fit2 <- lm(log(PRICE) ~., data=priceTrain2)
fit2Sum <- summary(fit2)
fit2Sum


#predict on existing data
priceTest2$logPredictions <-  predict(fit2, priceTest2)

#convert log predictions to monetary units
priceTest2$predictions <- exp(priceTest2$logPredictions)

#plot predictions vs actual
ggplot(priceTest2, aes(x = predictions, y = PRICE)) + 
  geom_point() + geom_abline(color = "blue") +
  ggtitle('Model2: House prices prediction vs actual') + 
  scale_x_continuous(labels = scales::comma) + scale_y_continuous(labels = scales::comma)

#residuals <- actual outcome - predicted outcome
priceTest2$residuals <- priceTest2$PRICE-priceTest2$predictions

#plot predictions vs residuals
ggplot(priceTest2, aes(x = predictions, y = residuals)) + 
  geom_pointrange(aes(ymin = 0, ymax = residuals)) + 
  geom_hline(yintercept = 0, linetype = 3) + 
  ggtitle("Model2: residuals vs. linear model prediction") + 
  scale_x_continuous(labels = scales::comma) + scale_y_continuous(labels = scales::comma)


rmse2 <- rmseMetric(priceTest2$predictions, priceTest2$PRICE)
rmseRel2 <- rmseRelMetric(priceTest2$predictions, priceTest2$PRICE)

#add results to models summary
res <- data.frame(model_no=2, 
                  RMSE = rmse2, 
                  RMS_relative_err = rmseRel2,
                  adjR2 = fit2Sum$adj.r.squared, 
                  num_of_pred = 21, 
                  comment = 'All vars, log(Y) transformation')
modelsSummary <- rbind(modelsSummary, res)

modelsSummary

#Comparing RMSE and root-mean-squared Relative Error (log-transforming a monetary output before modeling 
#improves mean relative error (but increases RMSE) compared to modeling the monetary output directly


#==============================
#==============================










#Get numeric variables
numericVars = select_if(data_final, is.numeric) #get all numeric variables only
numericVarNames = names(numericVars) #saving names vector for use later on
numericVarNames

#Check correlation on all numeric variables
cor_numVar = cor(numericVars, use="pairwise.complete.obs") 

#sort on decreasing correlations with PRICE
cor_sorted = as.matrix(sort(cor_numVar[,'PRICE'], decreasing = TRUE))
#select only high corelations
CorHigh = names(which(apply(cor_sorted, 1, function(x) abs(x)>0.3)))
cor_numVar = cor_numVar[CorHigh, CorHigh]

corrplot(cor_numVar, type='upper',tl.col="black", tl.pos = "lt")



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



#==============================
#==============================
