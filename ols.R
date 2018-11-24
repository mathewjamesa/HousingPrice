#load libraries
library('broom') #
library('ggplot2') #plotting
library('naniar') #visualizing NA's
library('tidyverse') #data manipulation
library('corrplot') #for corrplot.mixed
library('psych') #to select vars by skewness
library('caret') #for split train/test
library('MASS') #for stepwise variable selection
library('glmnet') #for lasso variable selection
library('mctest') #to detect multicollinearity


#load data
getwd()
load('data_final1122.Rdata')

#do some final data preparation
#drop some columns
data_final <- data_final %>% dplyr::select(-one_of('ZIP','LOCATION', 'URL', 'community_no'))

#Fix naming conflicts
colnames(data_final)[colnames(data_final)=="perc_16+_unempl"] = "perc_16plus_unempl"
colnames(data_final)[colnames(data_final)=="perc_25+_no_school_diploma"] = "perc_25plus_no_school_diploma"

#Calculate building age instead of year built variable
data_final$age <- 2018 - data_final$YEAR_BUILT
#for buildings being built - set age to zero
data_final$age[data_final$age<0] <- 0
data_final$YEAR_BUILT <- NULL

summary(data_final$BATHS)

#convert property type, beds and baths to factor
data_final$PROPERTY_TYPE <- as.factor(data_final$PROPERTY_TYPE)
data_final$BEDS <- as.factor(data_final$BEDS)
data_final$BATHS <- as.factor(data_final$BATHS)

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
                            RMSE_log = double(), adjR2 = double(), num_of_pred = numeric(), 
                            comment = character(), stringsAsFactors = FALSE)

#add new observations template
#res <- data.frame(model_no=1, 
#                  RMSE = 0.24, 
#                  RMS_relative_err = 0.02, 
#                  RMSE_log = 0.24, 
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

#we will treat BEDS and BATHS for this model as numeric
priceTrain1$BEDS <- as.numeric(as.character(priceTrain1$BEDS))
priceTest1$BEDS <- as.numeric(as.character(priceTest1$BEDS))
priceTrain1$BATHS <- as.numeric(as.character(priceTrain1$BATHS))
priceTest1$BATHS <- as.numeric(as.character(priceTest1$BATHS))

fit1 <- lm(PRICE~., data=priceTrain1)
fit1Sum <- summary(fit1)
fit1Sum

#create diagnostic plots
plot(fit1)

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
rmseLog1 <- RMSE(log(priceTest1$predictions), log(priceTest1$PRICE))

#add results to models summary
res <- data.frame(model_no=1, 
                  RMSE = rmse1, 
                  RMS_relative_err = rmseRel1, 
                  RMSE_log = rmseLog1,
                  adjR2 = fit1Sum$adj.r.squared, 
                  num_of_pred = 21, 
                  comment = 'All vars, no transformations, BEDS and BATHS numeric')
modelsSummary <- rbind(modelsSummary, res)
modelsSummary

#==============================
#Model 2
#fit MLR: 
#all variables
#beds and baths numeric
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

#we will treat BEDS and BATHS for this model as numeric
priceTrain2$BEDS <- as.numeric(as.character(priceTrain2$BEDS))
priceTest2$BEDS <- as.numeric(as.character(priceTest2$BEDS))
priceTrain2$BATHS <- as.numeric(as.character(priceTrain2$BATHS))
priceTest2$BATHS <- as.numeric(as.character(priceTest2$BATHS))

fit2 <- lm(log(PRICE) ~., data=priceTrain2)
fit2Sum <- summary(fit2)
fit2Sum

#create diagnostic plots
plot(fit2)

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
rmseLog2 <- RMSE(log(priceTest2$predictions), log(priceTest2$PRICE))

#add results to models summary
res <- data.frame(model_no=2, 
                  RMSE = rmse2, 
                  RMS_relative_err = rmseRel2,
                  RMSE_log = rmseLog2,
                  adjR2 = fit2Sum$adj.r.squared, 
                  num_of_pred = 21, 
                  comment = 'All vars, log(Y) transformation')
modelsSummary <- rbind(modelsSummary, res)

modelsSummary

#Comparing RMSE and root-mean-squared Relative Error (log-transforming a monetary output before modeling 
#improves mean relative error (but increases RMSE) compared to modeling the monetary output directly

#we see that residential with 10.5 baths and 12 beds appeared to be predicted to cost around $53M and all predicted values 
#that tend the linear regression to fall to the right have outlying number of baths.

#we will bin the outlying number of beds and baths based on the information from boxplots to a separate category
#and build a separate model
#summary(as.numeric(as.character(data_final$BATHS)))

#==============================
#Model 3
#fit MLR: 
#all variables
#beds and baths factor
#with log transformation of target
#==============================

#plot boxplots for BEDS and BATHS
boxplot(as.numeric(as.character(data_final$BATHS)))
table(as.numeric(as.character(data_final$BATHS)))
#we will bin number of BATHS>=5 into separate group 5+

boxplot(as.numeric(as.character(data_final$BEDS)))
table(as.numeric(as.character(data_final$BEDS)))
#we will bin number of BEDS>=7 into separate group 7+

#assign data for model3
data_final3 <- data_final 

#convert beds and baths to numeric
data_final3$BEDS <- as.numeric(as.character(data_final3$BEDS))
data_final3$BATHS <- as.numeric(as.character(data_final3$BATHS))

#bin beds>=7 to group 7+
data_final3 <- data_final3 %>% mutate(bedsBinned = if_else(BEDS >=7, '7+', as.character(BEDS)))

#bin baths>=5 to group 5+
data_final3 <- data_final3 %>% mutate(bathsBinned = if_else(BATHS >=5, '5+', as.character(BATHS)))

#replace original beds and baths vars with binned
data_final3$BEDS <- data_final3$bedsBinned
data_final3$bedsBinned <- NULL

data_final3$BATHS <- data_final3$bathsBinned
data_final3$bathsBinned <- NULL

#finally convert them to factor
data_final3$BEDS <- as.factor(data_final3$BEDS)
data_final3$BATHS <- as.factor(data_final3$BATHS)


#Doing stratified sampling on original train indices
priceTrain3 <- data_final3[trainInd,]
priceTest3 <- data_final3[-trainInd,]
stopifnot(nrow(priceTrain3) + nrow(priceTest3) == nrow(data_final3))


fit3 <- lm(log(PRICE) ~., data=priceTrain3)
fit3Sum <- summary(fit3)
fit3Sum

#create diagnostic plots
plot(fit3)

#predict on existing data
priceTest3$logPredictions <-  predict(fit3, priceTest3)

#convert log predictions to monetary units
priceTest3$predictions <- exp(priceTest3$logPredictions)

#plot predictions vs actual
ggplot(priceTest3, aes(x = predictions, y = PRICE)) + 
  geom_point() + geom_abline(color = "blue") +
  ggtitle('Model3: House prices prediction vs actual') + 
  scale_x_continuous(labels = scales::comma) + scale_y_continuous(labels = scales::comma)

#residuals <- actual outcome - predicted outcome
priceTest3$residuals <- priceTest3$PRICE-priceTest3$predictions

#plot predictions vs residuals
ggplot(priceTest3, aes(x = predictions, y = residuals)) + 
  geom_pointrange(aes(ymin = 0, ymax = residuals)) + 
  geom_hline(yintercept = 0, linetype = 3) + 
  ggtitle("Model3: residuals vs. linear model prediction") + 
  scale_x_continuous(labels = scales::comma) + scale_y_continuous(labels = scales::comma)


rmse3 <- rmseMetric(priceTest3$predictions, priceTest3$PRICE)
rmseRel3 <- rmseRelMetric(priceTest3$predictions, priceTest3$PRICE)
rmseLog3 <- RMSE(log(priceTest3$predictions), log(priceTest3$PRICE))

#add results to models summary
res <- data.frame(model_no=3, 
                  RMSE = rmse3, 
                  RMS_relative_err = rmseRel3,
                  RMSE_log = rmseLog3,
                  adjR2 = fit3Sum$adj.r.squared, 
                  num_of_pred = 21, 
                  comment = 'All vars, log(Y), binned baths and beds')
modelsSummary <- rbind(modelsSummary, res)

modelsSummary

car::vif(fit3)

#==============================
#Model 4 - NOT VALID - OMIT
#Compute stepwise regression
#with log transformation of target
#==============================

data_final4 <- data_final

#convert BEDS and BATHS to numeric
data_final4$BEDS <- as.numeric(as.character(data_final$BEDS))
data_final4$BATHS <- as.numeric(as.character(data_final$BATHS))

#setting seed
set.seed(11222018)

# Setting repeated 10-fold cross-validation
trainCV <- trainControl(method = "cv", number = 10)

# Train model
stepModel <- train(log(PRICE) ~., data = data_final4,
                    method = "leapSeq", #stepwise selection
                    tuneGrid = data.frame(nvmax = 1:21), #range of number of predictors include in the model
                    trControl = trainCV)

#model results
stepModel$results

#best model
stepModel$bestTune

summary(stepModel$finalModel)

coef(stepModel$finalModel, 19)

#let's fit the model with chosen variables
#copy train/test data for model 4
priceTrain4 <- data_final4[trainInd,]
priceTest4 <- data_final4[-trainInd,]
stopifnot(nrow(priceTrain4) + nrow(priceTest4) == nrow(data_final4))

#dropping percent_level2_school and perc_housing_crowded
priceTrain4 <- priceTrain4 %>% dplyr::select(-one_of('percent_level2_school', 'perc_housing_crowded'))
priceTest4 <- priceTest4 %>% dplyr::select(-one_of('percent_level2_school','perc_housing_crowded'))

fit4 <- lm(log(PRICE) ~., data=priceTrain4)
fit4Sum <- summary(fit4)
fit4Sum

#create diagnostic plots
plot(fit4)

#predict on existing data
priceTest4$logPredictions <-  predict(fit4, priceTest4)

#convert log predictions to monetary units
priceTest4$predictions <- exp(priceTest4$logPredictions)

#plot predictions vs actual
ggplot(priceTest4, aes(x = predictions, y = PRICE)) + 
  geom_point() + geom_abline(color = "blue") +
  ggtitle('Model4: House prices prediction vs actual') + 
  scale_x_continuous(labels = scales::comma) + scale_y_continuous(labels = scales::comma)

#residuals <- actual outcome - predicted outcome
priceTest4$residuals <- priceTest4$PRICE-priceTest4$predictions

#plot predictions vs residuals
ggplot(priceTest4, aes(x = predictions, y = residuals)) + 
  geom_pointrange(aes(ymin = 0, ymax = residuals)) + 
  geom_hline(yintercept = 0, linetype = 3) + 
  ggtitle("Model4: residuals vs. linear model prediction") + 
  scale_x_continuous(labels = scales::comma) + scale_y_continuous(labels = scales::comma)


rmse4 <- rmseMetric(priceTest4$predictions, priceTest4$PRICE)
rmseRel4 <- rmseRelMetric(priceTest4$predictions, priceTest4$PRICE)

#add results to models summary
res <- data.frame(model_no=4, 
                  RMSE = rmse4, 
                  RMS_relative_err = rmseRel4,
                  adjR2 = fit4Sum$adj.r.squared, 
                  num_of_pred = 19, 
                  comment = 'log(Y), stepwise drop percent_level2_school, perc_housing_crowded, BEDS&BATHS num')
modelsSummary <- rbind(modelsSummary, res)

modelsSummary


#==============================
#Model 4: NOT VALID - OMIT
#Haileys transformed data
#==============================
load('data/newdata/trainData.Rdata')
load('data/newdata/testData.Rdata')

fit4 <- lm(PRICE~., data=trainData)
fit4Sum <- summary(fit4)
fit4Sum

#create diagnostic plots
plot(fit4)

#predict on test
testData$predictions <-  predict(fit4, testData)

#plot predictions vs actual
ggplot(testData, aes(x = predictions, y = PRICE)) + 
  geom_point() + geom_abline(color = "blue") +
  ggtitle('Model1: House prices prediction vs actual') + 
  scale_x_continuous(labels = scales::comma) + scale_y_continuous(labels = scales::comma)

#residuals <- actual outcome - predicted outcome
testData$residuals <- testData$PRICE-testData$predictions

#plot predictions vs residuals
ggplot(testData, aes(x = predictions, y = residuals)) + 
  geom_pointrange(aes(ymin = 0, ymax = residuals)) + 
  geom_hline(yintercept = 0, linetype = 3) + 
  ggtitle("Model4: residuals vs. linear model prediction") + 
  scale_x_continuous(labels = scales::comma) + scale_y_continuous(labels = scales::comma)

#calculate rmse and rmse.relative for training
rmse4 <- rmseMetric(testData$predictions, testData$PRICE)
rmseRel4 <- rmseRelMetric(testData$predictions, testData$PRICE)



#add results to models summary
res <- data.frame(model_no=4, 
                  RMSE = rmse4, 
                  RMS_relative_err = rmseRel4, 
                  adjR2 = fit4Sum$adj.r.squared, 
                  num_of_pred = 55, 
                  comment = 'Transformed Y and X')
modelsSummary <- rbind(modelsSummary, res)
modelsSummary


#==============================
#Model 5:
#Lasso variable selection
#==============================

data_final5 <- data_final

#we will treat BEDS and BATHS for this model as numeric
data_final5$BEDS <- as.numeric(as.character(data_final5$BEDS))
data_final5$BATHS <- as.numeric(as.character(data_final5$BATHS))


x=model.matrix(PRICE~.-1,data=data_final5) 
y=data_final5$PRICE

#use trainInd to split to train/test data
lasso.tr <- glmnet(x[trainInd, ], y[trainInd])
lasso.tr

pred <- predict(lasso.tr, x[-trainInd, ])
dim(pred)

rmse <- sqrt( apply((y[-trainInd]-pred)^2, 2, mean) )
plot(log(lasso.tr$lambda), mse, type="b", xlab="Log(lambda)")

lam.best <- lasso.tr$lambda[order(rmse)[1]]
lam.best

coef(lasso.tr, s=lam.best)


#drop columns not outputed by lasso
data_final5 <- data_final5 %>% dplyr::select(-one_of('LATITUDE', 'life_exp_2010', 'unemployment', 
                                                    'perc_household_below_poverty','hardship_index'))


#convert beds and baths to numeric
data_final5$BEDS <- as.numeric(as.character(data_final5$BEDS))
data_final5$BATHS <- as.numeric(as.character(data_final5$BATHS))

#bin beds>=7 to group 7+
data_final5 <- data_final5 %>% mutate(bedsBinned = if_else(BEDS >=7, '7+', as.character(BEDS)))

#bin baths>=5 to group 5+
data_final5 <- data_final5 %>% mutate(bathsBinned = if_else(BATHS >=5, '5+', as.character(BATHS)))

#replace original beds and baths vars with binned
data_final5$BEDS <- data_final5$bedsBinned
data_final5$bedsBinned <- NULL

data_final5$BATHS <- data_final5$bathsBinned
data_final5$bathsBinned <- NULL

#finally convert them to factor
data_final5$BEDS <- as.factor(data_final5$BEDS)
data_final5$BATHS <- as.factor(data_final5$BATHS)


#Doing stratified sampling on original train indices
priceTrain5 <- data_final5[trainInd,]
priceTest5 <- data_final5[-trainInd,]
stopifnot(nrow(priceTrain5) + nrow(priceTest5) == nrow(data_final5))


fit5 <- lm(log(PRICE) ~., data=priceTrain5)
fit5Sum <- summary(fit5)
fit5Sum

#create diagnostic plots
plot(fit5)

#predict on existing data
priceTest5$logPredictions <-  predict(fit5, priceTest5)

#convert log predictions to monetary units
priceTest5$predictions <- exp(priceTest5$logPredictions)

#plot predictions vs actual
ggplot(priceTest5, aes(x = predictions, y = PRICE)) + 
  geom_point() + geom_abline(color = "blue") +
  ggtitle('Model5: House prices prediction vs actual') + 
  scale_x_continuous(labels = scales::comma) + scale_y_continuous(labels = scales::comma)

#residuals <- actual outcome - predicted outcome
priceTest5$residuals <- priceTest5$PRICE-priceTest5$predictions

#plot predictions vs residuals
ggplot(priceTest5, aes(x = predictions, y = residuals)) + 
  geom_pointrange(aes(ymin = 0, ymax = residuals)) + 
  geom_hline(yintercept = 0, linetype = 3) + 
  ggtitle("Model5: residuals vs. linear model prediction") + 
  scale_x_continuous(labels = scales::comma) + scale_y_continuous(labels = scales::comma)


rmse5 <- rmseMetric(priceTest5$predictions, priceTest5$PRICE)
rmseRel5 <- rmseRelMetric(priceTest5$predictions, priceTest5$PRICE)
rmseLog5 <- RMSE(log(priceTest5$predictions), log(priceTest5$PRICE))


#add results to models summary
res <- data.frame(model_no=5, 
                  RMSE = rmse5, 
                  RMS_relative_err = rmseRel5,
                  RMSE_log = rmseLog5,
                  adjR2 = fit5Sum$adj.r.squared, 
                  num_of_pred = 16, 
                  comment = 'All vars, log(Y) transformation, lasso var sel')
modelsSummary <- rbind(modelsSummary, res)

modelsSummary



#==============================
#Model6: 
#MLR: simple model
#==============================



pairs(data_final3[,c(1,11:20)])

data_final6 <- data_final3

#convert beds and baths to numeric
#data_final6$BEDS <- as.numeric(as.character(data_final6$BEDS))
#data_final6$BATHS <- as.numeric(as.character(data_final6$BATHS))


#drop columns that are hard to interpret or collect data for
data_final6 <- data_final6 %>% dplyr::select(-one_of('life_exp_2010', 'unemployment', 'perc_housing_crowded', 
                                                     'perc_household_below_poverty','perc_16plus_unempl', 
                                                     'perc_25plus_no_school_diploma', 'perc_under18_over64', 
                                                     'income_per_capite', 'hardship_index'))

#drop cases 5577, 4602, 208
data_final6 <- data_final6[-c(5577,4602,208),]

#Doing stratified sampling on original train indices
trainInd6 <- createDataPartition(data_final6$PRICE, p = 0.8, list = FALSE)
priceTrain6 <- data_final6[trainInd6,]
priceTest6 <- data_final6[-trainInd6,]
stopifnot(nrow(priceTrain6) + nrow(priceTest6) == nrow(data_final6))

fit6 <- lm(log(PRICE) ~., data=priceTrain6)
fit6Sum <- summary(fit6)
fit6Sum

#create diagnostic plots
plot(fit6)

#predict on existing data
priceTest6$logPredictions <-  predict(fit6, priceTest6)

#convert log predictions to monetary units
priceTest6$predictions <- exp(priceTest6$logPredictions)

#plot predictions vs actual
ggplot(priceTest6, aes(x = predictions, y = PRICE)) + 
  geom_point() + geom_abline(color = "blue") +
  ggtitle('Model6: House prices prediction vs actual') + 
  scale_x_continuous(labels = scales::comma) + scale_y_continuous(labels = scales::comma)

#residuals <- actual outcome - predicted outcome
priceTest6$residuals <- priceTest6$PRICE-priceTest6$predictions

#plot predictions vs residuals
ggplot(priceTest6, aes(x = predictions, y = residuals)) + 
  geom_pointrange(aes(ymin = 0, ymax = residuals)) + 
  geom_hline(yintercept = 0, linetype = 3) + 
  ggtitle("Model6: residuals vs. linear model prediction") + 
  scale_x_continuous(labels = scales::comma) + scale_y_continuous(labels = scales::comma)


rmse6 <- rmseMetric(priceTest6$predictions, priceTest6$PRICE)
rmseRel6 <- rmseRelMetric(priceTest6$predictions, priceTest6$PRICE)
rmseLog6 <- RMSE(log(priceTest6$predictions), log(priceTest6$PRICE))

#add results to models summary
res <- data.frame(model_no=6, 
                  RMSE = rmse6, 
                  RMS_relative_err = rmseRel6,
                  RMSE_log = rmseLog6,
                  adjR2 = fit6Sum$adj.r.squared, 
                  num_of_pred = 12, 
                  comment = '12 vars, log(Y) transformation')
modelsSummary <- rbind(modelsSummary, res)

modelsSummary



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

