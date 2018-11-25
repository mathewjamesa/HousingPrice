#load libraries
library('ggplot2') #plotting
library('naniar') #visualizing NA's
library('tidyverse') #data manipulation
library('corrplot') #for corrplot.mixed
library('psych') #to select vars by skewness
library('caret') #for split train/test
library('MASS') #for stepwise variable selection
library('glmnet') #for lasso variable selection
library('mctest') #to detect multicollinearity
library('GGally') #scatterplot matrix and corr
library('ggcorrplot') #plot correlations
library('mgcv') #fit GAM models


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

#VIF and multicollinearity diagnostics functions
#adopted from: http://rstudio-pubs-static.s3.amazonaws.com/281194_8a861a8b5d1e4cd6a5e7be6a9cdce94d.html
VIF <- function(linear.model, no.intercept=FALSE, all.diagnostics=FALSE, plot=FALSE) {
  require(mctest)
  if(no.intercept==FALSE) design.matrix <- model.matrix(linear.model)[,-1]
  if(no.intercept==TRUE) design.matrix <- model.matrix(linear.model)
  if(plot==TRUE) mc.plot(design.matrix,linear.model$model[1])
  if(all.diagnostics==FALSE) output <- imcdiag(design.matrix,linear.model$model[1], method='VIF')$idiags[,1]
  if(all.diagnostics==TRUE) output <- imcdiag(design.matrix,linear.model$model[1])
  output
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
#all variables 
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
                  comment = 'All vars, log(Y) transformation, beds&baths numeric')
modelsSummary <- rbind(modelsSummary, res)

modelsSummary

#Comparing RMSE and root-mean-squared Relative Error (log-transforming a monetary output before modeling 
#improves mean relative error (but increases RMSE) compared to modeling the monetary output directly

#we see that residential with 10.5 baths and 12 beds appeared to be predicted to cost around $53M and all predicted values 
#that tend the linear regression to fall to the right have outlying number of baths.

#we will bin the outlying number of beds and baths based on the information from boxplots to a separate category
#and build a separate model


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



#==============================
#Model 4: 
#Model3 tuning
#Reducing multicollinearity
#==============================

#summary of the best model so far
summary(fit3)

#let's look on the distribution of house prices based on baths
#side-by-side boxplots: price distribution based on number of baths
ggplot(data=data_final3, aes(x=BATHS, y=PRICE, fill=BATHS)) + geom_boxplot() + 
  labs(title = 'Distribution of house prices based on number of baths') + 
  scale_y_continuous(labels = scales::comma)

#we will do two-samples t-tests to compare mean of price between different number of baths 

#t-test of # of baths 1.5 and 2 means
bf1 <- data_final3 %>% filter(BATHS==1)
bf1.5 <- data_final3 %>% filter(BATHS==1.5)
bf2 <- data_final3 %>% filter(BATHS==2)
bf2.5 <- data_final3 %>% filter(BATHS==2.5)
bf3 <- data_final3 %>% filter(BATHS==3)
bf3.5 <- data_final3 %>% filter(BATHS==3.5)
bf4 <- data_final3 %>% filter(BATHS==4)
bf4.5 <- data_final3 %>% filter(BATHS==4.5)
bf5 <- data_final3 %>% filter(BATHS=='5+')

#t-test of difference in mean(price) for baths 2.5 and 3
t.test(bf2.5$PRICE, bf3$PRICE, alternative = "two.sided", var.equal = FALSE)

#t-test of difference in mean(price) for baths 3.5 and 4
t.test(bf3.5$PRICE, bf4$PRICE, alternative = "two.sided", var.equal = FALSE)

#we conclude there is no difference in mean price for group with number of baths 2.5 and 3, 3.5 and 4 and they can be 
#aggregated into one group

#bin baths ==2.5 and ==3 to group 2.5-3
data_final3 <- data_final3 %>% mutate(bathsNew = if_else(BATHS ==2.5 | BATHS==3, '2.5-3', as.character(BATHS)))
data_final3 <- data_final3 %>% mutate(bathsNew2 = if_else(bathsNew ==3.5 | bathsNew==4, '3.5-4', as.character(bathsNew)))

#replace original beds and baths vars with new binned baths
data_final3$BATHS <- data_final3$bathsNew2
data_final3$bathsNew <- NULL
data_final3$bathsNew2 <- NULL


#side-by-side boxplots: price distribution based on number of beds
ggplot(data=data_final3, aes(x=BEDS, y=PRICE, fill=BEDS)) + geom_boxplot() + 
  labs(title = 'Distribution of house prices based on number of beds') + 
  scale_y_continuous(labels = scales::comma)


#we will do two-samples t-tests to compare mean of price between different number of beds groups 

bd0 <- data_final3 %>% filter(BEDS==0)
bd1 <- data_final3 %>% filter(BEDS==1)
bd2 <- data_final3 %>% filter(BEDS==2)
bd3 <- data_final3 %>% filter(BEDS==3)
bd4 <- data_final3 %>% filter(BEDS==4)
bd5 <- data_final3 %>% filter(BEDS==5)
bd6 <- data_final3 %>% filter(BEDS==6)
bd7 <- data_final3 %>% filter(BEDS=='7+')


#t-test between groups of # of beds 3 and 4
t.test(bd3$PRICE, bd4$PRICE, alternative = "two.sided", var.equal = FALSE)

#t-test between groups of # of beds 6 and 7+
t.test(bd6$PRICE, bd7$PRICE, alternative = "two.sided", var.equal = FALSE)

#We are 95% confident that there is no difference in mean price of group with number of beds 3 and 4.
#Same for groups 6 and 7+
#bin beds ==3 and ==4 to group 3-4
data_final3 <- data_final3 %>% mutate(bedsNew = if_else(BEDS ==3 | BEDS==4, '3-4', as.character(BEDS)))
#bin beds ==6 and ==7+ to group 6+
data_final3 <- data_final3 %>% mutate(bedsNew2 = if_else(bedsNew ==6 | bedsNew=='7+', '6+', as.character(bedsNew)))

#replace original beds var with new binned beds
data_final3$BEDS <- data_final3$bedsNew2
data_final3$bedsNew <- NULL
data_final3$bedsNew2 <- NULL


data_final4 <- data_final3


#copy train/test data for model 4
priceTrain4 <- data_final4[trainInd,]
priceTest4 <- data_final4[-trainInd,]
stopifnot(nrow(priceTrain4) + nrow(priceTest4) == nrow(data_final4))


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
rmseLog4 <- RMSE(log(priceTest4$predictions), log(priceTest4$PRICE))


#add results to models summary
res <- data.frame(model_no=4, 
                  RMSE = rmse4, 
                  RMS_relative_err = rmseRel4,
                  RMSE_log = rmseLog4,
                  adjR2 = fit4Sum$adj.r.squared, 
                  num_of_pred = 21, 
                  comment = 'log(Y), optimized number of factors beds&baths')
modelsSummary <- rbind(modelsSummary, res)

modelsSummary


#let's investigate multicollinearity
summary(fit4)


numericVars <- c('PRICE', 'SQUARE_FEET', 'LATITUDE', 'LONGITUDE', 'min_dist_cta', 'num_cta_1mile', 
                 'crime_per_1000', 'life_exp_2010', 'unemployment', 'perc_housing_crowded', 
                 'perc_household_below_poverty', 'perc_16plus_unempl', 'perc_25plus_no_school_diploma', 
                 'perc_under18_over64', 'income_per_capite', 'hardship_index',
                 'percent_level1_school', 'percent_level2_school', 'age')

corr <- round(cor(data_final4[,numericVars]), 2)
ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE)

#there are high correlation between variables:
# life_exp_2010 and income_per_capite and hardship_index
# perc_25plus_no_school_diploma vs hardship_index vs income_per_capite
# perc_housing_crowded vs perc_25plus_no_school_diploma vs hardship_index
# perc_16plus_unelp vs hardship_index vs life_exp_2010
# unemployment vs perc_16plus_unelp vs hardship_index vs life_exp_2010
# perc_household_below_poverty vs unemployment vs perc_16plus_unelp vs hardship_index vs life_exp_2010
# perc_under18_over_64 vs unemployment vs perc_16plus_unelp vs perc_25plus_no_school_diploma vs hardship_index vs income_per_capite

# It looks like we can remove the following variables:
# life_exp_2010
# income_per_capite
# perc_25plus_no_school_diploma
# perc_16plus_unelp
# perc_housing_crowded
# perc_household_below_poverty
# perc_under18_over_64


#let's check VIF
VIF(fit4, all.diagnostics = T)

data_final42 <- data_final4
data_final42 <- data_final42 %>% dplyr::select(-one_of(
  #'life_exp_2010', 
  'unemployment', 
  #'perc_housing_crowded',
  'perc_household_below_poverty', 
  'perc_16plus_unempl', 
  'perc_25plus_no_school_diploma',
  #'perc_under18_over64', 
  'hardship_index',
  'income_per_capite'
))


#copy train/test data for model 42
priceTrain42 <- data_final42[trainInd,]
priceTest42 <- data_final42[-trainInd,]
stopifnot(nrow(priceTrain42) + nrow(priceTest42) == nrow(data_final42))


#fit new model without dropped vars
fit42 <- lm(log(PRICE) ~ ., data=priceTrain42)
fit42Sum <- summary(fit42)
fit42Sum

VIF(fit42, all.diagnostics = T)

#We've removed all variables with multicollinearity
#let's check correlation plot for variables left

numericVars2 <- c('PRICE', 'SQUARE_FEET', 'LATITUDE', 'LONGITUDE', 'min_dist_cta', 'num_cta_1mile', 
                 'crime_per_1000', 'life_exp_2010', 'perc_housing_crowded', 
                 'perc_under18_over64', 'percent_level1_school', 'percent_level2_school', 'age')

corr <- round(cor(data_final4[,numericVars2]), 2)
ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE)

plot(fit42)

#predict on existing data
priceTest42$logPredictions <-  predict(fit42, priceTest42)

#convert log predictions to monetary units
priceTest42$predictions <- exp(priceTest42$logPredictions)

#plot predictions vs actual
ggplot(priceTest42, aes(x = predictions, y = PRICE)) + 
  geom_point() + geom_abline(color = "blue") +
  ggtitle('Model42: House prices prediction vs actual') + 
  scale_x_continuous(labels = scales::comma) + scale_y_continuous(labels = scales::comma)

#residuals <- actual outcome - predicted outcome
priceTest42$residuals <- priceTest42$PRICE-priceTest42$predictions

#plot predictions vs residuals
ggplot(priceTest42, aes(x = predictions, y = residuals)) + 
  geom_pointrange(aes(ymin = 0, ymax = residuals)) + 
  geom_hline(yintercept = 0, linetype = 3) + 
  ggtitle("Model42: residuals vs. linear model prediction") + 
  scale_x_continuous(labels = scales::comma) + scale_y_continuous(labels = scales::comma)


rmse42 <- rmseMetric(priceTest42$predictions, priceTest42$PRICE)
rmseRel42 <- rmseRelMetric(priceTest42$predictions, priceTest42$PRICE)
rmseLog42 <- RMSE(log(priceTest42$predictions), log(priceTest42$PRICE))


#add results to models summary
res <- data.frame(model_no=42, 
                  RMSE = rmse42, 
                  RMS_relative_err = rmseRel42,
                  RMSE_log = rmseLog42,
                  adjR2 = fit42Sum$adj.r.squared, 
                  num_of_pred = 15, 
                  comment = 'model4 with multicollinearity removed')
modelsSummary <- rbind(modelsSummary, res)

modelsSummary

#==============================
#Model5:
# GAM
#==============================

#copy data for model
data_final5 <- data_final42

#copy train/test data for model 5
priceTrain5 <- data_final5[trainInd,]
priceTest5 <- data_final5[-trainInd,]
stopifnot(nrow(priceTrain5) + nrow(priceTest5) == nrow(data_final5))


#let's first create scatterplot matrix
ggpairs(data_final5[,numericVars2])


# Create the formula 
(fmla.gam <- log(PRICE) ~ SQUARE_FEET + s(LATITUDE,LONGITUDE) + s(min_dist_cta) + 
    s(num_cta_1mile) + s(crime_per_1000) + life_exp_2010 + perc_housing_crowded +
     s(perc_under18_over64) + s(percent_level1_school) + s(percent_level2_school) + s(age) + 
    PROPERTY_TYPE + BEDS + BATHS)

# Fit the GAM Model
fit5 <- gam(fmla.gam, family='gaussian', priceTrain5)

#gam model summary
fit5Sum <- summary(fit5)
fit5Sum$r.sq

# plots of vars
plot(fit5)


# Get predictions from best linear model - model 42
priceTest5$predLog.lin <- predict(fit42, newdata = priceTest5)

# Get predictions from gam model - fit5
priceTest5$predLog.gam <- as.numeric(predict(fit5, newdata = priceTest5))

priceTest5$pred.lin <- exp(priceTest5$predLog.lin)
priceTest5$pred.gam <- exp(priceTest5$predLog.gam)

#residuals <- actual outcome - predicted outcome
priceTest5$residuals <- priceTest5$PRICE-priceTest5$pred.gam



#Gather the predictions into a "long" dataset
price5 <- priceTest5 %>%
  gather(key = modeltype, value = pred, pred.lin, pred.gam)

# Calculate the rmse by model
price5 %>%
  mutate(residual = PRICE - pred) %>%     # residuals
  group_by(modeltype) %>%                  # group by modeltype
  summarize(rmse = sqrt(mean(residual^2))) # calculate the RMSE


rmse5 <- rmseMetric(priceTest5$pred.gam, priceTest5$PRICE)
rmseRel5 <- rmseRelMetric(priceTest5$pred.gam, priceTest5$PRICE)
rmseLog5 <- RMSE(log(priceTest5$pred.gam), log(priceTest5$PRICE))


#add results to models summary
res <- data.frame(model_no=5, 
                  RMSE = rmse5, 
                  RMS_relative_err = rmseRel5,
                  RMSE_log = rmseLog5,
                  adjR2 = fit5Sum$r.sq, 
                  num_of_pred = 15, 
                  comment = 'Generalized additive model')
modelsSummary <- rbind(modelsSummary, res)

modelsSummary


#==============================
#Lasso variable selection
#==============================

data_final6 <- data_final42

x=model.matrix(PRICE~.-1,data=data_final6) 
y=data_final6$PRICE

#use trainInd to split to train/test data
lasso.tr <- glmnet(x[trainInd, ], y[trainInd])
lasso.tr

pred <- predict(lasso.tr, x[-trainInd, ])
dim(pred)

rmse <- sqrt( apply((y[-trainInd]-pred)^2, 2, mean) )
plot(log(lasso.tr$lambda), rmse, type="b", xlab="Log(lambda)")

lam.best <- lasso.tr$lambda[order(rmse)[1]]
lam.best

coef(lasso.tr, s=lam.best)



#==============================
#Stepwise variable selection
#==============================
# Setting repeated 10-fold cross-validation
trainCV <- trainControl(method = "cv", number = 10)

# Train model
stepModel <- train(log(PRICE) ~  + ., data = data_final42,
                   method = "leapSeq", #stepwise selection
                   tuneGrid = data.frame(nvmax = 1:21), #range of number of predictors include in the model
                   trControl = trainCV)

#model results
stepModel$results

#best model - 19
stepModel$bestTune

summary(stepModel$finalModel)

coef(stepModel$finalModel, 19)

#==============================
