### reading the data
data <- read.csv ("clipboard", sep ="\t", header = T)

## check the missing data
sum(is.na(data))
data <- na.omit(data)# drop missing rows

## convert date column as date vector and calculate age of the house
data$date <- as.Date(data$date, format = "%Y%m%dT000000")

## need to calculate age of the house 
library(lubridate)
data$age <- year(data$date)- data$yr_built

##### yr_renovated we wil convert into a common format, and then as factor 
data$yr_renovated <- factor (ifelse(data$yr_renovated == 0, 0, 1))

## convert waterfront as  a factor vector
data$waterfront <- factor (data$waterfront)
### I need to drop some columns which is not important from modelling perspective

final_data <- data[, -c(1:2, 6:7, 15, 17:19)]

### we will split the data into train and test
set.seed(1234)
index <- sample (1:nrow(final_data), .80*nrow(final_data))
train <- final_data[index,]
test <- final_data[-index,]

#### let us build the model using train and see any problems in the model
options (scipen = 10)
model_1 <- lm (price~., data = train)
summary(model_1)
######################################
R-square and Adjusted R square will be used for analysing the performance of the model,
where R square will be used to talk about the contribution of IVs on Dv.
But, in case of comparing models, with different  number of IVs, then, 
we need to use adjustred R square, it will give you an idea about the which
model performs better. Higher adjustred R-square while comparing shows that
better the model fit of that model.
Otherwise, if you have a single model, no need to use adjusted R-square.
##########
train$residuals <- model_1$residuals
Q1 <- quantile(train$residuals, p = .25)
Q1
Q3 <- quantile(train$residuals, p = .75)
IQR <- IQR(train$residuals)

#### filter the train data without outlier
library(dplyr)
train_1 <- filter(train, residuals >= (Q1 - 1.5*IQR) & residuals <= (Q3 + 1.5*IQR))

model_2 <- lm(price~., data = train_1[,-15])
summary(model_2)

### check the multicollinearity in the model
library(car)
vif(model_2)# check the vif, if any variables the vif is greater than 5 shows that
#there is a potential issue of multicollinearity. Here there is no potential issue of
#multicollinerity.

## thre is an assumption in regression that the residuals will be
#normally distributed.
hist(model_2$residuals)

## we will use the train model (model_2) to predict the test data and check
#the accuracy of prediction
install.packages ('Metrics')
library(Metrics)

predicted <- predict (model_2, newdata = test)


accuracy <- 1- (mape(test$price, predicted))
accuracy

rmse <- rmse(test$price, predicted)
rmse

## here we found an accuracy of 73%, and rmse of 210737.7, this need to be
#compared against other algoritms (decision tree, random forest or boosting) to identify, 
#whether the use of other #algorithms improve the accuracy or reduce the rmse.








