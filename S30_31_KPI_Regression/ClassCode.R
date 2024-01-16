data <- read.csv("clipboard", sep = '\t', header = T)

sum(is.na(data))
data <- na.omit(data)
sum(is.na(data))

# fix date format
?as.Date
data$date <- as.Date(data$date, format = "%Y%m%dT000000")

#convert independent variables to factor vector
library(lubridate)
data$age =  year(data$date) - data$yr_built
data$yr_renovated = ifelse(data$yr_renovated==0,0,1)
data$waterfront= factor(data$waterfront)
data$yr_renovated= factor(data$yr_renovated)
str(data)
# remove unnecessary data
data <- data[, -c(1,2,6,7,15,17:19)]
set.seed(1234)
index <- sample(1:nrow(data), .80*nrow(data))
train <- data[index,]
test <- data[-index,]
set.seed(1234)
model_1 <- lm(price~., data = train)
options(scipen=10)
summary(model_1)


##outlier detection and removal
train$residuals <- model_1$residuals
q1 <- quantile(train$residuals, p=0.25)
q3 <- quantile(train$residuals, p=0.75)
IQR <- IQR(train$residuals)
?IQR
#Filter the outliers
library(dplyr)
train1 <- filter(train, residuals>= q1-(1.5)*IQR & residuals<= q3+1.5*IQR)

model_2 <- lm(price~., data = train1[,-c(15)])
summary(model_2)
library(car)
vif(model_2)
library(graphics)
hist(model_2$residuals)

library('Metrics')
predicted <- predict(model_2, newdata = test)
?mape
accuracy <- 1- mape(test$price, predicted)
accuracy
?rmse
rmse <- rmse(test$price, predicted)
rmse

##We intend to compare these values to that of other algorithms (decision, random and boosting).

##standard coefficients
library('lm.beta')
lm.beta(model_2)

##stepwise regression
model_3 <- step(model_2, trace=0)
summary(model_3)

model_4<- step(model_2)
