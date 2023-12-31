data <- read.csv("clipboard", sep = "\t", header = T)
sum(is.na(data))
data <- na.omit(data)
str(data$date)
data$date <- as.Date(data$date, format = "%Y%m%dT000000")
str(data$date)
library(lubridate)
data$age <- year(data$date) - data$yr_built
str(data$age)
data$yr_renovated <- factor(ifelse(data$yr_renovated==0,0,1))
data$waterfront <- factor(data$waterfront)
final_data <- data[ , -c(1,2,6,7,15, 17:19)]
options(scipen = 10)
 model <- lm(price~.,data = final_data)
summary(model) 
