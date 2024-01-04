data <- read.csv ("clipboard", sep = "\t", header = T)

#### let us do some basic data cleaning
## will check the missing data
sum(is.na(data))
summary(data)
#### we will apply na.omit function to delte the missing rows
data <- na.omit (data)
##### let us convert the date column, which is currency a string into 'date' vector
#and calculate age (age = date- year_built)
str(data$date)
data$date <- as.Date (data$date, format = "%Y%m%dT000000")

### if I want to pick up year from 'date' column I need the lubridate package
library(lubridate)
data$age <- year(data$date)- data$yr_built

### year_renovated columm rquires a transformation...
data$yr_renovated <- factor (ifelse(data$yr_renovated == 0, 0, 1))

### convert waterfront as factor
data$waterfront <- factor (data$waterfront)

final_data <- data[, -c(1, 2, 6, 7, 15, 17:19)]
####  will run the regression and estiamte the model to understand the 
#importance of these predictors, and we are not looking predicting the outcome now
options(scipen = 10)
model <- lm(price~., data = final_data)
summary(model)

### interpret the regression equation.
#First...interpret the F statistics, and see wther all the IVs.. jointly influence
the DV.

# Result: Here the p value is less than that of alpha 0.05, hence we reject the null 
#(which says joinly no contribution), and go with alterantive hypothesis.
F is also test of R square...it explains whether the R square is statistically
signifiant or substantial.Here we got an rsquare of 65%...65% of the variability
on DV is explained or contributed by IVs.


regression  = 14-1
total = 21611-1
error = total-regression
error





