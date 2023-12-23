
## read the data into R
data <- read.csv ("clipboard", sep = "\t", header = T)

## check the missing values in the data 
sum(is.na(data))

### clean the data columns since it contains unncessary spaces
install.packages ("janitor")
library(janitor)
data <- clean_names(data)

### understand the 33.33 percentile and 66.66 percentile of clv column
percentile_33 <- quantile(data$customer_lifetime_value, prob = .3333)
percentile_66 <- quantile(data$customer_lifetime_value, prob = .6666)
percentile_33
percentile_66

min_clv <- min(data$customer_lifetime_value)
min_clv
?cut
data$customer_lifetime_value <- cut(data$customer_lifetime_value, 
                                     breaks=c(min_clv,percentile_33,percentile_66,Inf), 
                                     labels=c('low_clv','medium_clv','high_clv'),
                                     include.lowest = TRUE)

### let us convert income into three categories (low,medium and high)

percentile_33_in <- quantile(data$income, prob = .3333)
percentile_66_in <- quantile(data$income, prob = .6666)
percentile_33
percentile_66

min_in <- min(data$income)
data$income <- cut(data$income, 
                                    breaks=c(min_in,percentile_33_in,percentile_66_in,Inf), 
                                    labels=c('low_income','medium_income','high_income'),
                                    include.lowest = TRUE)

## convert months_since_last_claim into categories
#1-12 months as "less than 1 year", 12-24 as "1-2 years", above 24 as 'more than 2' 
# decided to split into three groups (less than 1 year, 1-2 years, more than 2)

min_months <- min(data$months_since_last_claim)
min_months
data$months_since_last_claim <- cut(data$months_since_last_claim,
                                     breaks = c(min_months,12, 24,Inf),
                                    labels = c('less than 1 year', '1-2 year',
                                               'more than 2 years'),
                                    include.lowest = T)

## create bins for number of policies
hist(data$number_of_policies)
table(data$number_of_policies)
data$number_of_policies <- cut(data$number_of_policies,
                               breaks = c(1,2,5, Inf),
                               labels = c("low_policy_no", "average_policy_no", 
                                          "high_policy_no"),
                              include.lowest = T)

### create binds for total claim amount
# we can classify or create bins into three groups. (low_claim_amount, medium_claim amount,
                                                       #high_claim_amount)
percentile33_claim_amt <- quantile(data$total_claim_amount, prob = .3333)
percentile66_claim_amt <- quantile(data$total_claim_amount, prob = .6666)
min_claim_amt <- min(data$total_claim_amount)

data$total_claim_amount <- cut(data$total_claim_amount,
                               breaks = c(min_claim_amt,percentile33_claim_amt,
                                          percentile66_claim_amt, Inf),
                               labels = c('low_claim_amount', "medium_claim_amount",
                                          'high_claim_amount'),
                               include.lowest = T)
## check the summary for monthly premium
summary(data$monthly_premium_auto)

percentile_33_mp <- quantile(data$monthly_premium_auto, prob = .3333)
percentile_66_mp <- quantile(data$monthly_premium_auto, prob = .6666)
min_mp <- min(data$monthly_premium_auto)

data$monthly_premium_auto <- cut (data$monthly_premium_auto,
                                  breaks = c(min_mp,percentile_33_mp,
                                             percentile_66_mp, Inf),
                                  labels = c("Low_mp", "Average_mp", "High_mp"),
                                  include.lowest = T)

  
# delete some unncessary columns 
final_data <- data[,-c(1:2,7,15:16)]
class(final_data)
?unclass
# convert all the columns as factor vector
final_data <- as.data.frame(unclass(final_data), stringsAsFactors = TRUE)
final_data$response <-  factor (ifelse(final_data$response == "Yes", "response_yes", 
                                       "response_no"))
str(final_data)

### print the summary of the final_data

summary(final_data)

###
library(arules)

final_data <- as(final_data, "transactions")
summary(final_data)
image(final_data)
inspect(final_data)
items(final_data)
writeClipboard(labels(final_data)) 
show(final_data)
## let us run the association rule mininb without any specification of parameters
#and appearance (just for the sake of undestanding the rules)

rules <- apriori(final_data)
inspect(rules)
##### want to print the association of high_clv 
rules_high <- apriori(final_data, parameter = list(supp=0.04, conf = 0.71, maxlen = 6), 
                      appearance =list(default = "lhs", 
                                       rhs="customer_lifetime_value=high_clv"))

inspect(rules_high)

conf_rules_high<- sort(rules_high , by ="lift", decreasing = T )
inspect(conf_rules_high)
rules_high <- inspect(conf_rules_high[1:11], linebreak = F)

write.csv(rules_high, "high_CLV_customers.csv")

########## extract the rules for customers with low_clv
rules_low <- apriori(final_data, parameter = list(supp=0.02, conf = 0.71, maxlen = 6), 
                     appearance =list(default = "lhs", 
                                      rhs="customer_lifetime_value=low_clv"))

conf_rules_low<- sort(rules_low , by ="lift", decreasing = T )
rules_low <- inspect(conf_rules_low[1:3], linebreak = FALSE)

write.csv(rules_low, "low_CLV_customers.csv")

####### extract the rules for customer with medium_clv

rules_medium <- apriori(final_data, parameter = list(supp=0.06, conf = 0.80, maxlen = 6), 
                        appearance =list(default = "lhs", 
                                         rhs="customer_lifetime_value=medium_clv"))
inspect(rules_medium)
conf_rules_medium<- sort(rules_medium , by ="lift", decreasing = T )
medium_clv <- inspect(conf_rules_medium[1:10], linebreak = FALSE)
write.csv(medium_clv, "medium_CLV_customers.csv")
dev.off()
par("mar")
par(mar=c(1,1,1,1))
itemFrequencyPlot(final_data, topN=15)

itemFrequencyPlot(final_data, topN=15, type="absolute", col="red",xlab="PRODUCT NAME", 
                  ylab="ITEM FREQUENCY (absolute)", main="Absolute Item Frequency Plot")

