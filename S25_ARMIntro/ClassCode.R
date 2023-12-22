data <- read.csv("clipboard", sep = "\t", header= T, colClasses = "factor")
str(data)
library(arules)
?apriori
rules <- apriori(data)
inspect(rules)

#with increased parameters
 rules <- apriori(data, parameter = list(maxlen=3, minlen=2, supp=0.7, conf = 0.7))
inspect(rules) 
table(data$Nail.Polish)
table(data$Nail.Polish, data$Blush)

rules <- apriori(data, parameter = list(maxlen = 3, minlen= 2, supp= 0.4, conf= 0.5), appearance = list(rhs = c("Foundation=Yes"), default = "lhs"))
inspect(rules)

rules <- apriori(data, parameter = list(maxlen=3, minlen=2, supp=0.1, conf=0.5), appearance = list(rhs= c("Foundation=Yes"),lhs= c("Bronzer=Yes","Lip.liner=Yes","Nail.Polish=Yes","Mascara=Yes",
                                                                                                                                   "Brushes=Yes","Bag=Yes","Lip.Gloss=Yes","Concealer=Yes",
                                                                                                                                   "Eyebrow.Pencils=Yes","Blush=Yes","Lipstick=Yes","Eyeliner=Yes"), default = "none"))                
inspect(rules)
