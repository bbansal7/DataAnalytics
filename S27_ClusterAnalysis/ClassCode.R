data <- read.csv("clipboard", sep = "\t", header= T)
library(dplyr)
names(data)
data1 <- data %>% filter(Year ==2019) %>% rename(BUR= Data.Rates.Property.Burglary, LAR = Data.Rates.Property.Larceny,
                                                 MOT= Data.Rates.Property.Motor, ASS = Data.Rates.Violent.Assault,
                                                 MUR= Data.Rates.Violent.Murder, RAP = Data.Rates.Violent.Rape,
                                                 ROBB= Data.Rates.Violent.Robbery) %>%select (State, BUR, LAR, MOT, ASS, MUR, RAP, ROBB) %>%
  filter(State != "United States")
?scale
final_data <- scale(data1[,-1])

set.seed(1234)
cluster_two <- kmeans(final_data,2)
summary(cluster_two)
cluster_two
cluster_two$size
data1$cluster <- cluster_two$cluster
cluster_two$totss
cluster_two$tot.withinss
cluster_two$betweenss

set.seed(1234)
cluster_three <- kmeans(final_data,3)
cluster_three
data1$cluster_three <- cluster_three$cluster

set.seed(1234)
cluster_four <- kmeans(final_data,4)
cluster_four
data1$cluster_four <-cluster_four$cluster

install.packages("factoextra")
library(factoextra)
fviz_nbclust(final_data, kmeans, method = "wss")
