data <- read.csv("clipboard", sep = "\t", header = T)
data <- data[,-c(1:4,8)]
data <- data[, -c(8)]
library(dplyr)
str(data)

final_data <- scale(data)
sum(is.na(final_data))


cluster_two <- kmeans(final_data,2)
cluster_two
cluster_two$tot.withinss
cluster_two$totss

#Cluster means:
#  Runs        Avg         SR    Fifties      Fours      Sixes     Salary
#1  0.9349680  0.6410123  0.3955722  0.8372072  0.8945585  0.7526909  0.2991402
#2 -0.7873415 -0.5397999 -0.3331134 -0.7050166 -0.7533124 -0.6338450 -0.2519075

cluster_three <- kmeans(final_data,3)
cluster_three

#Cluster means:
# Runs        Avg         SR     Fifties      Fours      Sixes     Salary
#1  0.2400950  0.2595841  0.1191536  0.03445066  0.2622860  0.1314765 -0.5941081
#2 -0.9315094 -0.7997297 -0.3632996 -0.76066060 -0.8706887 -0.7223743 -0.1387633
#3  1.2590662  1.0156133  0.4598673  1.22566130  1.1305761  1.0432637  0.9574042


cluster_four <- kmeans(final_data,4)
cluster_four

library(factoextra)
fviz_nbclust(final_data,kmeans, "wss")
fviz_nbclust(final_data,kmeans, "silhouette")
