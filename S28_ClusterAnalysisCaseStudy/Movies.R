data <- read.csv("clipboard", sep = "\t", header = T)
final_data<- scale(data[,-1] )
set.seed(1234)
cluster_two <- kmeans(final_data,2)
cluster_two
cluster_two$tot.withinss
cluster_two$totss
cluster_two$betweenss

cluster_three <- kmeans(final_data,3)
cluster_three
cluster_three$tot.withinss
cluster_three$betweenss

cluster_four <- kmeans(final_data,4)
cluster_four

library(factoextra)
fviz_nbclust(final_data, kmeans, method = "wss")
fviz_nbclust(final_data, kmeans, method = "silhouette")
