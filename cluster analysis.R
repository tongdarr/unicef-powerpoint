library(rpart)
library(RColorBrewer)
library(ggplot2)
library(cluster)
library(clValid)
library(factoextra)
library(dplyr)
library(fpc)
library(NbClust)


setwd('H:/INFO264/Exercise 1')
cluster <- read.csv("Under-five deaths updated 2.csv")
data <- subset(cluster, Year==2016)
#You can replace 'cluster' with whatever you want to name your dataset.
#Reads the CSV file then sets a new name for the dataset.  
head(cluster,5)


row.names(data) <- data[,1]
#The 'row.names' code uses the values from a column as a key identified.  In this case, the first column [,1] values were used.
#You can now delete the Customer column.  This is the same code we used during the correlation exercise.
data$Location <- NULL
data$Year <- NULL
#Check for change
head(data,5)
#Customer is no longer a column, but the names are still visible.
#Use the Elbow Method to visualize the optimal number of clusters.
# Standardize the data
df <- scale(data)
head(df)
fviz_nbclust(df, kmeans, method = "wss") +
labs(subtitle = "Add an appropriate title")


#Replace ''k' with the number of clusters
DataNameKM <- kmeans(df, 4, iter.max = 20)
#'n' is the number of iterations that will be run to find the best clusters (set to 20).

DataNameKM
#Prints the results.

clusplot (df, DataNameKM$cluster, 
          color = TRUE, 
          line = 0)
#Creates a visualisation of the results.
#Replace ''k' with the number of clusters
km.res <- kmeans(df, 4, nstart=6)
fviz_cluster(km.res,data=df,
             ellipse.type="convex")+theme_minimal()
#Creates a different type of visualisation of the results.





####################################################





library(rpart)
library(RColorBrewer)
library(ggplot2)
library(cluster)
library(clValid)
library(factoextra)
library(dplyr)
library(fpc)
library(NbClust)


setwd('H:/INFO264/Exercise 1')
cluster <- read.csv("Under-five deaths version 3.csv")
cluster <- subset(cluster, Location!='(WHO) Global')
cluster
#You can replace 'cluster' with whatever you want to name your dataset.
#Reads the CSV file then sets a new name for the dataset.  
head(cluster,5)

row.names(cluster) <- cluster[,5]
row.names(cluster) <- make.names(cluster[,1], unique=TRUE) # The good one
#The 'row.names' code uses the values from a column as a key identified.  In this case, the first column [,1] values were used.
#You can now delete the Customer column.  This is the same code we used during the correlation exercise.
cluster$Location <- NULL
cluster$Year <- NULL
#Check for change
head(cluster,5)
#Customer is no longer a column, but the names are still visible.
#Use the Elbow Method to visualize the optimal number of clusters.
# Standardize the data
df <- scale(cluster)
head(df)
fviz_nbclust(df, kmeans, method = "wss") +
  labs(subtitle = "Add an appropriate title")


#Replace ''k' with the number of clusters
DataNameKM <- kmeans(df, 4, iter.max = 20)
#'n' is the number of iterations that will be run to find the best clusters (set to 20).

DataNameKM
#Prints the results.

clusplot (df, DataNameKM$cluster, 
          color = TRUE, 
          line = 0)
#Creates a visualisation of the results.
#Replace ''k' with the number of clusters
km.res <- kmeans(df, 4, nstart=6)
fviz_cluster(km.res,data=df,
             ellipse.type="convex")+theme_minimal()
#Creates a different type of visualisation of the results.




d <- dist(df, method="euclidean")
#This calculates the distance (Euclidean distance) and creates a matrix.
res.hc <- hclust(d, method="ward.D2")
#Applies Ward's method of Hierarchical Clustering (hclust).
fviz_dend(res.hc,cex=0.5)
#Displays Dendogram
#you may get an error, just move to next bit of code.
#Cut the tree into different groups

# Cut tree into 4 groups
grp <- cutree(res.hc, k = 4)
# Visualize
plot(res.hc, cex = 0.6) # plot tree
rect.hclust(res.hc, k = 4, border = 2:5) 
# add rectangle

