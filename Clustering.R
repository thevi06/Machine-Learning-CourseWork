install.packages("readxl")
install.packages("NbClust")
install.packages("cluster")
install.packages("factoextra")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("corrplot")
install.packages("FactoMineR")
install.packages("fpc")

# Import the Library

library(readxl)
library(NbClust)
library(cluster)
library(factoextra)
library(ggplot2)
library(dplyr)
library(corrplot)
library(FactoMineR)
library(fpc)

# Load the Dataset
vehicle_dataset <- read_excel("vehicles.xlsx")
vehicle_dataset <- vehicle_dataset[,-20]

# Display the Dimensions of Vehicle Dataset
cat("Dimensions of Dataset is : ",dim(vehicle_dataset))

# Create the DataFrame Object of Vehicle Dataset
vehicle_dataset <- as.data.frame(vehicle_dataset)

#                       SubTask No 1



# Preprocessing Vehicle Dataset

#1. Remove the Outliers from Vehicle Dataset

for (i in 1:length(vehicle_dataset)) {
  # Calculating the q1/q3
  q1 <- quantile(vehicle_dataset[,i] , 0.25)
  q3 <- quantile(vehicle_dataset[,i] , 0.75)
  
  # calculate the IQR and Lower and Upper Values
  iqr <- q3 - q1
  lowerValue <- q1 - 1.5 * iqr
  upperValue <- q3 + 1.5 * iqr
  
  # Find out the Outliers
  outliers <- vehicle_dataset[,i] < lowerValue | vehicle_dataset[,i] > upperValue
  
  # Remove the Outliers from Dataset
  vehicle_dataset  <- vehicle_dataset[!outliers,]
}

#2. Normalize/Standardize the Vehicle Dataset
vehicle_dataset <- scale(vehicle_dataset)


#      Find out the Best Clusters and Perform the Kmeans

#1. Method NBclust:

nb <- NbClust(vehicle_dataset , distance = "euclidean", min.nc = 2, max.nc = 14, method = "kmeans")
k <- nb$Best.nc[1]

NB_kmeans <- kmeans(vehicle_dataset, centers = k)

# Calculate the BSS/WSS indices and Ratio of BSS over TSS
BSS <- sum((colMeans(vehicle_dataset) - colMeans(NB_kmeans$centers))^2) * nrow(vehicle_dataset)
WSS <- sum(NB_kmeans$withinss)
TSS <- sum(apply(vehicle_dataset, 2, function(x) sum((x - mean(x))^2)))
BSS_TSS_ratio <- BSS / TSS

# Display the Results NB Cluster
cat("Best Cluster for NB Clust-->",k,"\n")
cat("NB Clust BSS-->", BSS,"\n")
cat("NB Clust Wss-->", WSS,"\n")
cat("NB Clust TSS-->", TSS,"\n")
cat("NB Clust BCC TSS Ratio-->", BSS_TSS_ratio,"\n")


#2. Method Elbow:

elbow <- sapply(1:15, function(k){kmeans(vehicle_dataset, k, nstart=10, iter.max = 150)$tot.withinss})
ggplot(data.frame(k=1:15, WSS=elbow), aes(x=k, y=WSS)) + geom_line() + geom_point() +
  labs(x = "Number of clusters", y = "WSS") + ggtitle("Clusters Elbow Method")

k <- 2
Elbow_Kmeans <- kmeans(vehicle_dataset, k, nstart=10, iter.max=300)

# Calculate BSS/WSS indices and Ratio of BSS over TSS
BSS  <- sum(Elbow_Kmeans$size * apply(Elbow_Kmeans$centers, 1, function(c) sum((c - Elbow_Kmeans$center)^2)))
TSS <- sum(dist(vehicle_dataset)^2)/nrow(vehicle_dataset)
WSS <- TSS - BSS
BSS_TSS_ratio <- BSS/TSS

# Display the Results of Elbow Cluster
cat("Best Cluster for Elbow-->",k,"\n")
cat("Elbow BSS-->", BSS,"\n")
cat("Elbow Wss-->", WSS,"\n")
cat("Elbow TSS-->", TSS,"\n")
cat("Elbow BCC TSS Ratio-->", BSS_TSS_ratio,"\n")


#3. Method Gap Statistics:

Gap <- clusGap(vehicle_dataset, FUN = kmeans, nstart = 20, K.max = 15, B = 50)
k <- maxSE(Gap$Tab[, "gap"], Gap$Tab[, "SE.sim"], method = "Tibs2001SEmax")

Gap_Kmeans <- kmeans(vehicle_dataset, centers = k, nstart = 25)

# Calculate BSS/WSS indices and BSS or WSS Ratio
BSS <- Gap_Kmeans$betweenss
WSS <- Gap_Kmeans$tot.withinss - BSS
total_SS <- Gap_Kmeans$totss
BSS_ratio <- BSS / total_SS
WSS_ratio <- WSS / total_SS

# Display the Results of GAP Cluster
cat("Best Cluster for GAP-->",k,"\n")
cat("GAP BSS-->", BSS,"\n")
cat("GAP Wss-->", WSS,"\n")
cat("GAP BSS Ratio-->", BSS_ratio,"\n")
cat("GAP WSS Ratio-->", WSS_ratio,"\n")


#4. Method silhouette:

Matrix <- as.matrix(vehicle_dataset)
width <- sapply(2:15, function(k){ 
  kmeans_w <- kmeans(Matrix, k)
  avg <- mean(silhouette(kmeans_w$cluster, dist(Matrix)))
})
plot(2:15, width, type="b", xlab="Number of Clusters", ylab="Silhouette Width")

k <- 3   
silhouette_Kmeans<- kmeans(vehicle_dataset, k, nstart = 20)

# Calculate BSS/WSS indices and BSS or WSS Ratio
BSS <- sum(silhouette_Kmeans$size * dist(silhouette_Kmeans$centers)^2)
WSS <- silhouette_Kmeans$tot.withinss
BSS_TSS_ratio <- BSS / (BSS + WSS)

# Display the Result of Silhouette 
cat("Best Cluster for Silhouette -->",k,"\n")
cat("Silhouette  BSS-->", BSS,"\n")
cat("Silhouette  Wss-->", WSS,"\n")
cat("Silhouette BSS and TSS Ratio-->", BSS_TSS_ratio,"\n")


# silhouette Plotting
plot(silhouette(NB_kmeans$cluster, dist(vehicle_dataset))) # NB Clust
plot(silhouette(Elbow_Kmeans$cluster, dist(vehicle_dataset))) # Elbow
plot(silhouette(Gap_Kmeans$cluster, dist(vehicle_dataset))) # Gap Statistic
plot(silhouette(silhouette_Kmeans$cluster, dist(vehicle_dataset))) # Silhouette



#                       SubTask No 2

# Generate the New Dataset by using PCA Analysis

pca_dataset <- PCA(vehicle_dataset, graph = FALSE)

# Create a Attribute of Databaset with PCA
data_trans <- as.data.frame(pca_dataset$ind$coord)
actul_data <- prcomp(data_trans)

# Display the Eign Vectors/Values
cat(pca_dataset$eig)
cat(pca_dataset$var$coord)

# Create the New Dataset with PCA Analysis # Cumulatice Score
cumulative_scores <- cumsum(actul_data$sdev^2 / sum(actul_data$sdev^2))
new_dataset <- data.frame(actul_data$x[, cumulative_scores > 0.92])


#      Find out the Best Clusters and Perform the Kmeans

#1. Method NBclust:

nb <- NbClust(new_dataset , distance = "euclidean", min.nc = 2, max.nc = 14, method = "kmeans")
k <- nb$Best.nc[1]

NB_kmeans <- kmeans(new_dataset, centers = k)

# Calculate the BSS/WSS indices and Ratio of BSS over TSS
BSS <- sum((colMeans(new_dataset) - colMeans(NB_kmeans$centers))^2) * nrow(new_dataset)
WSS <- sum(NB_kmeans$withinss)
TSS <- sum(apply(new_dataset, 2, function(x) sum((x - mean(x))^2)))
BSS_TSS_ratio <- BSS / TSS

# Display the Results NB Cluster
cat("Best Cluster for NB Clust-->",k,"\n")
cat("NB Clust BSS-->", BSS,"\n")
cat("NB Clust Wss-->", WSS,"\n")
cat("NB Clust TSS-->", TSS,"\n")
cat("NB Clust BCC TSS Ratio-->", BSS_TSS_ratio,"\n")

# Calculate the Index of Calinski-Harabasz
cluster_centers <- NB_kmeans$centers
total_mean <- colMeans(new_dataset)
SSB <- sum(apply(cluster_centers, 1, function(center) {
  sum((center - total_mean)^2) * sum(NB_kmeans$size)
}))
SSW <- NB_kmeans$tot.withinss

NB_index <- (SSB / (k - 1)) / (SSW / (nrow(new_dataset) - k))

#2. Method Elbow:

elbow <- sapply(1:15, function(k){kmeans(new_dataset, k, nstart=10, iter.max = 150)$tot.withinss})
ggplot(data.frame(k=1:15, WSS=elbow), aes(x=k, y=WSS)) + geom_line() + geom_point() +
  labs(x = "Number of clusters", y = "WSS") + ggtitle("Clusters Elbow Method")

k <- 2
Elbow_Kmeans <- kmeans(new_dataset, k, nstart=10, iter.max=300)

# Calculate BSS/WSS indices and Ratio of BSS over TSS
BSS  <- sum(Elbow_Kmeans$size * apply(Elbow_Kmeans$centers, 1, function(c) sum((c - Elbow_Kmeans$center)^2)))
TSS <- sum(dist(new_dataset)^2)/nrow(new_dataset)
WSS <- TSS - BSS
BSS_TSS_ratio <- BSS/TSS

# Display the Results of Elbow Cluster
cat("Best Cluster for Elbow-->",k,"\n")
cat("Elbow BSS-->", BSS,"\n")
cat("Elbow Wss-->", WSS,"\n")
cat("Elbow TSS-->", TSS,"\n")
cat("Elbow BCC TSS Ratio-->", BSS_TSS_ratio,"\n")

# Calculate the Index of Calinski-Harabasz
cluster_centers <- Elbow_Kmeans$centers
total_mean <- colMeans(new_dataset)
SSB <- sum(apply(cluster_centers, 1, function(center) {
  sum((center - total_mean)^2) * sum(Elbow_Kmeans$size)
}))
SSW <- Elbow_Kmeans$tot.withinss

Elbow_index <- (SSB / (k - 1)) / (SSW / (nrow(new_dataset) - k))


#3. Method Gap Statistics:

Gap <- clusGap(new_dataset, FUN = kmeans, nstart = 20, K.max = 15, B = 50)
k <- maxSE(Gap$Tab[, "gap"], Gap$Tab[, "SE.sim"], method = "Tibs2001SEmax")

Gap_Kmeans <- kmeans(new_dataset, centers = k+3, nstart = 25)

# Calculate BSS/WSS indices and BSS or WSS Ratio
BSS <- Gap_Kmeans$betweenss
WSS <- Gap_Kmeans$tot.withinss - BSS
total_SS <- Gap_Kmeans$totss
BSS_ratio <- BSS / total_SS
WSS_ratio <- WSS / total_SS

# Display the Results of GAP Cluster
cat("Best Cluster for GAP-->",k,"\n")
cat("GAP BSS-->", BSS,"\n")
cat("GAP Wss-->", WSS,"\n")
cat("GAP BSS Ratio-->", BSS_ratio,"\n")
cat("GAP WSS Ratio-->", WSS_ratio,"\n")

# Calculate the Index of Calinski-Harabasz
cluster_centers <- Gap_Kmeans$centers
total_mean <- colMeans(new_dataset)
SSB <- sum(apply(cluster_centers, 1, function(center) {
  sum((center - total_mean)^2) * sum(Gap_Kmeans$size)
}))
SSW <- Gap_Kmeans$tot.withinss

Gap_index <- (SSB / (k - 1)) / (SSW / (nrow(new_dataset) - k))


#4. Method silhouette:

Matrix <- as.matrix(new_dataset)
width <- sapply(2:15, function(k){ 
  kmeans_w <- kmeans(Matrix, k)
  avg <- mean(silhouette(kmeans_w$cluster, dist(Matrix)))
})
plot(2:15, width, type="b", xlab="Number of Clusters", ylab="Silhouette Width")

k <- 4  
silhouette_Kmeans<- kmeans(new_dataset, k, nstart = 20)

# Calculate BSS/WSS indices and BSS or WSS Ratio
BSS <- sum(silhouette_Kmeans$size * dist(silhouette_Kmeans$centers)^2)
WSS <- silhouette_Kmeans$tot.withinss
BSS_TSS_ratio <- BSS / (BSS + WSS)

# Display the Result of Silhouette 
cat("Best Cluster for Silhouette -->",k,"\n")
cat("Silhouette  BSS-->", BSS,"\n")
cat("Silhouette  Wss-->", WSS,"\n")
cat("Silhouette BSS and TSS Ratio-->", BSS_TSS_ratio,"\n")

# Calculate the Index of Calinski-Harabasz
cluster_centers <- silhouette_Kmeans$centers
total_mean <- colMeans(new_dataset)
SSB <- sum(apply(cluster_centers, 1, function(center) {
  sum((center - total_mean)^2) * sum(silhouette_Kmeans$size)
}))
SSW <- silhouette_Kmeans$tot.withinss

silhouette_index <- (SSB / (k - 1)) / (SSW / (nrow(new_dataset) - k))

# silhouette Plotting
#plot(silhouette(NB_kmeans$clsuster, dist(new_dataset))) # NB Clust
plot(silhouette(Elbow_Kmeans$cluster, dist(new_dataset))) # Elbow
plot(silhouette(silhouette_Kmeans$cluster, dist(new_dataset))) # Silhouette
plot(silhouette(Gap_Kmeans$cluster, dist(new_dataset))) # Gap Statistic
