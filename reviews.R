library(ggplot2)
library(knitr)
library(dplyr)
library(tidyr)
library(factoextra)
library(gridExtra)
library(stats)
library("PerformanceAnalytics")
library(corrplot)
library(gplots)


#getting data from csv
UserId <- data$V1
ArtGalleries <- data$V2
DanceClubs <- data$V3
JuiceBars <- data$V4
Restaurants <- data$V5
Museums <- data$V6
Resorts <- data$V7
ParksPicnics <- data$V8
Beaches <- data$V9
Theaters <- data$V10
Religious <- data$V11
df<-data.frame(UserId, ArtGalleries, DanceClubs, JuiceBars,
              Restaurants, Museums, Resorts, ParksPicnics,
              Beaches, Theaters, Religious)

df1=df[,-1]
df=scale(df[,-1])
summary(df1)
avg=colMeans(df1)
barplot(avg, main = "Bar Plot Example", xlab = "Category", ylab = "Values", col = "blue")
par(mfrow = c(2, 5))
hist(df1$'ArtGalleries',col="red",breaks=c(0,1,2,3,4),xlab="ratings of ArtGalleries")
hist(df1$'DanceClubs',col="red",breaks=c(0,1,2,3,4),xlab="ratings of DanceClubs")
hist(df1$'JuiceBars',col="red",breaks=c(0,1,2,3,4),xlab="ratings of JuiceBars")
hist(df1$'Restaurants',col="red",breaks=c(0,1,2,3,4),xlab="ratings of Restaurants ")
hist(df1$'Museums',col="red",breaks=c(0,1,2,3,4),xlab="ratings of Museums")
hist(df1$'Resorts',col="red",breaks=c(0,1,2,3,4),xlab="ratings of Resorts")
hist(df1$'ParksPicnics',col="red",breaks=c(0,1,2,3,4),xlab="ratings of ParksPicnics")
hist(df1$'Beaches',col="red",breaks=c(0,1,2,3,4),xlab="ratings of Beaches")
hist(df1$'Theaters',col="red",breaks=c(0,1,2,3,4),xlab="ratings of Theaters")
hist(df1$'Religious',col="red",breaks=c(0,1,2,3,4),xlab="ratings of Religious")

par(mfrow = c(2, 5))
boxplot(df1$'ArtGalleries',col="red",xlab="ratings of ArtGalleries")
boxplot(df1$'DanceClubs',col="red",xlab="ratings of DanceClubs")
boxplot(df1$'JuiceBars',col="red",xlab="ratings of JuiceBars")
boxplot(df1$'Restaurants',col="red",xlab="ratings of Restaurants ")
boxplot(df1$'Museums',col="red",xlab="ratings of Museums")
boxplot(df1$'Resorts',col="red",xlab="ratings of Resorts")
boxplot(df1$'ParksPicnics',col="red",xlab="ratings of ParksPicnics")
boxplot(df1$'Beaches',col="red",xlab="ratings of Beaches")
boxplot(df1$'Theaters',col="red",xlab="ratings of Theaters")
boxplot(df1$'Religious',col="red",xlab="ratings of Religious")

correlation_matrix=cor(df1)
correlation_matrix
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x=correlation_matrix, col = col, symm = TRUE)
chart.Correlation(df1, histogram=TRUE, pch=19)

kmReviews <- kmeans(df, centers = 2)
fviz_cluster(kmReviews, data = df, geom = 'point')
k_values <- seq(2, 10, 1)
withins_sum <- sapply(k_values, function(k){
  kmeans(df, centers = k, nstart = 25)$tot.withinss
})
cat("within-cluster sum of squares",withins_sum)
plot(k_values, withins_sum, type='b', xlab="Number of clusters", ylab="Within groups sum of squares", main="WCSS vs K")
km3 <- kmeans(df, centers = 3, nstart = 25)
km4 <- kmeans(df, centers = 4, nstart = 25)
km5 <- kmeans(df, centers = 5, nstart = 25)
km6 <- kmeans(df, centers = 6, nstart = 25)

p3 <- fviz_cluster(km3, df, geom='point') + ggtitle("3 Clusters")
p4 <- fviz_cluster(km4, df, geom='point') + ggtitle("4 Clusters")
p5 <- fviz_cluster(km5, df, geom='point') + ggtitle("5 Clusters")
p6 <- fviz_cluster(km6, df, geom='point') + ggtitle("6 Clusters")

grid.arrange(p3, p4, p5, p6, ncol = 2)
btw_df <- data.frame(k = 3:6, betweenss = sapply(list(km3, km4, km5, km6), function(km) km$betweenss))
kable(btw_df)
kable(data.frame(k=3:10, wcss_decrease=round(abs(diff(withins_sum)), 2)))

km4_centers <- as.data.frame(round(km4$centers, 3))
km4_centers <- km4_centers %>% gather(Activity, Value)
km4_centers$Cluster <- rep(paste0("Cluster", " ", 1:4), 10)
km4_centers <- arrange(km4_centers, Activity)



ggplot(km4_centers, aes(Activity, Value, group=Cluster, fill=Cluster)) +
  geom_bar(stat="identity", position = position_dodge()) +
 theme(axis.text.x = element_text(hjust = 1, angle = 60)) +
  labs(y = "Scaled Mean Rating (k)", fill="") +
 ggtitle("Cluster means")
print("Cluster 1: People in this cluster seem to like visiting dance clubs as well as restaurants and it’s somewhat probable that they are not very religious.")
print("Cluster 2: It looks like the users in this group are very religious ones and they also have preference for cultural activities like going to art galleries.")
print("Cluster 3: This group also likes to go outside, specifically, they tend to enjoy going to theaters and beaches, but don’t seem get along with juice bars or art galleries.")
print("Cluster 4: This cluster really seems to enjoy juice bars, resorts and parks, just like cluster 2, they do not have big preference for activities related to religion.")


distances <- dist(df, method = "euclidean")
hc1 <- hclust(distances, method = "ward.D")

hc <- cutree(hc1, k=4)
spl <- split(as.data.frame(df),hc)

hclustDf <- as.data.frame(round(sapply(spl, colMeans), 3))
names(hclustDf) <- c("Cluster1", "Cluster2", "Cluster3", "Cluster4")
hclustDf$Activity <- rownames(hclustDf)

# Delete row names
rownames(hclustDf) <- NULL


# Reorder columns
hclustDf <- hclustDf[,c(5, 1:4)]
hclustDf <- gather(hclustDf, Cluster, Value, -Activity) %>% arrange(Activity)

ggplot(hclustDf, aes(Activity, Value, group=Cluster, fill=Cluster)) +
  geom_bar(stat="identity", position = position_dodge()) +
  theme(axis.text.x = element_text(hjust = 1, angle = 60)) +
  labs(y = "Scaled Mean Rating(D)", fill="") +
  ggtitle("Cluster means")

dev.new()
plot(hc1)
print("Cluster 1: This cluster really seems to enjoy juice bars, dance clubs and parks, they like to party, but for some reason don’t like beaches.")
print("Cluster 2: This group also likes to go outside, specifically, they tend to enjoy going to art galleries and beaches, but don’t seem get along with dance clubs or museums (which might be contradictory because of their preference for art).")
print("Cluster 3: It looks like the users in this group are very religious and they also have little preference for other activities that are not related to religion.")
print("Cluster 4: People in this cluster seem to like visiting restaurants, resorts and also museums.")
