# ***************
#     K MEANS
# ***************

# DATA ENGINEERING AND ANALYSIS:
# Loading and understanding dataset

setwd("D:/R")
auto_data <- read.csv("auto-data.csv")
str(auto_data)
summary(auto_data)
head(auto_data)

# Data Cleansing
# Clustering reqs all numeric values to be in the same range.
# Need to center and scale dataset.

scaled_num <- scale(auto_data[8:12])
# put the attributes into the main data frame
auto_data[,8:12] <- scaled_num
summary(auto_data)

#Exploratory Data Analysis
par(mfrow=c(1,5))
boxplot(auto_data$HP, col="red")
title("HP")
boxplot(auto_data$RPM, col="blue")
title("RPM")
boxplot(auto_data$MPG.CITY, col="yellow")
title("MPG.CITY")
boxplot(auto_data$MPG.HWY, col="green")
title("MPG.HWY")
boxplot(auto_data$PRICE, col="black")
title("PRICE")

# MODELLING AND PREDICTION:
# Build Clusters for 2 variables
# In order to demonstrate the clusters being formed on a 2d plot,
# We will only use 100 samples and 2 attributes
# HP and PRICE to create 4 clusters

library(class)
# keep the same seed for each execution
# Seed impacts the initial centroid position
# it may impact the actual clusters formed

set.seed(11111)
auto_subset <- auto_data[1:100, c(8,12)]
clusters <- kmeans(auto_subset,4)
clusters

par(mfrow=c(1,1))
plot(auto_subset$HP, auto_subset$PRICE, col=clusters$cluster, pch=20, cex=2)
points(clusters$centers, col="purple", pch=17, cex=3)

# Clustering all data

# First, convert all factors in first 8 columns to numeric
for (i in 1:8) {
    auto_data[,i] = as.numeric(auto_data[,i])
}
summary(auto_data)

set.seed(11111)
clusters <- kmeans(auto_data[,1:12],4)
clusters

# Finding optimal no. of clusters
wssplot <- function(data, nc=15, seed=1234) {
    wss <- (nrow(data)-1)*sum(apply(data,2,var))
    for (i in 2:nc) {
      set.seed(seed)
      wss[i] <- sum(kmeans(data, centers=i)$withinss)
    }
    plot(1:nc, wss, type="b", xlab="Number of clusters",
         ylab ="Within groups sum of squares", col="red")
}
wssplot(auto_data)

# Insights: 3 seems to be the optimal number of clusters
# because it starts to "take a knee" after 3.