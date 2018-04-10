
HomeData <- read.csv("HMEQ.csv", header=T)

# check the dimension of the data
dim(HomeData)

# check the variable names of the data
names(HomeData)

# check for missing values
sapply(HomeData, function(x) sum(is.na(x)))

# calculate the missing rate (in percentage) for each variable 
colMeans(is.na(HomeData)*100)

# replacing missing values for JOB and REASON with "Unknown" 
# first change the data type from factor to character in columns 5,6
HomeData$REASON <- as.character(HomeData$REASON)
HomeData$JOB <- as.character(HomeData$JOB)
HomeData[ ,5:6][is.na(HomeData[ ,5:6])] <- "Unknown"

# change the columns back from character to factor
HomeData$REASON <- as.factor(HomeData$REASON)
HomeData$JOB <- as.factor(HomeData$JOB)

# frequency distribution of REASON
table(HomeData$REASON)

# frequency distribution of JOB
table(HomeData$JOB)

# performing natural logarithm on variables LOAN, MORTDUE, VALUE, YOJ, and CLAGE
HomeData[ ,c(2,3,4,7,10)] <- log(1+HomeData[ ,c(2,3,4,7,10)])

# imputing missing values
library(randomForest)
HomeData.imputed <- na.roughfix(HomeData) 

#Transfering data into a numeric data matrix for subsequent analyses
#######################################################################
HomeData.numeric <- model.matrix(~.-1, data=HomeData.imputed)
HomeData.new <- as.data.frame(HomeData.numeric)
#######################################################################

# calculating the distance matrix 
# The response variable "BAD" is first removed before this is performed
HomeData.final <- HomeData.new[ ,-1]

# Obtain the distance matrix using the Gower dissimilarity index
library(cluster)

# compute distance/similarity matrix
HomeData_dist <- daisy(HomeData.final, metric="gower", stand=F) 


# I implemented the hierarchical clustering procedure using the "Complete" Linkage method
fit.complete <- hclust(HomeData_dist, method="complete")


K.max <- 30
height <- tail(fit.complete$height, n=K.max)

n.cluster <- tail((nrow(HomeData.final)-1):1, n=K.max)
plot(n.cluster, height, type="b", pch=19, cex=.5, xlab="number of clusters", ylab="height")


plot(fit.complete)
groups <- cutree(fit.complete, k=4) # cut tree into 4 clusters

# draw dendogram with red borders around the 4 clusters

rect.hclust(fit.complete, k=4, border="red")


##########################################################################
##########################################################################

# estimating the number of clusters via gap statistics
num_of_clusters <- clusGap(HomeData.final, FUN=kmeans, K.max=10, B=200); 
num_of_clusters

# plot of gap statistics
plot(num_of_clusters, main = "Gap Statistic", cex.main=1.5)

# checking 
names(num_of_clusters)

#### K- MEANS PARTITIONING 

k <- 4
# K-Means Cluster Analysis
fit <- kmeans(HomeData.final, k) # K cluster solution
# get cluster means
aggregate(HomeData.final,by=list(fit$cluster),FUN=mean)

# Collect Cluster membership for Hierarchical Clustering
member.Hierarchical <- groups
member.Hierarchical[1:20]

# Collect Cluster membership for K-means Clustering
member.Kmeans <- fit$cluster
member.Kmeans[1:20]


library(Rtsne)
tsne <- Rtsne(HomeData.final, dims = 2, perplexity = 30,theta = 0.5, check_duplicates = F)

## Plotting
plot(tsne$Y, t='n', main="tSNE")
text(tsne$Y, labels = HomeData.new$BAD, col = fit$cluster)


table(fit$cluster, groups)


# Jaccard similarity index
library(clusteval)
cluster_similarity(groups, fit$cluster, similarity="jaccard", method="independence")


# Rand similarity index
cluster_similarity(groups, fit$cluster, similarity = "rand", method = "independence")