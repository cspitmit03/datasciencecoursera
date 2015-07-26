#Exercise on Segmenting Data

seg.raw <- read.csv("http://goo.gl/qw303p")
seg.df  <- seg.raw[ , -7]     # remove the known segment assignments
seg.summ <- function(data, groups) {
        aggregate(data, list(groups), function(x) mean(as.numeric(x)))}
seg.summ(seg.df, seg.raw$Segment)
d <- dist(seg.df[, c("age", "income", "kids")])
as.matrix(d)[1:5, 1:5]


#For seg.df we cannot assume that factor variables are irrelevant to our cluster definitions; 
#it is better to use all the data. The daisy() function in the cluster package [108] works 
#with mixed data types by rescaling the values, so we use that instead of Euclidean distance:

library(cluster)                  # daisy works with mixed data types
seg.dist <- daisy(seg.df)

as.matrix(seg.dist)[1:5, 1:5]

seg.hc <- hclust(seg.dist, method="complete")
plot(seg.hc)
plot(cut(as.dendrogram(seg.hc), h=0.5)$lower[[1]])
seg.df[c(101, 107), ]  # similar
seg.df[c(278, 294), ]  # similar
seg.df[c(173, 141), ]  # less similar

#distance between dendogram and the original euclidian distance
cor(cophenetic(seg.hc), seg.dist)

plot(seg.hc)
rect.hclust(seg.hc, k=5, border="red")

seg.hc.segment <- cutree(seg.hc, k=4)     # membership vector for 4 groups
table(seg.hc.segment)
seg.summ(seg.df, seg.hc.segment)

plot(jitter(as.numeric(seg.df$gender)) ~ jitter(as.numeric(seg.df$subscribe)), 
     col=seg.hc.segment, yaxt="n", xaxt = "n", ylab = "", xlab = "")
axis(1, at=c(1, 2), labels=c("Subscribe: No", "Subscribe: Yes"))
axis(2, at=c(1, 2), labels=levels(seg.df$gender))

#K- means clustering method
seg.df.num <- seg.df
seg.df.num$gender <- ifelse(seg.df$gender=="Male", 0, 1)
seg.df.num$ownHome <- ifelse(seg.df$ownHome=="ownNo", 0, 1)
seg.df.num$subscribe <- ifelse(seg.df$subscribe=="subNo", 0, 1)

#run k means
set.seed(967434)
seg.k <- kmeans(seg.df.num, centers = 4)

seg.summ(seg.df, seg.k$cluster)

boxplot(seg.df.num$income ~ seg.k$cluster, ylab="Income", xlab="Cluster")
boxplot(seg.df.num$age ~ seg.k$cluster, ylab="age", xlab="Cluster")

library(cluster)
clusplot(seg.df, seg.k$cluster, color=TRUE, shade=TRUE, labels=4, 
         lines=0, main="K-means cluster plot")

#run mclust
library(mclust)
seg.mc <- Mclust(seg.df.num)
summary(seg.mc)