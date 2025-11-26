library(factoextra)
library(purrr)

rfm <- read.csv("rfm.csv")

rownames(rfm) <- rfm[, 1]
rfm <- rfm[, -1]

hist(rfm$Recency)
hist(rfm$Frequency)
hist(rfm$Monetary)

rfm <- rfm[rfm$Recency < 2, ]
rfm <- rfm[rfm$Frequency < 2, ]
rfm <- rfm[rfm$Monetary < 2, ]

hist(rfm$Recency)
hist(rfm$Frequency)
hist(rfm$Monetary)

set.seed(123)
kmeans_result <- kmeans(rfm, centers = 3, nstart = 25)

fviz_cluster(kmeans_result, data = rfm)

wss <- function(k) {
  kmeans(rfm, k, nstart = 10)$tot.withinss
}

k.values <- 1:10
wss_values <- map_dbl(k.values, wss)

plot(
  k.values, wss_values,
  type = "b", pch = 19, frame = FALSE,
  xlab = "Number of clusters K",
  ylab = "Total wss",
  xaxt = "n"
)
axis(1, at = 1:10, labels = TRUE)

rfm$cluster <- kmeans_result$cluster

boxplot(
  rfm$Recency ~ rfm$cluster,
  main = "Recency", xlab = "Cluster", ylab = "Recency"
)
boxplot(
  rfm$Frequency ~ rfm$cluster,
  main = "Frequency", xlab = "Cluster", ylab = "Frequency"
)
boxplot(
  rfm$Monetary ~ rfm$cluster,
  main = "Monetary", xlab = "Cluster", ylab = "Monetary"
)

d <- dist(rfm[, 1:3], method = "euclidean")
hc <- hclust(d, method = "ward.D2")
plot(hc)

cutree(hc, k = 3)

rfm$cluster_h <- cutree(hc, k = 3)

boxplot(
  rfm$Recency ~ rfm$cluster_h,
  main = "Recency", xlab = "Cluster", ylab = "Recency"
)
boxplot(
  rfm$Frequency ~ rfm$cluster_h,
  main = "Frequency", xlab = "Cluster", ylab = "Frequency"
)
boxplot(
  rfm$Monetary ~ rfm$cluster_h,
  main = "Monetary", xlab = "Cluster", ylab = "Monetary"
)

mat <- table(rfm$cluster, rfm$cluster_h)
1 - sum(diag(mat)) / sum(mat)
