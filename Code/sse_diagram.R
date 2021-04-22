library(LICORS)
library(cluster)


# SSE plot for choosing the value of k 
# that gives the appropriate number of clusters for kmeans in small codes.

SSE <- (nrow(small_sel_metrics[,-1]) - 1) * sum(apply(small_sel_metrics[,-1], 2, var))
for (i in 2:10)
  SSE[i] <- kmeanspp(small_sel_metrics[,-1], k = i)$tot.withinss
plot(1:10, SSE, type="b", xlab="Number of Clusters", ylab="SSE")

# SSE plot for choosing the value of k 
# that gives the appropriate number of clusters for kmeans in big codes.

SSE <- (nrow(high_sel_metrics[,-1]) - 1) * sum(apply(high_sel_metrics[,-1], 2, var))
for (i in 2:10)
  SSE[i] <- kmeanspp(high_sel_metrics[,-1], k = i)$tot.withinss
plot(1:10, SSE, type="b", xlab="Number of Clusters", ylab="SSE")