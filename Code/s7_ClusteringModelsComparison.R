library(cluster)
library(LICORS)

# We are sampling the dataset so we can check silhouette for 
# different ways of clustering because for all the dataset calculation of distance
# is impossible.

small_sel_samp=small_sel_metrics[sample(nrow(small_sel_metrics),25000,replace=FALSE),]
small_sel_samp_kmodel = kmeanspp(small_sel_samp[,-1], k = 3, iter.max = 1000)
small_sel_samp_kmodel_silhouette = silhouette(small_sel_samp_kmodel$cluster, dist(small_sel_samp[,-1]))
plot(small_sel_samp_kmodel_silhouette)

# Complete hierarchical clustering.

small_hc_complete=hclust(dist(small_sel_samp[,-1]),method="complete")
slc = c()
for (i in 2:7){
  small_clusters = cutree(small_hc_complete, k = i)
  slc [i-1] = mean(silhouette(small_clusters, dist(small_sel_samp[,-1]))[, 3])
}
small_clusters = cutree(small_hc_complete,k=3)
small_tmodel_silhouette = silhouette(small_clusters, dist(small_sel_samp[,-1]))
plot(small_tmodel_silhouette)

# We see that complete hierarchical clustering clusters all the data in one group 
# and that is not desirable.

# Single hierarchical clustering.

small_hc_single=hclust(dist(small_sel_samp[,-1]),method="single")

slc = c()
for (i in 2:7){
  small_clusters = cutree(small_hc_single, k = i)
  slc [i-1] = mean(silhouette(small_clusters, dist(small_sel_samp[,-1]))[, 3])
}
small_clusters = cutree(small_hc_single,k=3)
small_tmodel_silhouette = silhouette(small_clusters, dist(small_sel_samp[,-1]))
plot(small_tmodel_silhouette)

# We see that single hierarchical clustering clusters all the data in one group 
# and that is not desirable.

# Average hierarchical clustering.

small_hc_average=hclust(dist(small_sel_samp[,-1]),method="average")

slc = c()
for (i in 2:7){
  small_clusters = cutree(small_hc_average, k = i)
  slc [i-1] = mean(silhouette(small_clusters, dist(small_sel_samp[,-1]))[, 3])
}
small_clusters = cutree(small_hc_average,k=3)
small_tmodel_silhouette = silhouette(small_clusters, dist(small_sel_samp[,-1]))
plot(small_tmodel_silhouette)

# We see that average hierarchical clustering clusters all the data in one group 
# and that is not desirable.

# We are sampling the dataset so we can check silhouette for 
# different ways of clustering because for all the dataset calculation of distance
# is impossible.


high_sel_samp=high_sel_metrics[sample(nrow(high_sel_metrics),25000,replace=FALSE),]
high_sel_samp_kmodel = kmeanspp(high_sel_samp[,-1], k = 3)
high_sel_samp_kmodel_silhouette = silhouette(high_sel_samp_kmodel$cluster, dist(high_sel_samp[,-1]))
plot(high_sel_samp_kmodel_silhouette)

# Complete hierarchical clustering.

high_hc_complete=hclust(dist(high_sel_samp[,-1]),method="complete")
slc = c()
for (i in 2:7){
  high_clusters = cutree(high_hc_complete, k = i)
  slc [i-1] = mean(silhouette(high_clusters, dist(high_sel_samp[,-1]))[, 3])
}
high_clusters = cutree(high_hc_complete,k=3)
high_tmodel_silhouette = silhouette(high_clusters, dist(high_sel_samp[,-1]))
plot(high_tmodel_silhouette)

# We see that in average hierarchical clustering all records belong 
# to one cluster and that's not desirable. This also is shown by the
# silhouette plot.

# Single hierarchical clustering.

high_hc_single=hclust(dist(high_sel_samp[,-1]),method="single")

slc = c()
for (i in 2:7){
  high_clusters = cutree(high_hc_single, k = i)
  slc [i-1] = mean(silhouette(high_clusters, dist(high_sel_samp[,-1]))[, 3])
}
high_clusters = cutree(high_hc_single,k=3)
high_tmodel_silhouette = silhouette(high_clusters, dist(high_sel_samp[,-1]))
plot(high_tmodel_silhouette)

# We see that in single hierarchical clustering all records belong 
# to one cluster and that's not desirable. This also is shown by the
# silhouette plot.

# Average hierarchical clustering.

high_hc_average=hclust(dist(high_sel_samp[,-1]),method="average")

slc = c()
for (i in 2:7){
  high_clusters = cutree(high_hc_average, k = i)
  slc [i-1] = mean(silhouette(high_clusters, dist(high_sel_samp[,-1]))[, 3])
}
high_clusters = cutree(high_hc_average,k=3)
high_tmodel_silhouette = silhouette(high_clusters, dist(high_sel_samp[,-1]))
plot(high_tmodel_silhouette)

# We see that in average hierarchical clustering all records belong 
# to one cluster and that's not desirable. This also is shown by the
# silhouette plot.

# DBSCAN does not work, due to the dataset nature.