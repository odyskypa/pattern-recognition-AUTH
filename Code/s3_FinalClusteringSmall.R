library(cluster)

# Clustering small data set.
# We use initial centers in kmeans, which we have obtained from the k-means++ algorithm.

small_initial_centers = matrix(c(0.004268718, 0.659745803, 0.403402690, 0.01966705, 0.01561266, 0.01771365), 3, 2)
small_kmodel = kmeans(small_sel_metrics[,-1], centers = small_initial_centers)
small_sel_kclusters = small_kmodel$cluster

# Checking the mean of all metrics in order to decide 
# which ground of readability to assign to each cluster.

# Inserting clusters to small data set.

small_sel_metrics = cbind(small_sel_metrics,small_sel_kclusters)

# With the means below , we verify if the clustering was right
# based on the theory assumptions.

mean_small_VPTLOC_cluster1 = mean(small_sel_metrics[small_sel_metrics$small_sel_kclusters==1,"small_viol_per_TLOC"])
mean_small_VPTLOC_cluster2 = mean(small_sel_metrics[small_sel_metrics$small_sel_kclusters==2,"small_viol_per_TLOC"])
mean_small_VPTLOC_cluster3 = mean(small_sel_metrics[small_sel_metrics$small_sel_kclusters==3,"small_viol_per_TLOC"])

mean_small_TCD_cluster1 = mean(small_sel_metrics[small_sel_metrics$small_sel_kclusters==1,"TCD"])
mean_small_TCD_cluster2 = mean(small_sel_metrics[small_sel_metrics$small_sel_kclusters==2,"TCD"])
mean_small_TCD_cluster3 = mean(small_sel_metrics[small_sel_metrics$small_sel_kclusters==3,"TCD"])

# Checking the number of codes that belong to each cluster.

sum_small_cluster1 = sum(small_sel_kclusters == 1)

sum_small_cluster2 = sum(small_sel_kclusters == 2)

sum_small_cluster3 = sum(small_sel_kclusters == 3)

# Calculating metric VPTLOC for Violations per Total Line Of Code before normalization.

temp = small_sel_metrics
temp$small_viol_per_TLOC = (max(small_viol_per_TLOC) - min(small_viol_per_TLOC)) * small_sel_metrics$small_viol_per_TLOC + min(small_viol_per_TLOC)
mean_small_VPTLOC1 = mean(temp[temp$small_sel_kclusters==1,3])
mean_small_VPTLOC2 = mean(temp[temp$small_sel_kclusters==2,3])
mean_small_VPTLOC3 = mean(temp[temp$small_sel_kclusters==3,3])

# Assigning the class attribute to each code based on the value of metric "VPTLOC".

mean_small_VPTLOC = as.data.frame(cbind(mean_small_VPTLOC1,mean_small_VPTLOC2,mean_small_VPTLOC3))
mean_small_VPTLOC = sort(mean_small_VPTLOC)

names(mean_small_VPTLOC)[names(mean_small_VPTLOC) == "mean_small_VPTLOC1"] <- 1
names(mean_small_VPTLOC)[names(mean_small_VPTLOC) == "mean_small_VPTLOC2"] <- 2
names(mean_small_VPTLOC)[names(mean_small_VPTLOC) == "mean_small_VPTLOC3"] <- 3
High_small = colnames(mean_small_VPTLOC[1])
High_small = strtoi(High_small)
Medium_small = colnames(mean_small_VPTLOC[2])
Medium_small = strtoi(Medium_small)
Low_small = colnames(mean_small_VPTLOC[3])
Low_small = strtoi(Low_small)

# Assigning Readability values based on clustering.

small_sel_metrics$small_sel_kclusters[small_sel_metrics$small_sel_kclusters == Low_small] <- "Low"
small_sel_metrics$small_sel_kclusters[small_sel_metrics$small_sel_kclusters == Medium_small] <- "Medium"
small_sel_metrics$small_sel_kclusters[small_sel_metrics$small_sel_kclusters == High_small] <- "High"

names(small_sel_metrics)[names(small_sel_metrics) == "small_sel_kclusters"] <- "Readability"

Readability_small = small_sel_metrics$Readability