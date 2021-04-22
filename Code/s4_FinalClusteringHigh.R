library(cluster)
library(LICORS)

# Clustering high dataset.

high_kmodel = kmeanspp(high_sel_metrics[,-1], k=3)
high_sel_kclusters = high_kmodel$cluster

# Checking the mean of all metrics in order to decide 
# which ground of readability to assign to each cluster.

# Inserting clusters to high dataset.

high_sel_metrics = cbind(high_sel_metrics,high_sel_kclusters)

# With the means below , we verify if the clustering was right
# based on the theory assumptions we made for big codes.

mean_VPTLOC_cluster1 = mean(high_sel_metrics[high_sel_metrics$high_sel_kclusters==1,"high_viol_per_TLOC"])
mean_VPTLOC_cluster2 = mean(high_sel_metrics[high_sel_metrics$high_sel_kclusters==2,"high_viol_per_TLOC"])
mean_VPTLOC_cluster3 = mean(high_sel_metrics[high_sel_metrics$high_sel_kclusters==3,"high_viol_per_TLOC"])

mean_McCC_cluster1 = mean(high_sel_metrics[high_sel_metrics$high_sel_kclusters==1,"McCC"])
mean_McCC_cluster2 = mean(high_sel_metrics[high_sel_metrics$high_sel_kclusters==2,"McCC"])
mean_McCC_cluster3 = mean(high_sel_metrics[high_sel_metrics$high_sel_kclusters==3,"McCC"])

mean_NL_cluster1 = mean(high_sel_metrics[high_sel_metrics$high_sel_kclusters==1,"NL"])
mean_NL_cluster2 = mean(high_sel_metrics[high_sel_metrics$high_sel_kclusters==2,"NL"])
mean_NL_cluster3 = mean(high_sel_metrics[high_sel_metrics$high_sel_kclusters==3,"NL"])

mean_HTRP_cluster1 = mean(high_sel_metrics[high_sel_metrics$high_sel_kclusters==1,"HTRP"])
mean_HTRP_cluster2 = mean(high_sel_metrics[high_sel_metrics$high_sel_kclusters==2,"HTRP"])
mean_HTRP_cluster3 = mean(high_sel_metrics[high_sel_metrics$high_sel_kclusters==3,"HTRP"])

mean_TCD_cluster1 = mean(high_sel_metrics[high_sel_metrics$high_sel_kclusters==1,"TCD"])
mean_TCD_cluster2 = mean(high_sel_metrics[high_sel_metrics$high_sel_kclusters==2,"TCD"])
mean_TCD_cluster3 = mean(high_sel_metrics[high_sel_metrics$high_sel_kclusters==3,"TCD"])

mean_LLOC_cluster1 = mean(high_sel_metrics[high_sel_metrics$high_sel_kclusters==1,"LLOC"])
mean_LLOC_cluster2 = mean(high_sel_metrics[high_sel_metrics$high_sel_kclusters==2,"LLOC"])
mean_LLOC_cluster3 = mean(high_sel_metrics[high_sel_metrics$high_sel_kclusters==3,"LLOC"])

mean_CLLC_cluster1 = mean(high_sel_metrics[high_sel_metrics$high_sel_kclusters==1,"CLLC"])
mean_CLLC_cluster2 = mean(high_sel_metrics[high_sel_metrics$high_sel_kclusters==2,"CLLC"])
mean_CLLC_cluster3 = mean(high_sel_metrics[high_sel_metrics$high_sel_kclusters==3,"CLLC"])

mean_NII_cluster1 = mean(high_sel_metrics[high_sel_metrics$high_sel_kclusters==1,"NII"])
mean_NII_cluster2 = mean(high_sel_metrics[high_sel_metrics$high_sel_kclusters==2,"NII"])
mean_NII_cluster3 = mean(high_sel_metrics[high_sel_metrics$high_sel_kclusters==3,"NII"])

# Checking the number of codes that belong to each cluster.

sum_cluster1 = sum(high_sel_kclusters==1)

sum_cluster2 = sum(high_sel_kclusters==2)

sum_cluster3 = sum(high_sel_kclusters==3)

# Calculating metric VPTLOC for Violations per Total Line Of Code before normalization.

temp = high_sel_metrics
temp$high_viol_per_TLOC = (max(high_viol_per_TLOC) - min(high_viol_per_TLOC)) * high_sel_metrics$high_viol_per_TLOC + min(high_viol_per_TLOC)
mean_VPTLOC1 = mean(temp[temp$high_sel_kclusters==1,9])
mean_VPTLOC2 = mean(temp[temp$high_sel_kclusters==2,9])
mean_VPTLOC3 = mean(temp[temp$high_sel_kclusters==3,9])

# Assigning the class attribute to each code based on the value of metric "VPTLOC".

mean_VPTLOC = as.data.frame(cbind(mean_VPTLOC1,mean_VPTLOC2,mean_VPTLOC3))
mean_VPTLOC = sort(mean_VPTLOC)

names(mean_VPTLOC)[names(mean_VPTLOC) == "mean_VPTLOC1"] <- 1
names(mean_VPTLOC)[names(mean_VPTLOC) == "mean_VPTLOC2"] <- 2
names(mean_VPTLOC)[names(mean_VPTLOC) == "mean_VPTLOC3"] <- 3
High = colnames(mean_VPTLOC[1])
High = strtoi(High)
Medium = colnames(mean_VPTLOC[2])
Medium = strtoi(Medium)
Low = colnames(mean_VPTLOC[3])
Low = strtoi(Low)

# Assigning Readability values based on clustering.

high_sel_metrics$high_sel_kclusters[high_sel_metrics$high_sel_kclusters == Low] <- "Low"
high_sel_metrics$high_sel_kclusters[high_sel_metrics$high_sel_kclusters == Medium] <- "Medium"
high_sel_metrics$high_sel_kclusters[high_sel_metrics$high_sel_kclusters == High] <- "High"

names(high_sel_metrics)[names(high_sel_metrics) == "high_sel_kclusters"] <- "Readability"

Readability = high_sel_metrics$Readability
