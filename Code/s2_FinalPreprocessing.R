library(corrplot)
# Removing NA values from dataframe.

new_total_metrics = total_metrics[rowSums(is.na(total_metrics))==0,]
new_total_violations = total_violations[rowSums(is.na(total_metrics))==0,]

# Finding percentiles, in order to split the data
quantile(new_total_metrics$TLOC, c(0.57))
quantile(new_total_metrics$TLOC, c(0.999))


# In this way, we decide to split the data into two groups
# One with 1-6 TLOC and one with 7-200 TLOC


# Creating a group of codes with TLOC <= 6.

small_metrics <- new_total_metrics[ which(new_total_metrics$TLOC<=6), ]
small_violations <-new_total_violations[ which(new_total_metrics$TLOC<=6), ]

# Creating a group of codes with TLOC >= 7 and TLOC <= 200.

high_metrics <- new_total_metrics[ which(new_total_metrics$TLOC>=7 & new_total_metrics$TLOC<=200), ]
high_violations <-new_total_violations[ which(new_total_metrics$TLOC>=7 & new_total_metrics$TLOC<=200), ]

# Checking the number of features we need to use for the group "small" with PCA.

pca_small_model <- prcomp(small_metrics[,11:47], center = TRUE, scale = TRUE)
small_eigenvalues = pca_small_model$sdev^2
small_eigenvectors = pca_small_model$rotation

# Checking the number of features we need to use for the group "high" with PCA.

pca_high_model <- prcomp(high_metrics[,11:47], center = TRUE, scale = TRUE)
high_eigenvalues = pca_high_model$sdev^2
high_eigenvectors = pca_high_model$rotation

# Finding the principal components for the group "small"
# We select the most significant features from the first eight principal components

PC1_small = sort(abs(pca_small_model$rotation[, 1]))
PC1_small = as.data.frame(PC1_small)

PC2_small = sort(abs(pca_small_model$rotation[, 2]))
PC2_small = as.data.frame(PC2_small)

PC3_small = sort(abs(pca_small_model$rotation[, 3]))
PC3_small = as.data.frame(PC3_small)

PC4_small = sort(abs(pca_small_model$rotation[, 4]))
PC4_small = as.data.frame(PC4_small)

PC5_small = sort(abs(pca_small_model$rotation[, 5]))
PC5_small = as.data.frame(PC5_small)

PC6_small = sort(abs(pca_small_model$rotation[, 6]))
PC6_small = as.data.frame(PC6_small)

PC7_small = sort(abs(pca_small_model$rotation[, 7]))
PC7_small = as.data.frame(PC7_small)

PC8_small = sort(abs(pca_small_model$rotation[, 8]))
PC8_small = as.data.frame(PC8_small)

# Finding the principal components for the group "high"
# We select the most significant features from the first eight principal components

PC1_high = sort(abs(pca_high_model$rotation[, 1]))
PC1_high = as.data.frame(PC1_high)

PC2_high = sort(abs(pca_high_model$rotation[, 2]))
PC2_high = as.data.frame(PC2_high)

PC3_high = sort(abs(pca_high_model$rotation[, 3]))
PC3_high = as.data.frame(PC3_high)

PC4_high = sort(abs(pca_high_model$rotation[, 4]))
PC4_high = as.data.frame(PC4_high)

PC5_high = sort(abs(pca_high_model$rotation[, 5]))
PC5_high = as.data.frame(PC5_high)

PC6_high = sort(abs(pca_high_model$rotation[, 6]))
PC6_high = as.data.frame(PC6_high)

PC7_high = sort(abs(pca_high_model$rotation[, 7]))
PC7_high = as.data.frame(PC7_high)

PC8_high = sort(abs(pca_high_model$rotation[, 8]))
PC8_high = as.data.frame(PC8_high)

# We check for each one principal component which metric has the biggest value in the eigenvectors.

corrplot(cor(high_metrics[,11:47]), method="ellipse")

# Calculating correlation matrix for metrics of small codes
# to see dependencies and make groups with metrics that have a value bigger than 0.85.

temp_cor = cor(small_metrics[,11:47])

for (i in colnames(small_metrics[, 11:47])) {
  
  small_cor_groups = which(temp_cor[, i] > 0.85)
  print(small_cor_groups)
  
}


# Calculating correlation matrix for metrics of big codes 
# to see dependencies and make groups with metrics that have a value bigger than 0.85.

temp_cor = cor(high_metrics[,11:47])

for (i in colnames(high_metrics[, 11:47])) {
  
  high_cor_groups = which(temp_cor[, i] > 0.85)
  print(high_cor_groups)
  
}


# Selected features for small codes based on correlation matrix , pca analysis and theory from papers.

small_sel_metrics = small_metrics[, c("ID", "TCD")]

# Selected features for big codes based on correlation matrix , pca analysis and theory from papers.

high_sel_metrics = high_metrics[, c("ID","LLOC","TCD","CLLC","McCC","NL","HTRP","NII")]

# Normalization of data for small codes

for (i in colnames(small_sel_metrics[,-1])) {
  
  
  small_sel_metrics[,i]=(small_sel_metrics[,i]-min(small_sel_metrics[,i]))/(max(small_sel_metrics[,i]) - min(small_sel_metrics[,i]))
  
}

# Normalization of data for big codes

for (i in colnames(high_sel_metrics[,-1])) {
  
  
  high_sel_metrics[,i]=(high_sel_metrics[,i]-min(high_sel_metrics[,i]))/(max(high_sel_metrics[,i]) - min(high_sel_metrics[,i]))
  
}

# Calculating sum of violations of every code in the high dataset.

high_violations_sum = rowSums(high_violations[1:398821,11:203])

# Calculating sum of violations of every code in the small dataset.

small_violations_sum = rowSums(small_violations[1:543272,11:203])


# Calcuting new metric which is the result of the sum of violations divided by TLOC in the high dataset.

high_viol_per_TLOC = high_violations_sum / high_metrics$TLOC

# Calcuting new metric which is the result of the sum of violations divided by TLOC in the small dataset.

small_viol_per_TLOC = small_violations_sum / small_metrics$TLOC

# We insert this column to the dataframe that contains the metrics we chose
# and we normalize that column for high codes.

high_sel_metrics=cbind(high_sel_metrics,high_viol_per_TLOC)
high_sel_metrics[,9]=(high_sel_metrics[,9] - min(high_sel_metrics[,9]))/(max(high_sel_metrics[,9]) - min(high_sel_metrics[,9]))
                                                                         
# We insert this column to the dataframe that contains the metrics we chose
# and we normalize that column for small codes .

small_sel_metrics=cbind(small_sel_metrics,small_viol_per_TLOC)
small_sel_metrics[,3]=(small_sel_metrics[,3] - min(small_sel_metrics[,3]))/(max(small_sel_metrics[,3]) - min(small_sel_metrics[,3]))
