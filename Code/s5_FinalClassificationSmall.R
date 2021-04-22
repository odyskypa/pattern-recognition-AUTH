library(rpart)
library(rpart.plot)

# We run the first two models: decision trees, naive Bayes.
# In the other models, we use normalized data.


# Creating non-normalized training and testing set.

small_shuffle = cbind(small_metrics,small_viol_per_TLOC,Readability_small)
small_shuffle = small_shuffle[sample(nrow(small_shuffle),543272,replace=FALSE),c("ID","TCD","small_viol_per_TLOC","Readability_small")]
names(small_shuffle)[names(small_shuffle) == "Readability_small"] <- "Readability"
small_trainingset = small_shuffle[1:400000,]
small_testingset = small_shuffle[400001:543272,]

# Normalizing training and testing set.

temp = small_trainingset
for (i in colnames(temp[,2:3])) {
  
  
  temp[,i]=(temp[,i]-min(temp[,i]))/(max(temp[,i]) - min(temp[,i]))
  
}

small_norm_trainingset = temp

temp = small_testingset
for (i in colnames(temp[,2:3])) {
  
  
  temp[,i]=(temp[,i]-min(temp[,i]))/(max(temp[,i]) - min(temp[,i]))
  
}

small_norm_testingset = temp


# Classification tree implementation.

small_tree_model <- rpart(Readability ~ ., method = "class", data = small_trainingset[,-1],minsplit = 2)

small_tree_pred = predict(small_tree_model, small_testingset[,2:3], type="class")

small_tree_cm = as.matrix(table(Actual = small_testingset[,4], Predicted = small_tree_pred))


small_tree_accuracy = sum(diag(small_tree_cm)) / sum(small_tree_cm)
small_tree_precision = diag(small_tree_cm) / colSums(small_tree_cm)
small_tree_recall = diag(small_tree_cm) / rowSums(small_tree_cm)
small_tree_fmeasure = 2 * small_tree_precision * small_tree_recall / (small_tree_precision + small_tree_recall)

# Naive-Bayes Classification Implementation.

library(class)
library(e1071)
library(MLmetrics)

small_naive_model <- naiveBayes(Readability ~ ., data = small_trainingset[, -1], laplace = 1)
small_naive_pred = predict(small_naive_model, small_testingset[, 2:3])

# Confusion Matrix

small_naive_cm = as.matrix(table(Actual = small_testingset[, 4], Predicted = small_naive_pred))

# Naive-Bayes Model Measures.

small_naive_accuracy = sum(diag(small_naive_cm)) / sum(small_naive_cm)
small_naive_precision = diag(small_naive_cm) / colSums(small_naive_cm)
small_naive_recall = diag(small_naive_cm) / rowSums(small_naive_cm)
small_naive_fmeasure = 2 * small_naive_precision * small_naive_recall / (small_naive_precision + small_naive_recall)

# Implementing cross validation

k = 10
small_cross_metrics = rbind(small_trainingset, small_testingset)
dsize = nrow(small_cross_metrics)
folds = split(sample(1:dsize), ceiling(seq(dsize) * k / dsize))
accuracies <- c()
predictions <- data.frame()
testsets <- data.frame()

for (i in 1:k) {
  
  trainingset <- small_cross_metrics[unlist(folds[-i]),]
  validationset <- small_cross_metrics[unlist(folds[i]),]
  small_naive_model <- naiveBayes(Readability ~ ., data = trainingset[, -1], laplace = 1)
  small_naive_pred = predict(small_naive_model, validationset[, 2:3])
  predictions <- rbind(predictions, as.data.frame(small_naive_pred))
  testsets <- rbind(testsets, as.data.frame(validationset[,4]))
  accuracies = c(accuracies, Accuracy(predictions, testsets))
  
}

small_avg_naive_accuracies = mean(accuracies)
print(small_avg_naive_accuracies)


# Neural Network Classification Implementation.

library(neuralnet)

# Constructing the training and the testing_set

# Changing training set because NN works better with numerical values rather than categorical
# for example a code with low readability will be recognized by having the values 1 , 0 , 0
# in the columns low readability , medium readability , high readability respectively .
# So on codes with medium readability will have the values 0 , 1 , 0 
# and with high readability 0 , 0 , 1.

low_readability = c()
low_readability[small_norm_trainingset$Readability == "Low"] <- 1
low_readability[small_norm_trainingset$Readability != "Low"] <- 0
medium_readability = c()
medium_readability[small_norm_trainingset$Readability == "Medium"] <- 1
medium_readability[small_norm_trainingset$Readability != "Medium"] <- 0
high_readability = c()
high_readability[small_norm_trainingset$Readability == "High"] <- 1
high_readability[small_norm_trainingset$Readability != "High"] <- 0
small_norm_trainingset_neural = cbind(small_norm_trainingset, low_readability, medium_readability, high_readability)
small_norm_trainingset_neural = small_norm_trainingset_neural[, -4]

# Implementation of Neural Network.

small_neural_model = neuralnet(low_readability + medium_readability + high_readability ~ ., small_norm_trainingset_neural[, c(2:6)], hidden = 0, threshold = 0.0001)
pred = compute(small_neural_model, small_norm_testingset[, 2:3])$net.result
small_neural_pred = max.col(pred, 'first')
small_neural_pred = as.data.frame(small_neural_pred)
names(small_neural_pred)[names(small_neural_pred) == "small_neural_pred"] <- "Readability"
small_neural_pred$Readability[small_neural_pred$Readability == 1] <- "Low"
small_neural_pred$Readability[small_neural_pred$Readability == 2] <- "Medium"
small_neural_pred$Readability[small_neural_pred$Readability == 3] <- "High"

small_neural_pred = as.matrix(small_neural_pred)

# Confusion Matrix

small_neural_cm = as.matrix(table(Actual = small_norm_testingset[,4], Predicted = small_neural_pred))

# Neural Model Measures.

small_neural_accuracy = sum(diag(small_neural_cm)) / sum(small_neural_cm)
small_neural_precision = diag(small_neural_cm) / colSums(small_neural_cm)
# small_neural_recall = diag(small_neural_cm) / rowSums(small_neural_cm)
# small_neural_fmeasure = 2 * small_neural_precision * small_neural_recall / (small_neural_precision + small_neural_recall)

# Implementing cross validation

k = 10
small_cross_metrics_norm = rbind(small_norm_trainingset, small_norm_testingset)
dsize = nrow(small_cross_metrics_norm)
folds = split(sample(1:dsize), ceiling(seq(dsize) * k / dsize))
accuracies <- c()
predictions <- data.frame()
testsets <- data.frame()

for (i in 1:k) {
  
  trainingset <- small_cross_metrics_norm[unlist(folds[-i]),]
  low_readability = c()
  low_readability[trainingset$Readability == "Low"] <- 1
  low_readability[trainingset$Readability != "Low"] <- 0
  medium_readability = c()
  medium_readability[trainingset$Readability == "Medium"] <- 1
  medium_readability[trainingset$Readability != "Medium"] <- 0
  high_readability = c()
  high_readability[trainingset$Readability == "High"] <- 1
  high_readability[trainingset$Readability != "High"] <- 0
  trainingset = cbind(trainingset, low_readability, medium_readability, high_readability)
  trainingset = trainingset[, -4]
  validationset <- small_cross_metrics_norm[unlist(folds[i]),]
  
  small_neural_model = neuralnet(low_readability + medium_readability + high_readability ~ ., trainingset[, c(2:6)], hidden = 0, threshold = 0.01)
  pred = compute(small_neural_model, validationset[, 2:3])$net.result
  small_neural_pred = max.col(pred, 'first')
  small_neural_pred = as.data.frame(small_neural_pred)
  names(small_neural_pred)[names(small_neural_pred) == "small_neural_pred"] <- "readability"
  small_neural_pred$readability[small_neural_pred$readability == 1] <- "Low"
  small_neural_pred$readability[small_neural_pred$readability == 2] <- "Medium"
  small_neural_pred$readability[small_neural_pred$readability == 3] <- "High"
  small_neural_pred = as.matrix(small_neural_pred)
  
  predictions <- rbind(predictions, as.data.frame(small_neural_pred))
  testsets <- rbind(testsets, as.data.frame(validationset[,4]))
  accuracies = c(accuracies, Accuracy(predictions, testsets))
  
}

small_avg_neural_accuracies = mean(accuracies)
print(small_avg_neural_accuracies)