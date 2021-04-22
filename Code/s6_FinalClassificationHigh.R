library(rpart)
library(rpart.plot)

# We run the first two models: decision trees, naive Bayes.
# In the other models, we use normalized data.


# Creating non-normalized training and testing set.

high_shuffle = cbind(high_metrics,high_viol_per_TLOC,Readability)
high_shuffle = high_shuffle[sample(nrow(high_shuffle),398821,replace=FALSE),c("ID","LLOC","TCD","CLLC","McCC","NL","HTRP","NII","high_viol_per_TLOC","Readability")]
high_trainingset = high_shuffle[1:300000,]
high_testingset = high_shuffle[300001:398821,]

# Normalizing training and testing set.

temp = high_trainingset
for (i in colnames(temp[,2:9])) {
  
  
  temp[,i]=(temp[,i]-min(temp[,i]))/(max(temp[,i]) - min(temp[,i]))
  
}

high_norm_trainingset = temp

temp = high_testingset
for (i in colnames(temp[,2:9])) {
  
  
  temp[,i]=(temp[,i]-min(temp[,i]))/(max(temp[,i]) - min(temp[,i]))
  
}

high_norm_testingset = temp


# Classification tree implementation.

high_tree_model <- rpart(Readability ~ ., method = "class", data = high_trainingset[,-1],minsplit = 2)

high_tree_pred = predict(high_tree_model, high_testingset[,2:9], type="class")

high_tree_cm = as.matrix(table(Actual = high_testingset[,10], Predicted = high_tree_pred))


high_tree_accuracy = sum(diag(high_tree_cm)) / sum(high_tree_cm)
high_tree_precision = diag(high_tree_cm) / colSums(high_tree_cm)
high_tree_recall = diag(high_tree_cm) / rowSums(high_tree_cm)
high_tree_fmeasure = 2 * high_tree_precision * high_tree_recall / (high_tree_precision + high_tree_recall)

# Naive-Bayes Classification Implementation.

library(class)
library(e1071)
library(MLmetrics)

high_naive_model <- naiveBayes(Readability ~ ., data = high_trainingset[, -1], laplace = 1)
high_naive_pred = predict(high_naive_model, high_testingset[, 2:9])

# Confusion Matrix

naive_cm = as.matrix(table(Actual = high_testingset[, 10], Predicted = high_naive_pred))

# Naive-Bayes Model Measures.

naive_accuracy = sum(diag(naive_cm)) / sum(naive_cm)
naive_precision = diag(naive_cm) / colSums(naive_cm)
naive_recall = diag(naive_cm) / rowSums(naive_cm)
naive_fmeasure = 2 * naive_precision * naive_recall / (naive_precision + naive_recall)

# Implementing cross validation

k = 10
high_cross_metrics = rbind(high_trainingset, high_testingset)
dsize = nrow(high_cross_metrics)
folds = split(sample(1:dsize), ceiling(seq(dsize) * k / dsize))
accuracies <- c()
predictions <- data.frame()
testsets <- data.frame()

for (i in 1:k) {
  
  trainingset <- high_cross_metrics[unlist(folds[-i]),]
  validationset <- high_cross_metrics[unlist(folds[i]),]
  high_naive_model <- naiveBayes(Readability ~ ., data = trainingset[, -1], laplace = 1)
  high_naive_pred = predict(high_naive_model, validationset[, 2:9])
  predictions <- rbind(predictions, as.data.frame(high_naive_pred))
  testsets <- rbind(testsets, as.data.frame(validationset[,10]))
  accuracies = c(accuracies, Accuracy(predictions, testsets))
  
}

high_avg_naive_accuracies = mean(accuracies)
print(high_avg_naive_accuracies)


# Neural Network Classification Implementation.

library(neuralnet)

# Constructing the training and the testing_set

# Changing training set because NN works better with numerical values rather than categorical
# for example a code with low readability will be recognized by having the values 1 , 0 , 0
# in the columns low readability , medium readability , high readability respectively .
# So on codes with medium readability will have the values 0 , 1 , 0 
# and with high readability 0 , 0 , 1.

low_readability = c()
low_readability[high_norm_trainingset$Readability == "Low"] <- 1
low_readability[high_norm_trainingset$Readability != "Low"] <- 0
medium_readability = c()
medium_readability[high_norm_trainingset$Readability == "Medium"] <- 1
medium_readability[high_norm_trainingset$Readability != "Medium"] <- 0
high_readability = c()
high_readability[high_norm_trainingset$Readability == "High"] <- 1
high_readability[high_norm_trainingset$Readability != "High"] <- 0
high_norm_trainingset_neural = cbind(high_norm_trainingset, low_readability, medium_readability, high_readability)
high_norm_trainingset_neural = high_norm_trainingset_neural[, -10]

# Implementation of Neural Network.

high_neural_model = neuralnet(low_readability + medium_readability + high_readability ~ LLOC + TCD + CLLC + McCC + NL + HTRP + NII + high_viol_per_TLOC, high_norm_trainingset_neural[, c(2:12)], hidden = 0, threshold = 0.0001)
pred = compute(high_neural_model, high_norm_testingset[, 2:9])$net.result
high_neural_pred = max.col(pred, 'first')
high_neural_pred = as.data.frame(high_neural_pred)
names(high_neural_pred)[names(high_neural_pred) == "high_neural_pred"] <- "Readability"
high_neural_pred$Readability[high_neural_pred$Readability == 1] <- "Low"
high_neural_pred$Readability[high_neural_pred$Readability == 2] <- "Medium"
high_neural_pred$Readability[high_neural_pred$Readability == 3] <- "High"

high_neural_pred = as.matrix(high_neural_pred)

# Confusion Matrix

neural_cm = as.matrix(table(Actual = high_norm_testingset[,10], Predicted = high_neural_pred))

# Neural Model Measures.

neural_accuracy = sum(diag(neural_cm)) / sum(neural_cm)
neural_precision = diag(neural_cm) / colSums(neural_cm)
neural_recall = diag(neural_cm) / rowSums(neural_cm)
neural_fmeasure = 2 * neural_precision * neural_recall / (neural_precision + neural_recall)

# Implementing cross validation

k = 10
high_cross_metrics_norm = rbind(high_norm_trainingset, high_norm_testingset)
dsize = nrow(high_cross_metrics_norm)
folds = split(sample(1:dsize), ceiling(seq(dsize) * k / dsize))
accuracies <- c()
predictions <- data.frame()
testsets <- data.frame()

for (i in 1:k) {
  
  trainingset <- high_cross_metrics_norm[unlist(folds[-i]),]
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
  trainingset = trainingset[, -10]
  validationset <- high_cross_metrics_norm[unlist(folds[i]),]
  
  high_neural_model = neuralnet(low_readability + medium_readability + high_readability ~ ., trainingset[, c(2:12)], hidden = 0, threshold = 0.01)
  pred = compute(high_neural_model, validationset[, 2:9])$net.result
  high_neural_pred = max.col(pred, 'first')
  high_neural_pred = as.data.frame(high_neural_pred)
  names(high_neural_pred)[names(high_neural_pred) == "high_neural_pred"] <- "readability"
  high_neural_pred$readability[high_neural_pred$readability == 1] <- "Low"
  high_neural_pred$readability[high_neural_pred$readability == 2] <- "Medium"
  high_neural_pred$readability[high_neural_pred$readability == 3] <- "High"
  high_neural_pred = as.matrix(high_neural_pred)
  
  predictions <- rbind(predictions, as.data.frame(high_neural_pred))
  testsets <- rbind(testsets, as.data.frame(validationset[,10]))
  accuracies = c(accuracies, Accuracy(predictions, testsets))
  
}

high_avg_neural_accuracies = mean(accuracies)
print(high_avg_neural_accuracies)

# Implementation of KNN 

library(class)

high_knn_pred = knn(high_norm_trainingset[,2:9] , high_norm_testingset[,2:9] , high_norm_trainingset[,10] , k = 7)
knn_cm = as.matrix(table(Actual = high_norm_testingset[,10], Predicted = high_knn_pred))

# KNN Model Measures.

knn_accuracy = sum(diag(knn_cm)) / sum(knn_cm)
knn_precision = diag(knn_cm) / colSums(knn_cm)
knn_recall = diag(knn_cm) / rowSums(knn_cm)
knn_fmeasure = 2 * neural_precision * neural_recall / (neural_precision + neural_recall)

# Implementing cross validation

k = 10
high_cross_metrics_norm = rbind(high_norm_trainingset, high_norm_testingset)
dsize = nrow(high_cross_metrics_norm)
folds = split(sample(1:dsize), ceiling(seq(dsize) * k / dsize))
accuracies <- c()
predictions <- data.frame()
testsets <- data.frame()

for (i in 1:k) {
  
  trainingset <- high_cross_metrics_norm[unlist(folds[-i]),]
  validationset <- high_cross_metrics_norm[unlist(folds[i]),]
  high_knn_pred = knn(trainingset[,2:9] , validationset[,2:9] , trainingset[,10] , k = 3)
  predictions <- rbind(predictions, as.data.frame(high_knn_pred))
  testsets <- rbind(testsets, as.data.frame(validationset[,10]))
  accuracies = c(accuracies, Accuracy(predictions, testsets))
  
}

high_avg_knn_accuracies = mean(accuracies)
print(high_avg_knn_accuracies)


