#MNIST Case Study#
# -----------------------------------------------------------
# Packages and Data
# -----------------------------------------------------------
library(dslabs)
library(caret)
library(matrixStats)
library(ggplot2)
library(randomForest)
library(class)

# Load MNIST
mnist <- read_mnist()

# -----------------------------------------------------------
# Data Sampling
# -----------------------------------------------------------
set.seed(1990)

# Sample 10,000 training and 1,000 test samples
index <- sample(nrow(mnist$train$images), 10000)
x <- mnist$train$images[index, ]
y <- factor(mnist$train$labels[index])

index <- sample(nrow(mnist$test$images), 1000)
x_test <- mnist$test$images[index, ]
y_test <- factor(mnist$test$labels[index])

# -----------------------------------------------------------
# Preprocessing: Removing columns with low variance
# -----------------------------------------------------------
sds <- colSds(x)
nzv <- nearZeroVar(x) # Identify columns with variance near zero
col_index <- setdiff(1:ncol(x), nzv) # Keep only relevant columns

# Adjust column names (required for `caret`)
colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(x)

# -----------------------------------------------------------
# KNN model with cross-validation
# -----------------------------------------------------------
set.seed(1991)
control <- trainControl(method = "cv", number = 10)

train_knn <- train(
  x[, col_index], y,
  method = "knn",
  tuneGrid = data.frame(k = c(3, 5, 7)),
  trControl = control
)

train_knn$bestTune # Shows the best k found

# Train the final model with the best k
fit_knn <- knn3(x[, col_index], y, k = train_knn$bestTune$k)

# -----------------------------------------------------------
# KNN prediction (corrected)
# -----------------------------------------------------------
# Obtain the probabilities
p_knn <- predict(fit_knn, x_test[, col_index], type = "prob")

# Converting to class (class with the highest probability)
y_hat_knn <- factor(apply(p_knn, 1, which.max) - 1, levels = levels(y_test))

# Assessment
cm_knn <- confusionMatrix(y_hat_knn, y_test)
acc_knn <- cm_knn$overall["Accuracy"]

# ---------------------------------------------------------------
# Random Forest with manual hyperparameter search
# ---------------------------------------------------------------
set.seed(123)

ntree_list <- c(50, 75, 100)
maxnodes_list <- c(5, 10, 15)
mtry_list <- c(2, 5, 10)

results <- data.frame( 
  ntree = integer(), 
  maxnodes = integer(), 
  mtry = integer(), 
  Accuracy = numeric()
)

for (ntree in ntree_list) { 
  for (maxnodes in maxnodes_list) { 
    for (mtry_val in mtry_list) { 
      rf_model <- randomForest( 
        x = x[, col_index], 
        y = y, 
        ntree = ntree, 
        maxnodes = maxnodes, 
        mtry = mtry_val 
      ) 
      
      pred <- predict(rf_model, x[, col_index]) 
      acc <- mean(pred == y) 
      
      results <- rbind(results, data.frame( 
        ntree = ntree, 
        maxnodes = maxnodes, 
        mtry = mtry_val, 
        Accuracy = acc 
      )) 
    } 
  }
}

# Best combination
best <- results[which.max(results$Accuracy), ]
print(best)

# Train final model with better parameters
final_rf <- randomForest( 
  x = x[, col_index], 
  y = y,
  ntree = best$ntree,
  maxnodes = best$maxnodes,
  mtry = best$mtry
)

# Prediction in test
y_hat_rf <- predict(final_rf, x_test[, col_index])
acc_rf <- confusionMatrix(y_hat_rf, y_test)$overall["Accuracy"]

# -----------------------------------------------------------
# Ensemble: Average of RF + KNN probabilities
# -----------------------------------------------------------
p_rf <- predict(final_rf, x_test[, col_index], type = "prob")
p_rf <- p_rf / rowSums(p_rf) # Normalization

# p_knn has already been calculated previously

# Ensemble: Average of probabilities
p_ensemble <- (p_rf + p_knn) / 2

# Class with highest probability
y_pred <- factor(apply(p_ensemble, 1, which.max) - 1, levels = levels(y_test))

# Ensemble evaluation
acc_ensemble <- confusionMatrix(y_pred, y_test)$overall["Accuracy"]

# ---------------------------------------------------------------
# Final comparison table
# ---------------------------------------------------------------
results_table <- data.frame( 
  Model = c("Random Forest", "KNN", "Ensemble (RF + KNN)"), 
  Accuracy = round(c(acc_rf, acc_knn, acc_ensemble), 4)
)

print(results_table)