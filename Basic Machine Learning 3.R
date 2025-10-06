#########################################
##### Breast Cancer Project 
#########################################

options(digits = 3)

# Load required libraries
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
library(ggplot2)
library(reshape2)

# Load BRCA dataset
data(brca)

##### Descriptive Data Exploration #####

# Number of samples and features
num_samples <- dim(brca$x)[1]
num_features <- dim(brca$x)[2]
cat("Number of samples:", num_samples, "\n")
cat("Number of features:", num_features, "\n")

# Distribution of tumor types
tumor_counts <- table(brca$y)
cat("Tumor type counts:\n")
print(tumor_counts)
cat("Proportion malignant:", mean(brca$y == "M"), "\n")
cat("Proportion benign:", mean(brca$y == "B"), "\n")

# Summary statistics for the first 5 features
cat("Summary statistics for first 5 features:\n")
print(summary(brca$x[, 1:5]))

# Visualize distribution of a few features by tumor type
feature_names <- colnames(brca$x)
selected_features <- feature_names[1:4]

# Prepare data frame for plotting
feature_df <- as.data.frame(brca$x[, selected_features])
feature_df$Tumor <- brca$y

# Boxplots of selected features by tumor type
feature_long <- melt(feature_df, id.vars = "Tumor",
                     variable.name = "Feature", value.name = "Expression")

ggplot(feature_long, aes(x = Tumor, y = Expression, fill = Tumor)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  facet_wrap(~ Feature, scales = "free_y") +
  scale_fill_manual(values = c("B" = "steelblue", "M" = "firebrick")) +
  labs(title = "Expression of Selected Features by Tumor Type",
       x = "Tumor Type",
       y = "Expression Level") +
  theme_minimal()

##### Data preprocessing #####

# Center and scale features
x_centered <- sweep(brca$x, 2, colMeans(brca$x))
x_scaled <- sweep(x_centered, 2, colSds(brca$x), FUN = "/")

# PCA analysis
pca <- prcomp(x_scaled)
explained_var <- pca$sdev^2 / sum(pca$sdev^2)
cat("Variance explained by first PC:", explained_var[1], "\n")

cum_var <- cumsum(explained_var)
cat("Number of PCs to explain at least 90% variance:", which(cum_var >= 0.90)[1], "\n")

# Prepare data frame for PCA plot
pca_data <- data.frame(PC1 = pca$x[,1],
                       PC2 = pca$x[,2],
                       Tumor = brca$y)

# PCA plot
ggplot(pca_data, aes(x = PC1, y = PC2, color = Tumor)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(title = "PCA of BRCA Gene Expression Data",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal() +
  scale_color_manual(values = c("B" = "steelblue", "M" = "firebrick"),
                     labels = c("Benign", "Malignant"),
                     name = "Tumor Type")

# Boxplots of first 10 PCs grouped by tumor type
pc_scores <- as.data.frame(pca$x[, 1:10])
pc_scores$Tumor <- brca$y
pc_long <- melt(pc_scores, id.vars = "Tumor",
                variable.name = "PC", value.name = "Score")

ggplot(pc_long, aes(x = PC, y = Score, fill = Tumor)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8) +
  labs(title = "Boxplot of First 10 Principal Components by Tumor Type") +
  theme_minimal() +
  scale_fill_manual(values = c("B" = "steelblue", "M" = "firebrick"),
                    labels = c("Benign", "Malignant")) +
  coord_flip()

# Function to get IQR range (Q1 and Q3)
get_iqr_range <- function(x) {
  quantile(x, probs = c(0.25, 0.75))
}

# Identify PCs with non-overlapping IQRs between tumor types
non_overlapping_pcs <- c()
for (i in 1:10) {
  pc_name <- paste0("PC", i)
  benign_iqr <- get_iqr_range(pc_scores[pc_scores$Tumor == "B", i])
  malignant_iqr <- get_iqr_range(pc_scores[pc_scores$Tumor == "M", i])
  
  if (benign_iqr[2] < malignant_iqr[1] || malignant_iqr[2] < benign_iqr[1]) {
    non_overlapping_pcs <- c(non_overlapping_pcs, pc_name)
  }
}
cat("PCs with non-overlapping IQRs between tumor types:", paste(non_overlapping_pcs, collapse = ", "), "\n")

##### Train/test split #####

set.seed(1)
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- x_scaled[test_index,]
test_y <- brca$y[test_index]
train_x <- x_scaled[-test_index,]
train_y <- brca$y[-test_index]

cat("Proportion benign in training set:", mean(train_y == "B"), "\n")
cat("Proportion benign in test set:", mean(test_y == "B"), "\n")

##### Logistic Regression Model #####

train_df <- data.frame(train_x)
train_df$Tumor <- train_y

model <- glm(Tumor ~ ., data = train_df, family = "binomial")

test_df <- data.frame(test_x)

probs <- predict(model, newdata = test_df, type = "response")

predictions <- ifelse(probs > 0.5, "M", "B")
predictions <- factor(predictions, levels = levels(test_y))

logistic_accuracy <- mean(predictions == test_y)
cat("Logistic Regression accuracy:", logistic_accuracy, "\n")

##### Loess Model #####

set.seed(5)
train_loess <- train(train_x, train_y, method = "gamLoess")
loess_preds <- predict(train_loess, test_x)
loess_accuracy <- mean(loess_preds == test_y)
cat("Loess model accuracy:", loess_accuracy, "\n")

##### k-Nearest Neighbors Model #####

set.seed(7, sample.kind = "Rounding")
tuning <- data.frame(k = seq(3, 21, 2))
train_knn <- train(train_x, train_y,
                   method = "knn",
                   tuneGrid = tuning)
best_k <- train_knn$bestTune$k
cat("Best k for kNN:", best_k, "\n")

knn_preds <- predict(train_knn, test_x)
knn_accuracy <- mean(knn_preds == test_y)
cat("kNN model accuracy:", knn_accuracy, "\n")

##### Random Forest Model #####

set.seed(9, sample.kind = "Rounding")
tuning_rf <- data.frame(mtry = c(3, 5, 7, 9))
train_rf <- train(train_x, train_y,
                  method = "rf",
                  tuneGrid = tuning_rf,
                  importance = TRUE)
best_mtry <- train_rf$bestTune$mtry
cat("Best mtry for Random Forest:", best_mtry, "\n")

rf_preds <- predict(train_rf, test_x)
rf_accuracy <- mean(rf_preds == test_y)
cat("Random Forest accuracy:", rf_accuracy, "\n")

# Variable importance plot
varImp(train_rf)

##### Ensemble Model #####

ensemble_preds <- data.frame(
  logistic = predictions,
  loess = loess_preds,
  knn = knn_preds,
  rf = rf_preds
)

majority_vote <- function(x) {
  names(sort(table(x), decreasing = TRUE))[1]
}

ensemble_prediction <- apply(ensemble_preds, 1, majority_vote)
ensemble_prediction <- factor(ensemble_prediction, levels = levels(test_y))
ensemble_accuracy <- mean(ensemble_prediction == test_y)
cat("Ensemble model accuracy:", ensemble_accuracy, "\n")

##### Summary of model accuracies #####

accuracies <- data.frame(
  Model = c("Logistic Regression", "LOESS", "k-NN", "Random Forest", "Ensemble"),
  Accuracy = c(
    logistic_accuracy,
    loess_accuracy,
    knn_accuracy,
    rf_accuracy,
    ensemble_accuracy
  )
)

print(accuracies)

best_model <- accuracies[which.max(accuracies$Accuracy), "Model"]
best_accuracy <- max(accuracies$Accuracy)

cat("Model with highest accuracy:", best_model, "\nAccuracy:", best_accuracy, "\n")
