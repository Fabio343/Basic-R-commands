# ----------------------------------------
# SETUP AND PACKAGES
# ----------------------------------------
options(digits = 3)
library(tidyverse)
library(caret)
library(fastDummies)
library(matrixStats)
library(corrplot)
library(arrow)
library(matrixStats)
library(dslabs)
library(ggplot2)
library(reshape2)
library(dplyr)
source("C:/Users/xxx/Downloads/xxx/functions.R")
# ----------------------------------------
# IMPORTATION AND CLEANING
# ----------------------------------------
data <- read.csv("C:/Users/xxx/Downloads/xxx/credito.csv", stringsAsFactors = FALSE)

#"$60K - $80K" to "60K_80K"
data$salario_anual <- gsub("\\$([0-9]+K) *- *\\$([0-9]+K)", "\\1_\\2", data$salario_anual)
# $40K" to "ate_40K"
data$salario_anual <- gsub("menos que \\$([0-9]+K)", "ate_\\1", data$salario_anual, ignore.case = TRUE)
# "$120K +" → "120K+"
data$salario_anual <- gsub("\\$?([0-9]+K) *\\+", "\\1+", data$salario_anual)
# remove \\$
data$salario_anual <- gsub("\\$", "", data$salario_anual)


# if there are no salary ranges annual
#data$annual_salary <- cut(
# as.numeric(data$annual_salary),
# breaks = c(0, 40000, 60000, 80000, 120000, Inf),
# labels = c("less than $40K", "$40K - $60K", "$60K - $80K", "$80K - $120K", "$120K +"),
# include.lowest = TRUE
#)

# Plotting features
# ⚠️ Cleaning step: transform 'credit_limit' and other columns to numeric (if they come with symbols)
cleaned_data <- data %>%
  mutate(
    salario_anual = as.factor(salario_anual), # ensure it is categorical
    limite_credito = as.numeric(str_replace_all(limite_credito, "[\\$,]", "")),
    valor_transacoes_12m = as.numeric(str_replace_all(valor_transacoes_12m, "[\\$,]", ""))
  )
colnames(cleaned_data)
# ✅ Select only numeric variables + salary range
numeric_variables <- cleaned_data %>%
  select(
    salario_anual,
    idade, dependentes, meses_de_relacionamento,
    qtd_produtos, iteracoes_12m, meses_inativo_12m,
    limite_credito, valor_transacoes_12m, qtd_transacoes_12m
  )

# 🔄 Convert to long format
long_data <- numeric_variables %>%
  pivot_longer(
    cols = - salario_anual,
    names_to = "Variable",
    values_to = "Value"
  )

# 📊 Create boxplot chart
ggplot(long_data, aes(x = salario_anual, y = Value, fill = salario_anual)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  #geom_jitter(width = 0.2, size = 1, alpha = 0.4, color = "black") +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(
    title = "Distribution of Variables by Salary Range",
    x = "Salary Range",
    y = "Variable Value"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# data processing function
numeric_data <- model_pre_processing(cleaned_data)
colnames(numeric_data)
x_scaled <- normalization(numeric_data)

# ----------------------------------------
# SPLIT TRAINING/TEST
# ----------------------------------------
set.seed(1)
x_scaled$tg_aumento <- factor(x_scaled$tg_aumento, levels = c(0,1))
test_index <- createDataPartition(x_scaled$tg_aumento, times = 1, p = 0.2, list = FALSE)

train_x <- x_scaled[-test_index, setdiff(names(x_scaled), "tg_aumento")]
train_y <- x_scaled$tg_aumento[-test_index]
test_x <- x_scaled[test_index, setdiff(names(x_scaled), "tg_aumento")]
test_y <- x_scaled$tg_aumento[test_index]

# ----------------------------------------
# TEMPLATES
# ----------------------------------------

##### MODels
##### Logistic Regression Model #####

train_df <- data.frame(train_x)
train_df$tg_aumento <- train_y

model <- glm(tg_aumento ~ ., data = train_df, family = "binomial")

# train
train_probs <- predict(model, newdata = train_df, type = "response")
train_predictions <- ifelse(train_probs > 0.5, 1, 0)
train_predictions <- factor(train_predictions, levels = levels(train_y))

logistic_accuracy_train <- mean(train_predictions == train_y)
cat("Logistic Regression train accuracy:", logistic_accuracy_train, "\n")

# test
test_df <- data.frame(test_x)
test_probs <- predict(model, newdata = test_df, type = "response")
test_predictions <- ifelse(test_probs > 0.5, 1, 0)
test_predictions <- factor(test_predictions, levels = levels(test_y))

logistic_accuracy_test <- mean(test_predictions == test_y)
cat("Logistic Regression test accuracy:", logistic_accuracy_test, "\n")

# ConfusionMatrix
confusionMatrix(test_predictions, test_y)

##### k-Nearest Neighbors Model #####
set.seed(7, sample.kind = "Rounding")
tuning <- data.frame(k = seq(3, 21, 2))
train_knn <- train(train_x, train_y, 
                   method = "knn", 
                   tuneGrid = tuning)
best_k <- train_knn$bestTune$k
cat("Best k for kNN:", best_k, "\n")

knn_preds_train <- predict(train_knn, train_x)
knn_accuracy <- mean(knn_preds_train == train_y)
cat("kNN model accuracy:", knn_accuracy, "\n")

knn_preds <- predict(train_knn, test_x)
knn_accuracy <- mean(knn_preds == test_y)
cat("kNN model accuracy:", knn_accuracy, "\n")

#Confusion Matrix
confusionMatrix(knn_preds, test_y)
##### Random Forest Model #####

set.seed(9, sample.kind = "Rounding")
tuning_rf <- data.frame(mtry = c(3, 5, 7, 9))
train_rf <- train(train_x, train_y, 
                  method = "rf", 
                  tuneGrid = tuning_rf, 
                  importance = TRUE)
best_mtry <- train_rf$bestTune$mtry
cat("Best mtry for Random Forest:", best_mtry, "\n")

rf_preds_train <- predict(train_rf, train_x)

rf_accuracy <- mean(rf_preds_train == train_y)
cat("Random Forest accuracy:", rf_accuracy, "\n")

# Variable importance plot train
varImp(train_rf)

rf_preds <- predict(train_rf, test_x)
rf_accuracy <- mean(rf_preds == test_y)
cat("Random Forest accuracy:", rf_accuracy, "\n")

# Variable importance plot
varImp(train_rf)

# ConfusionMatrix
confusionMatrix(rf_preds, test_y)

##### Ensemble Model #####

ensemble_preds <- data.frame( 
  logistics = test_probs, 
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
  Model = c("Logistic Regression", "k-NN", "Random Forest", "Ensemble"), 
  Accuracy = c( 
    logistic_accuracy_test, 
    knn_accuracy, 
    rf_accuracy, 
    ensemble_accuracy 
  )
)

print(accuracies)

best_model <- accuracies[which.max(accuracies$Accuracy), "Model"]
best_accuracy <- max(accuracies$Accuracy)

cat("Model with highest accuracy:", best_model, "\nAccuracy:", best_accuracy, "\n")

# Mark who was chosen for augmentation
ensemble_final <- apply(ensemble_preds, 1, majority_vote)
ensemble_final <- factor(ensemble_final, levels = levels(test_y))

data$selected_for_augmentation <- NA
data$selected_for_augmentation[test_index] <- ifelse(ensemble_final == "1", 1, 0)
data
# ----------------------------------------
# SAVE MODELS
# ----------------------------------------
if (!dir.exists("C:/Users/xxx/Downloads/xxx/modelos_uso")) dir.create("C:/Users/xxx/Downloads/xxx/modelos_uso")
saveRDS(model, "C:/Users/xxx/Downloads/xxx/modelo_logistico.rds")
saveRDS(train_knn, "C:/Users/xxx/Downloads/xxx/modelo_knn.rds")
saveRDS(train_rf, "C:/Users/xxx/Downloads/xxx/modelo_rf.rds")
