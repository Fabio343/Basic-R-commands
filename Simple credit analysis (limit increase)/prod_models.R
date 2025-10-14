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
library(dslabs)
library(ggplot2)
library(reshape2)
library(dplyr)
library(readr)
source("C:/Users/xxxx/Downloads/xxxx/functions.R")

# ----------------------------------------
# FUNCTION TO CLEAR MONETARY VALUES
# ----------------------------------------
limpar_valores <- function(x) {
  x <- gsub("\\.", "", x) # remove periods (thousands)
  x <- gsub(",", ".", x) # replace decimal point with point
  as.numeric(x)
}

# ----------------------------------------
# READING AND PROCESSING THE NEW BASE
# ----------------------------------------
csv_file <- "C:/Users/xxx/Downloads/xxx/clientes_sinteticos.csv"

new_base <- read_delim(
  csv_file,
  delim = ";",
  locale = locale(decimal_mark = ",", grouping_mark = ".")
)
new_base <- as.data.frame(new_base)

# "$60K - $80K" to "60K_80K"
new_base$salario_anual <- gsub("\\$([0-9]+K) *- *\\$([0-9]+K)", "\\1_\\2", new_base$salario_anual)
# $40K" to "ate_40K"
new_base$salario_anual <- gsub("menos que \\$([0-9]+K)", "ate_\\1", new_base$salario_anual, ignore.case = TRUE)
# "$120K +" → "120K+"
new_base$salario_anual <- gsub("\\$?([0-9]+K) *\\+", "\\1+", new_base$salario_anual)
#  Remove \\$
new_base$salario_anual <- gsub("\\$", "", new_base$salario_anual)

# Saves original copy for later reattachment
new_base <- new_base %>%
  mutate(
    salario_anual = as.factor(salario_anual), # ensure it is categorical
    limite_credito = as.numeric(str_replace_all(limite_credito, "[\\$,]", "")),
    valor_transacoes_12m = as.numeric(str_replace_all(valor_transacoes_12m, "[\\$,]", ""))
  )

head(new_base)
# ----------------------------------------
# LOAD OBJECTS FROM NORMALIZATION AND REMOVED VARIABLES
# ----------------------------------------
medias_treino <- readRDS("C:/Users/xxx/Downloads/xxx/medias_treino.rds")
sds_treino <- readRDS("C:/Users/xxx/Downloads/xxx/sds_treino.rds")
high_cor <- readRDS("C:/Users/xxx/Downloads/xxx/variaveis_removidas_correlacao.rds")

# data processing function
new_base <- model_pre_processing(new_base)

# ----------------------------------------
# NORMALIZATION WITH TRAINING PARAMETERS
# ----------------------------------------

# ----------------------------------------
# REMOVE VARIABLE > +70% OF SAME VALUE
# ----------------------------------------
repeated_ratio <- sapply(new_base, function(col) {
  max(table(col)) / length(col)
})

repeated_vars <- names(new_base[new_base > 0.7])

if (length(repeated_vars) > 0) {
  cat("Removed:\n")
  print(repeated_vars)
  
  # Remove
  new_base <- new_base[, !(names(new_base) %in% repeated_vars)]
}

# ----------------------------------------
# CHECK COLUMNS AND ALIGN
# ----------------------------------------
missing_cols <- setdiff(names(medias_treino), names(new_base))
if(length(missing_cols) > 0) {
  stop("Missing columns in nova base: ", paste(missing_cols, collapse = ", "))
}

# Select and sort the columns in the same order as in the training
new_base_alinhada <- new_base[, names(medias_treino), drop = FALSE]

# Normalize with training means and deviations
nova_centered <- sweep(new_base_alinhada, 2, medias_treino)
nova_scaled <- sweep(nova_centered, 2, sds_treino, FUN = "/")
nova_scaled <- as.data.frame(nova_scaled)

# Remove highly correlated variables
# Variable we want to preserve
corre <- cor(nova_scaled)
cor_matrix <- cor(corre, use = "complete.obs")
cutoff <- 0.5
high_cor <- findCorrelation(cor_matrix, cutoff = cutoff, names = TRUE)

variable_important <- c("tg_aumento","limite_credito_num","qtd_transacoes_12m","valor_transacoes_12m_num","estado_civil_casado")
# Remove the important variable from the list of removed variables
high_cor <- setdiff(high_cor, variable_important)
nova_scaled <- nova_scaled[, !(names(nova_scaled) %in% high_cor)]
head(nova_scaled)

# ----------------------------------------
# LOAD MODELS
# ----------------------------------------
model_log <- readRDS("C:/Users/xxx/Downloads/xxx/modelo_logistico.rds")
model_knn <- readRDS("C:/Users/xxx/Downloads/xxx/modelo_knn.rds")
model_rf <- readRDS("C:/Users/xxx/Downloads/xxx/modelo_rf.rds")

# ----------------------------------------
# PREDICTION
# ----------------------------------------
pred_log_nova <- factor(ifelse(predict(model_log, nova_scaled, type = "response") > 0.5, 1, 0), levels = c(0, 1))
pred_knn_nova <- predict(model_knn, nova_scaled)
pred_rf_nova <- predict(model_rf, nova_scaled)

colnames(nova_scaled)

# Majority Vote Function
majority_vote <- function(x) {
  names(sort(table(x), decreasing = TRUE))[1]
}

ensemble_nova <- apply(
  data.frame(logistic = pred_log_nova, knn = pred_knn_nova, rf = pred_rf_nova),
  1, majority_vote
)

ensemble_nova <- factor(ensemble_nova, levels = c(0, 1))

# Add to original base
new_base$selected_for_augmentation <- as.integer(as.character(ensemble_nova))

# ----------------------------------------
# SAVE RESULTS
# ----------------------------------------
write_parquet(new_base, "C:/Users/xxx/Downloads/xxx/clientes_novos_com_predicao.parquet")

# Save CSV
write_csv2(new_base, "C:/Users/xxx/Downloads/xxx/clientes_novos_com_predicao.csv")

# Quick Preview
head(new_base)

# ----------------------------------------
# MODEL EVALUATION METRICS

# Assuming you have the default real vector (0/1) in the new original basis
# If not, replace this line with the correct variable
default_real <- nova_original$default

if (!is.null(default_real)) {
  
  # Convert to factor, aligning levels
  default_real <- factor(default_real, levels = c(0, 1))
  
  # Confusion matrix for the ensemble
  confusion_ensemble <- caret::confusionMatrix(ensemble_nova, default_real)
  print(confusion_ensemble)
  
  # ROC curve and AUC for the logistic model (example)
  library(pROC)
  
  # Probabilities predicted by the logistic model
  pred_log_prob <- predict(model_log, nova_scaled, type = "response")
  
  roc_obj <- roc(default_real, pred_log_prob)
  print(roc_obj)
  
  plot(roc_obj, main = "ROC Curve - Logistic Model", col = "blue")
  auc_val <- auc(roc_obj)
  cat("AUC:", auc_val, "\n")
  
  # KS Statistic (Kolmogorov-Smirnov)
  # KS function: maximum absolute value of the difference between cumulative distributions of probs for classes 0 and 1
  pred_df <- data.frame(
    default_real = default_real,
    prob = pred_log_prob
  )
  
  ecdf0 <- ecdf(pred_df$prob[pred_df$default_real == 0])
  ecdf1 <- ecdf(pred_df$prob[pred_df$default_real == 1])
  
  all_probs <- sort(unique(pred_df$prob))
  ks_stat <- max(abs(ecdf1(all_probs) - ecdf0(all_probs)))
  cat("KS Statistic:", ks_stat, "\n")
  
} else {
  cat("Vector 'default_real' not found. Unable to calculate metrics.\n")
}