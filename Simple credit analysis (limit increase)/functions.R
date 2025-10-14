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

model_pre_processing <- function (data){ 
  ##### Descriptive Data Exploration ##### 
  
  # Number of samples and features 
  num_samples <- dim(data)[1] 
  num_features <- dim(data)[2] 
  cat("Number of samples:", num_samples, "\n") 
  cat("Number of features:", num_features, "\n") 
  
  #remove NA 
  for (variable in colnames(data)) { 
    if (any(is.na(data[[variable]]))) { 
      print(paste("Column '", variable, "' contains NA values. Removing NA values ​​from this column only...", sep = "")) 
      data[[variable]] <- data[[variable]][!is.na(data[[variable]])] 
    } else { 
      print(paste("Column '", variable, "' has no NA values.", sep = "")) 
    } 
  } 
  
  ## remove bad values 
  for (variable in colnames(data)) { 
    data <- data[!(data[[variable]] %in% c("na", " ")), ] 
  } 
  
  # Summary statistics 
  cat("Summary statistics:\n") 
  ### select only usefull variables

  columns_to_convert <- c("limite_credito", "valor_transacoes_12m")
  
  for (column in columns_to_convert) {
    data[[paste0(column, "_num")]] <- as.numeric(
      gsub(",", ".", gsub("\\.", "", data[[column]]))
    )
  }
  variables_exclude <- c('limite_credito','valor_transacoes_12m',
                         'id','default')
  
  # Transform strings in numeric values, dummies
  data <- dummy_cols(data, 
                      select_columns=c('sexo','escolaridade','estado_civil','salario_anual'))
  
  filtered_data <- data %>%
    select(-all_of(variables_exclude))
  head(filtered_data)
  
  numeric_data <- filtered_data %>%
    select(where(Negate(is.numeric)))
  head(numeric_data)
  str(numeric_data)
  
  # Center and scale features
  numeric_data <- filtered_data %>%
    select(where(is.numeric))
  
  # condition to target variable
  numeric_data$tg_aumento <- ifelse(
    numeric_data$meses_de_relacionamento >= 12 &
      numeric_data$qtd_transacoes_12m >= 24 &
      numeric_data$limite_credito_num > 0 &
      ((numeric_data$valor_transacoes_12m_num) / numeric_data$limite_credito_num) > 0.8 &
      numeric_data$dependentes >= 2,
    1, # if condition is true
    0 # if condition is false
  )
  return (numeric_data)
}

normalization <- function (numeric_data){
  # ----------------------------------------
  # NORMALIZATION AND VARIABLE REDUCTION
  # ----------------------------------------
  
  ####################
  target <- numeric_data$tg_aumento
  predictor_data <- numeric_data[, setdiff(names(numeric_data), "tg_aumento")]
  
  # ----------------------------------------
  # REMOVE VARIABLE > +70% OF SAME VALUE
  # ----------------------------------------
  repeated_ratio <- sapply(predictor_data, function(col) {
    max(table(col)) / length(col)
  })
  
  repeated_vars <- names(repeated_ratio[repeated_ratio > 0.7])
  
  if (length(repeated_vars) > 0) {
    cat("Removed:\n")
    print(repeated_vars)
    
    # Remove
    predictor_data <- predictor_data[, !(names(predictor_data) %in% repeated_vars)]
  }
  
  # ----------------------------------------
  # NORMALIZATION
  # ----------------------------------------
  
  x_centered <- sweep(predictor_data, 2, colMeans(predictor_data))
  x_scaled <- sweep(x_centered, 2, colSds(as.matrix(predictor_data)), FUN = "/")
  x_scaled <- cbind(x_scaled, tg_aumento = target)
  
  # SAVE TRAINING PARAMETERS FOR FUTURE USE
  # ----------------------------------------
  
  # Means and Deviations used in normalization (before tg_augmentation is added)
  medias_treino <- colMeans(predictor_data)
  sds_treino <- colSds(as.matrix(predictor_data))
  
  # Correlation
  cor_matrix <- cor(x_scaled, use = "complete.obs")
  high_cor <- findCorrelation(cor_matrix, cutoff = 0.5, names = TRUE)
  
  preserve_variables <- c("tg_aumento","limite_credito_num","qtd_transacoes_12m","valor_transacoes_12m_num","estado_civil")
  high_cor <- setdiff(high_cor, preserve_variables)
  
  x_scaled <- x_scaled[, !(names(x_scaled) %in% high_cor)]
  
  # Save parameters in .rds files
  saveRDS(medias_treino, "C:/Users/xxxx/Downloads/xxx/medias_treino.rds")
  saveRDS(sds_treino, "C:/Users/xxxx/Downloads/xxx/sds_treino.rds")
  saveRDS(high_cor, "C:/Users/xxxx/Downloads/xxx/variaveis_removidas_correlacao.rds")
  
  return(x_scaled)
  
}