###########################################################
##### Machine Learning ####################################
library(dslabs)
library(tidyverse)
library(caret)
library(ggrepel)
library(lubridate)
library(gridExtra)
library(purrr)
library(rpart)
library(randomForest)
library(ggplot2)
library(rpart.plot)

data(heights)
#simple example predicting/guessing heights
# define the outcome and predictors
y <- heights$sex
x <- heights$height

# generate training and test sets, in this case p represent 50% of my data
set.seed(2007)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

# guess the outcome
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>%
  factor(levels = levels(test_set$sex))

# calculate accuracy
mean(y_hat == test_set$sex)

# compare the heights of men and women in our dataset
heights %>%
  group_by(sex) %>%
  summarize(mean(height), sd(height))

# Now try to predict "Male" if the height is within 2 SD of the male mean.
y_hat <- ifelse(x > 62, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
mean(y == y_hat)

# Examine the accuracy of 10 cutoff points
cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})
data.frame(cutoff, accuracy) %>%
  ggplot(aes(cutoff, accuracy)) +
  geom_point() +
  geom_line()
max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex) #Better if compare with our first result

# Some metrics usefull
#Confusion Matrix 
confusionMatrix(data = y_hat, reference = test_set$sex)
#F1 - Score
#Considers specificity and sensitivity
cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x,"Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})
F_1
data.frame(cutoff, F_1) %>% 
  ggplot(aes(cutoff, F_1)) + 
  geom_point() + 
  geom_line()

max(F_1)

best_cutoff_2 <- cutoff[which.max(F_1)]
best_cutoff_2

y_hat <- ifelse(test_set$height > best_cutoff_2, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
sensitivity(data = y_hat, reference = test_set$sex)
################################################################
# ROC CURVE SENSITIVITY vs 1-SPECIFICITY
p <- 0.9
n <- length(test_index)
y_hat <- sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)

# ROC curve
probs <- seq(0, 1, length.out = 10)
test_set$sex <- factor(test_set$sex, levels = c("Female", "Male"))
n <- nrow(test_set)

guessing <- map_df(probs, function(p) {
  y_hat <- sample(c("Male", "Female"), n, replace = TRUE, prob = c(p, 1 - p)) %>% 
    factor(levels = c("Female", "Male"))
  
  cm <- confusionMatrix(y_hat, test_set$sex, positive = "Male")
  
  tibble(
    method = "Guessing",
    prob_male = p,
    FPR = 1 - cm$byClass["Specificity"],
    TPR = cm$byClass["Sensitivity"]
  )
})
guessing %>% qplot(FPR, TPR, data =., xlab = "1 - Specificity", ylab = "Sensitivity")

# PRECISION-RECALL TO CONSIDER PREVALENCE
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), 
                  replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guess",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()
###########################
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, 
                  prob=c(p, 1-p)) %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Guess",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()

################################################################
#### Linear Model 
################################################################
#check data
head(heights)
str(heights)
table(heights$sex)
summary(heights$height)
#model
modelo <- lm(height ~ sex, data = heights)
summary(modelo)
#plot
ggplot(heights, aes(x = sex, y = height)) +
  geom_boxplot() +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.3) +
  stat_summary(fun = mean, geom = "point", color = "red", size = 3)
#predict
predict(modelo, newdata = data.frame(sex = c("Male", "Female")))

################################################################
##### KNN Model
################################################################
#using the new dataset (numbers)
data("mnist_27")

mnist_27$test %>%
  ggplot(aes(x_1, x_2, color = y)) +
  geom_point()
#knn model
knn_fit <- knn3(y ~ ., data = mnist_27$train)
#predict result and metrics
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall["Accuracy"]

##### Again Linear Model to comparate
fit_lm <- mnist_27$train %>% 
  mutate(y = ifelse(y == 7, 1, 0)) %>% 
  lm(y ~ x_1 + x_2, data = .)
p_hat_lm <- predict(fit_lm, mnist_27$test)
y_hat_lm <- factor(ifelse(p_hat_lm > 0.5, 7, 2))
confusionMatrix(y_hat_lm, mnist_27$test$y)$overall["Accuracy"]

######### Plot KNN Model###########
plot_cond_prob <- function(p_hat=NULL){
  tmp <- mnist_27$true_p
  if(!is.null(p_hat)){
    tmp <- mutate(tmp, p=p_hat)
  }
  tmp %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
    geom_raster(show.legend = FALSE) +
    scale_fill_gradientn(colors=c("#F8766D", "white", "#00BFC4")) +
    stat_contour(breaks=c(0.5), color="black")
}
p1 <- plot_cond_prob() +
  ggtitle("True Cond. Prob")
p2 <- plot_cond_prob(predict(knn_fit, mnist_27$true_p)[,2]) +
  ggtitle("Estimate kNN-5")
grid.arrange(p2, p1, nrow=1)

y_hat_knn <- predict(knn_fit, mnist_27$train, type = "class")
confusionMatrix(y_hat_knn, mnist_27$train$y)$overall["Accuracy"]

y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall["Accuracy"]

#################################################################
### Define K value to KNN
ks <- seq(3, 251, 2)

accuracy <- map_df(ks, function(k){
  fit <- knn3(y ~ ., data = mnist_27$train, k = k)
  
  y_hat <- predict(fit, mnist_27$train, type = "class")
  cm_train <- confusionMatrix(y_hat, mnist_27$train$y)
  train_error <- cm_train$overall["Accuracy"]
  
  y_hat <- predict(fit, mnist_27$test, type = "class")
  cm_test <- confusionMatrix(y_hat, mnist_27$test$y)
  
  test_error <- cm_test$overall["Accuracy"]
  
  tibble(train = train_error, test = test_error)
})

accuracy %>% mutate(k = ks) %>%
  gather(set, accuracy, -k) %>%
  mutate(set = factor(set, levels = c("train", "test"))) %>%
  ggplot(aes(k, accuracy, color = set)) + 
  geom_line() +
  geom_point()

ks[which.max(accuracy$test)]
max(accuracy$test)
#################################################################
########## Bootstrap ######################################################

#Suppose the income distribution of your population is as follows:

set.seed(1995)
n <- 10^6
income <- 10^(rnorm(n, log10(45000), log10(3)))
qplot(log10(income), bins = 30, color = I("black"))

#The population median is given by the following code:
m <- median(income)

#However, if we don't have access to the entire population,
#but want to estimate the median, we can obtain a sample of 100
#and estimate the population median using the sample median, as follows:
N <- 100
X <- sample(income, N)
median(X)

#Since we are simulating the data,
#we can use a Monte Carlo simulation
#to learn the distribution of using the following code:

B <- 10^4
M <- replicate(B, {
  X <- sample(income, N)
  median(X)
})
p1 <- qplot(M, bins = 30, color = I("black"))
p2 <- qplot(sample = scale(M), xlab = "theoretical", ylab = "sample") +
  geom_abline()
grid.arrange(p1, p2, ncol = 2)

#Knowing the distribution allows us
#to construct a confidence interval.
#However, as we discussed before,
#in practice we do not have access to the distribution.

#In the past, we've used the Central Limit Theorem (CLT),
#but the CLT we studied applies to means, and here we're
#interested in the median.
#If we construct the 95% confidence interval
#based on CLT using the code below,
#we'll see that it's quite different from the
#confidence interval we would generate if we knew the
#actual distribution of .

median(X) + 1.96 * sd(X) / sqrt(N) * c(-1, 1)
#The 95% confidence interval based on CLT

quantile(M, c(0.025, 0.975))

#The confidence interval based on the actual distribution 

##The bootstrap allows us to approximate a Monte Carlo simulation
#without access to the full distribution. The general idea is relatively simple.
#We act as if the observed sample were the population.
#Next, we sample (with replacement) data sets of the same size
#as the original data set. We then compute the summary statistic,
#in this case the median, on these bootstrap samples.

#Theory tells us that, in many situations,
#the distribution of statistics obtained with bootstrap samples
#approximates the distribution of our actual statistic. #We can construct bootstrap samples and an approximate distribution
#using the following code:

B <- 10^4
M_star <- replicate(B, {
  X_star <- sample(X, N, replace = TRUE)
  median(X_star)
})
#The confidence interval constructed using the bootstrap is much closer
#to that constructed with the theoretical distribution,
#as you can see when using this code:
quantile(M_star, c(0.025, 0.975))

########### another abridged version of the code above
# define the population distribution of income
set.seed(1995)
n <- 10^6
income <- 10^(rnorm(n, log10(45000), log10(3)))
qplot(log10(income), bins = 30, color = I("black"))

# calculate the population median
m <- median(income)
m

# estimate the population median
N <- 100
X <- sample(income, N)
M <- median(X)
M

# use a Monte Carlo simulation to find the distribution of M
B <- 10^4
M <- replicate(B, {
  X <- sample(income, N)
  median(X)
})
p1 <- qplot(M, bins = 30, color = I("black"))
p2 <- qplot(sample = scale(M), xlab = "theoretical", ylab = "sample") + geom_abline()
grid.arrange(p1, p2, ncol = 2)

# compare the 95% CI based on the TCL with the actual one
median(X) + 1.96 * sd(X) / sqrt(N) * c(-1, 1)
quantile(M, c(0.025, 0.975))

# bootstrap and approximate the distribution
B <- 10^4
M_star <- replicate(B, {
  X_star <- sample(X, N, replace = TRUE)
  median(X_star)
})

# examine the bootstrap confidence interval
quantile(M_star, c(0.025, 0.975))

###########################################################
###########################################################
## Caret package
#consolidates machine learning methods
#The train() function allows you to easily train different models

train_glm <- train(y ~ ., method = "glm", data = mnist_27$train)
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)

y_hat_glm <- predict(train_glm, mnist_27$test, type = "raw")
y_hat_knn <- predict(train_knn, mnist_27$test, type = "raw")

confusionMatrix(y_hat_glm, mnist_27$test$y)$overall[["Accuracy"]]
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall[["Accuracy"]]

######################################################
######################################################
####################### Random Trees and Forests #####
# Train the decision tree model
# Define depth grid for testing
# List of depths to test
depths <- c(1, 2, 3, 4, 5, 6, 7)

resultados_arvore <- data.frame(
  maxdepth = integer(),
  Accuracy = numeric()
)

set.seed(123)
for (md in depths) {
  model <- rpart(y ~ ., data = mnist_27$train,
                 method = "class",
                 control = rpart.control(maxdepth = md, cp = 0)) # cp = 0 to let it grow to maxdepth
  
  pred <- predict(model, mnist_27$test, type = "class")
  
  acc <- confusionMatrix(pred, mnist_27$test$y)$overall["Accuracy"]
  
  resultados_arvore <- rbind(resultados_arvore,
                             data.frame(maxdepth = md, Accuracy = acc))
}

print(resultados_arvore)

# Select best depth
best_depth <- resultados_arvore$maxdepth[which.max(resultados_arvore$Accuracy)]
cat("Best depth:", best_depth, "\n")

# Train the final model with the best depth
final_tree_model <- rpart(y ~ ., data = mnist_27$train,
                          method = "class",
                          control = rpart.control(maxdepth = best_depth, cp = 0))

# Predict on test
final_tree_pred <- predict(final_tree_model, mnist_27$test, type = "class")

# Evaluate
final_tree_metrics <- confusionMatrix(final_tree_pred, mnist_27$test$y)
print(final_tree_metrics)

# Importance of Varaible
importance_arvore <- final_tree_model$variable.importance
print(importance_arvore)

# Plot Importance
barplot(importance_arvore,
        main = "Importance of Varaible - Decision Tree",
        col = "lightblue",
        ylab = "Importance")


########## Random Forest ###########################
# Train the random forest model
# Load data
data("mnist_27")

# Define hyperparameter lists
ntree_list <- c(50, 100, 200)
maxnodes_list <- c(5, 10, 15)

# Create data frame to store the results
results <- data.frame(
  ntree = integer(),
  maxnodes = integer(),
  mtry = integer(),
  accuracy = numeric()
)

# Loop to test combinations
set.seed(123)
for (ntree in ntree_list) {
  for (maxnodes in maxnodes_list) {
    for (mtry_val in c(1, 2)) { # possible mtry values ​​(because we only have 2 features)
      
      # Train model
      model <- randomForest(y ~ .,
                            data = mnist_27$train,
                            ntree = ntree,
                            maxnodes = maxnodes,
                            mtry = mtry_val)
      
      # Predict
      pred <- predict(model, mnist_27$test)
      
      # Evaluate
      acc <- confusionMatrix(pred, mnist_27$test$y)$overall["Accuracy"]
      
      # Store results
      results <- rbind(results, data.frame(
        ntree = ntree,
        maxnodes = maxnodes,
        mtry = mtry_val,
        Accuracy = acc
      ))
    }
  }
}

# View all results
print(results)

# Select the best
best_model <- results[which.max(results$Accuracy), ]
cat("\n🔍 Best combination of hyperparameters:\n")
print(best_model)

###### apply in the final model
modelo_rf_final <- randomForest(y ~ ., 
                                data = mnist_27$train,
                                ntree = best_model$ntree,
                                maxnodes = best_model$maxnodes,
                                mtry = best_model$mtry)

# Avaliar no teste
confusionMatrix(predict(modelo_rf_final, mnist_27$test), mnist_27$test$y)
# Prediction on the test set
pred_rf_final <- predict(modelo_rf_final, mnist_27$test)

# Show the first predictions
head(pred_rf_final)
##
importance(modelo_rf_final)
varImpPlot(modelo_rf_final,
           main = "Importance of variables - Random Forest")
