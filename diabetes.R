
###############################################################
# title: Capstone project - Diabetes
# author: Eli F Mayost
# date: 16/11/2019
###############################################################


###############################################################
# Setup environment
###############################################################

# import all needed libraries; Install if not found
if(!require("stats")) install.packages("stats")
if(!require("readr")) install.packages("readr")
if(!require("dplyr")) install.packages("dplyr")
if(!require("glue")) install.packages("glue")
if(!require("plotly")) install.packages("plotly")
if(!require("rpart")) install.packages("rpart")
if(!require("randomForest")) install.packages("randomForest")
if(!require("knitr")) install.packages("knitr")
if(!require("gridExtra")) install.packages("gridExtra")
if(!require("caret")) install.packages("caret")
if(!require("ggcorrplot")) install.packages("ggcorrplot")
if(!require("rattle")) install.packages("rattle")

###############################################################
# Import and lean the data
###############################################################

# read the data from the csv file
data <- readr::read_csv("diabetes.csv")

# Checking that we do not have missing values
data %>% is.na() %>% any()

# data's dimensions
data %>% dim()

# First 10 observations
data %>% head(10) %>% knitr::kable()

# NA have been replaced by 0. Finding how many, and in what rows and columns
columns <- data %>% 
  dplyr::select(-Outcome, -Pregnancies, -Insulin, -DiabetesPedigreeFunction) %>% 
  colnames()

for(c in columns){
  glue::glue("{as.character(c)} has {sum(data[as.character(c)] == 0)} zeroes") %>% 
  print()
}

total_rows <- data %>% filter_at(vars(BMI, SkinThickness, BloodPressure, Glucose), any_vars(. == 0)) %>% nrow()

data %>% dplyr::filter_at(vars(BMI, SkinThickness, BloodPressure, Glucose), any_vars(. == 0)) %>% nrow()

# The zero values are in the 4 columns
# Replacing the zero values with the mean of the other observations having the same outcome.
bmi_mean <- data %>%
  dplyr::filter(! BMI == 0) %>% 
  dplyr::group_by(Outcome) %>% 
  dplyr::summarize(avg = mean(BMI)) %>% 
  .$avg

bp_mean <- data %>% 
  dplyr::filter(! BloodPressure == 0) %>% 
  dplyr::group_by(Outcome) %>% 
  dplyr::summarize(avg = mean(BloodPressure)) %>% 
  .$avg

st_mean <- data %>% 
  dplyr::filter(! SkinThickness == 0) %>% 
  dplyr::group_by(Outcome) %>% 
  dplyr::summarize(avg = mean(SkinThickness)) %>% 
  .$avg

gl_mean <- data %>% 
  dplyr::filter(! Glucose == 0) %>% 
  dplyr::group_by(Outcome) %>% 
  dplyr::summarize(avg = mean(Glucose)) %>% 
  .$avg

data <- data %>%
  dplyr::mutate(BMI = ifelse(BMI == 0 & Outcome == 0, bmi_mean[1],
    ifelse(BMI == 0 & Outcome == 1, bmi_mean[2], BMI))) %>%
  
  dplyr::mutate(BloodPressure = ifelse(BloodPressure == 0 & Outcome == 0, bp_mean[1],
    ifelse(BloodPressure == 0 & Outcome == 1, bp_mean[2], BloodPressure))) %>%
  
  dplyr::mutate(SkinThickness = ifelse(SkinThickness == 0 & Outcome == 0, st_mean[1],
    ifelse(SkinThickness == 0 & Outcome == 1, st_mean[2], SkinThickness))) %>%
  
  dplyr::mutate(Glucose = ifelse(Glucose == 0 & Outcome == 0, gl_mean[1],
    ifelse(Glucose == 0 & Outcome == 1, gl_mean[2], Glucose)))

data %>% head(10) %>% round(digits = 2) %>% knitr::kable()

# Double check that we do not have zero values anymore
data %>% dplyr::filter_at(vars(BMI, SkinThickness, Age, BloodPressure), any_vars(. == 0)) %>% nrow()

###############################################################
# Investigate the data with visualizations
###############################################################

# Quick summary of the features
data %>% 
  dplyr::select(Pregnancies, Glucose, BloodPressure, SkinThickness) %>% 
  summary() %>% 
  knitr::kable()

data %>% 
  dplyr::select(Insulin, BMI, Age, DiabetesPedigreeFunction) %>% 
  summary() %>% 
  knitr::kable()

# The outcome, total of negative and positive observations
data %>%
  ggplot(aes(Outcome)) + 
  geom_bar(aes(color = factor(Outcome), fill = factor(Outcome)), show.legend = F) + 
  scale_x_discrete(limits = c(0, 1)) + 
  scale_y_discrete(name = "Total", limits = seq(0, sum(data$Outcome == 0), 100)) + 
  scale_color_manual(values = c("steelblue", "purple")) +
  scale_fill_manual(values = c("steelblue", "purple")) +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

# Exploring the correlation between features and outcome
data %>% 
  dplyr::mutate(Outcome = as.integer(Outcome)) %>% 
  cor %>% 
  round(digits = 2) %>% 
  knitr::kable()

# Visualizing the correlations with the outcome
data %>% 
  cor %>% 
  ggcorrplot(type = "lower",
             lab = T, 
             colors = c("#6D9EC1", "white", "#E46726"), 
             outline.color = "darkgrey",
             tl.cex = 10,
             legend.title = "correlation")

# Vizualizing if the data is grouped, the outliers, and the
# reliationship betwen a categorical feature (outcome), and 
# a continuous one (Glucose, BMI etc...) with boxplots
make_plot <- function(feature){
  data %>% 
  ggplot(aes(x=Outcome, y=!!rlang::sym(feature))) + 
    
    geom_boxplot(aes(fill=factor(Outcome))) +
    scale_x_discrete(limits = c(0, 1)) +
    scale_color_manual(values = c("steelblue", "purple")) +
    scale_fill_manual("Outcome", values = c("steelblue", "purple")) +
    theme_minimal() +
    theme(axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10),
          legend.title = element_text(size = 10),
          plot.margin = unit(c(1,1,1,1), "cm"))
}

p1 <- make_plot("BMI")
p2 <- make_plot("Insulin")
p3 <- make_plot("SkinThickness")
p4 <- make_plot("Age")
p5 <- make_plot("BloodPressure")
p6 <- make_plot("DiabetesPedigreeFunction")
p7 <- make_plot("Pregnancies")
p8 <- make_plot("Glucose")

grid.arrange(
  grobs = list(p1, p2, p3, p4),
  layout_matrix = rbind(c(1, 2),
                        c(3, 4))
)

grid.arrange(
  grobs = list(p5, p6, p7, p8),
  layout_matrix = rbind(c(1, 2),
                        c(3, 4))
)

###############################################################
# Partitioning the data
###############################################################

# Partitioning the data in two; 70% for the train set, and 30% for the test set
ind <- caret::createDataPartition(data$Outcome, times = 1, p = 0.3, list = F)

train_set <- data %>% dplyr::slice(-ind)
test_set <- data %>% dplyr::slice(ind)

# Checking how many observations in each set
glue::glue("The train set has {nrow(train_set)} observations.")
glue::glue("The test set has {nrow(test_set)} observations.")


###############################################################
# ML models training
###############################################################

### Guessing model
set.seed(1976)

y_hat_guess <- sample(c(0, 1), size = nrow(test_set), replace = T)
conf_matrix_guess <- confusionMatrix(factor(y_hat_guess), factor(test_set$Outcome))
conf_matrix_guess

### Logistic regression model
set.seed(1976)
fit_glm <- train(factor(Outcome) ~ ., data = train_set, method = "glm")
y_hat_glm <- predict(fit_glm, newdata = test_set)
conf_matrix_glm <- confusionMatrix(factor(y_hat_glm), factor(test_set$Outcome))
conf_matrix_glm

### Recursive Partitioning model
# available parameter for tuning
getModelInfo()$rpart$parameters

set.seed(1976)
tune_grid_rpart <- expand.grid(cp = seq(0, 0.2, 0.0025))
fit_rpart <- train(factor(Outcome) ~ ., data = train_set, method = "rpart", tuneGrid = tune_grid_rpart)
y_hat_rpart <- predict(fit_rpart, type = "raw", newdata = test_set)
conf_matrix_rpart <- confusionMatrix(factor(y_hat_rpart), factor(test_set$Outcome))
conf_matrix_rpart

# Best cp value
fit_rpart$results %>% 
  as_tibble() %>% 
  ggplot(aes(x = cp, y = Accuracy)) + 
    geom_line(color = "steelblue") + 
    theme_minimal()

# Visualizing the final model's decision tree
rattle::fancyRpartPlot(fit_rpart$finalModel, caption = "")

### Random Forest model
# available parameter for tuning
getModelInfo()$rf$parameters

set.seed(1976)
fit_rf <- train(factor(Outcome) ~ .,
             data = train_set, 
             method = "rf",
             tuneGrid = data.frame(mtry = seq(train_set %>% colnames() %>% length() - 1)))
y_hat_rf <- predict(fit_rf, newdata = test_set)
conf_matrix_rf <- confusionMatrix(factor(y_hat_rf), factor(test_set$Outcome))
conf_matrix_rf

# Visualizing best mtry value
cols <- replicate(fit_rf$results %>% nrow, "steelblue")
cols[which.max(fit_rf$results$Accuracy)] = "red"

fit_rf$results %>% 
  as_tibble() %>% 
  ggplot(aes(x = mtry, y = Accuracy)) + 
    geom_line(color = "steelblue") + 
    geom_label(aes(label = mtry), fill = cols, color = "white") + 
    theme_minimal()

### K Nearest Neighbours model
#k is the tuning parameter. We will try every second k values from 1 to 100

set.seed(1976)
fit_knn <- train(factor(Outcome) ~ ., data = train_set, method = "knn", tuneGrid = data.frame(k = seq(1, 100, 2)))
y_hat_knn <- predict(fit_knn, newdata = test_set)
conf_matrix_knn <- confusionMatrix(factor(y_hat_knn), factor(test_set$Outcome))
conf_matrix_knn

# Graph overall accuracy vs. the k value
cols <- replicate(fit_knn$results %>% nrow, "steelblue")
cols[which.max(fit_knn$results$Accuracy)] = "red"

fit_knn$results %>% 
  as_tibble() %>% 
  ggplot(aes(x = k, y = Accuracy)) + 
    geom_line(color = "steelblue") + 
    geom_label(aes(label = k), fill = cols, color = "white") + 
    theme_minimal()

# The best k value
fit_knn$bestTune

### Gradient Boosting Machine model
# To tune gbm, the following parameters are required
getModelInfo()$gbm$parameters

# For interaction.depth, the max value is the number of predictors.
# For shrinkage, generally, the smaller the number the better the predicting value keeping in mind the modest computational resources of a laptop.
# n.minobsinnode's default is 10, but for classification smaller number work well. To minimize the time it runs, we will leave it at 10, but we could supply it a sequence like seq(1, 11, 2).
# For small shrinkage, we need more trees. We will go up to 5000 trees.
set.seed(1976)

tune_grid_gbm <- expand.grid(n.trees = seq(50, 5000, 50),
                             interaction.depth = seq(1, 9, 2), 
                             shrinkage = 0.01,
                             n.minobsinnode = 10)
fit_gbm <- train(factor(Outcome) ~ ., data = train_set, method = "gbm", tuneGrid = tune_grid_gbm, verbose = F)
y_hat_gbm <- predict(fit_gbm, newdata = test_set)
conf_matrix_gbm <- confusionMatrix(factor(y_hat_gbm), factor(test_set$Outcome))
conf_matrix_gbm

### ensemble rpart, rf, and gbm model
# We take an average of the different models' predictions, and declare an outcome of 1 when the average is equal or above 2/3 (either 2 or 3 of the methods precict diabetes)

y_hat_ensemble <- (y_hat_rf %>% as.character() %>% as.numeric() + 
                   y_hat_gbm %>% as.character() %>% as.numeric() +
                   y_hat_rpart %>% as.character() %>% as.numeric()) / 3

y_hat_ensemble <-  replace(y_hat_ensemble, y_hat_ensemble >= 2/3, 1)
y_hat_ensemble <-  replace(y_hat_ensemble, y_hat_ensemble < 2/3, 0)

conf_matrix_ensemble <- confusionMatrix(factor(y_hat_ensemble), factor(test_set$Outcome))
conf_matrix_ensemble

###############################################################
# Picking up a model
###############################################################

# Summarizing the accuracies of the different methods
accuracies <- c(
  conf_matrix_guess$overall["Accuracy"],
  conf_matrix_glm$overall["Accuracy"],
  conf_matrix_rpart$overall["Accuracy"],
  conf_matrix_rf$overall["Accuracy"],
  conf_matrix_knn$overall["Accuracy"],
  conf_matrix_gbm$overall["Accuracy"],
  conf_matrix_ensemble$overall["Accuracy"]
)

accuracies_table <- accuracies %>%
  as.table() %>%
  setNames(c("guessing", "glm", "rpart", "rf", "knn", "gbm", "ensemble"))

accuracies_table

###############################################################
# Conclusion
###############################################################

### Conclusion
# Best method
accuracies_table %>% which.max() %>% names()

# With an accuracy of
accuracies_table[accuracies_table %>% which.max()] %>% round(digits = 4) %>% paste("%", sep = "")


