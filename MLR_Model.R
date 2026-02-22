#===============================
# 1. Load libraries
#===============================
library(nnet)        # For multinomial logistic regression
library(caret)       # For evaluation
library(dplyr)       # For data handling

#===============================
# 2. Define Variables for Each Pipeline
#===============================
# Define predictor variables
pipeline_a_vars <- setdiff(names(train_data), "predicted_class")

pipeline_b_vars <- setdiff(pipeline_a_vars, "current_energy_efficiency")
pipeline_b_vars <- unique(c(pipeline_b_vars,
                            "energy_consumption_current",
                            "co2_per_m2",
                            "energy_per_m2"))

#===============================
# 3. Train Multinomial Logistic Model
#===============================
# Pipeline A
logit_train_a <- train_data[, c(pipeline_a_vars, "predicted_class")]
logit_test_a  <- test_data[, c(pipeline_a_vars, "predicted_class")]

logit_model_a <- multinom(predicted_class ~ ., data = logit_train_a, maxit = 500, trace = FALSE)
logit_pred_a  <- predict(logit_model_a, newdata = logit_test_a)

# Pipeline B
logit_train_b <- train_data[, c(pipeline_b_vars, "predicted_class")]
logit_test_b  <- test_data[, c(pipeline_b_vars, "predicted_class")]

logit_model_b <- multinom(predicted_class ~ ., data = logit_train_b, maxit = 500, trace = FALSE)
logit_pred_b  <- predict(logit_model_b, newdata = logit_test_b)

#===============================
# 4. Evaluate Models
#===============================
# Pipeline A evaluation
cm_logit_a <- confusionMatrix(logit_pred_a, logit_test_a$predicted_class)
cat("📊 Logistic Regression - Pipeline A Accuracy:", round(cm_logit_a$overall["Accuracy"], 4), "\n")

# Pipeline B evaluation
cm_logit_b <- confusionMatrix(logit_pred_b, logit_test_b$predicted_class)
cat("📊 Logistic Regression - Pipeline B Accuracy:", round(cm_logit_b$overall["Accuracy"], 4), "\n")
