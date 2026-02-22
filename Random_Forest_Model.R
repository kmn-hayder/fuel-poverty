# ===== Load Required Library =====
library(randomForest)

# ===== Prepare Training & Test Data =====

# Pipeline A
rf_train_a <- train_data[, c(pipeline_a_vars, "predicted_class")]
rf_test_a  <- test_data[, c(pipeline_a_vars, "predicted_class")]

# Pipeline B
rf_train_b <- train_data[, c(pipeline_b_vars, "predicted_class")]
rf_test_b  <- test_data[, c(pipeline_b_vars, "predicted_class")]

# ===== Train Random Forest - Pipeline A =====
set.seed(42)
rf_model_a <- randomForest(
  x = rf_train_a[, pipeline_a_vars],
  y = rf_train_a$predicted_class,
  ntree = 300
)

# Predict on test set A
rf_pred_a <- predict(rf_model_a, newdata = rf_test_a[, pipeline_a_vars])

# Accuracy for Pipeline A
rf_acc_a <- mean(rf_pred_a == rf_test_a$predicted_class)
cat("Random Forest - Pipeline A Accuracy:", round(rf_acc_a, 4), "\n")


# ===== Train Random Forest - Pipeline B =====
set.seed(42)
rf_model_b <- randomForest(
  x = rf_train_b[, pipeline_b_vars],
  y = rf_train_b$predicted_class,
  ntree = 300
)

# Predict on test set B
rf_pred_b <- predict(rf_model_b, newdata = rf_test_b[, pipeline_b_vars])

# Accuracy for Pipeline B
rf_acc_b <- mean(rf_pred_b == rf_test_b$predicted_class)
cat("Random Forest - Pipeline B Accuracy:", round(rf_acc_b, 4), "\n")

library(caret)

#==========================
# 📦 Pipeline A Evaluation
#==========================

# Confusion Matrix
cm_rf_a <- confusionMatrix(rf_pred_a, rf_test_a$predicted_class)
print(cm_rf_a)

# Macro F1
label_map <- levels(train_data$predicted_class)
f1_per_class_a <- numeric(length(label_map))

for (i in seq_along(label_map)) {
  cls <- label_map[i]
  tp <- cm_rf_a$table[cls, cls]
  fp <- sum(cm_rf_a$table[, cls]) - tp
  fn <- sum(cm_rf_a$table[cls, ]) - tp
  precision <- ifelse(tp + fp == 0, 0, tp / (tp + fp))
  recall <- ifelse(tp + fn == 0, 0, tp / (tp + fn))
  f1 <- ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
  f1_per_class_a[i] <- f1
}
macro_f1_rf_a <- mean(f1_per_class_a)
cat("Random Forest - Pipeline A Macro F1:", round(macro_f1_rf_a, 4), "\n")


#==========================
# 📦 Pipeline B Evaluation
#==========================

# Confusion Matrix
cm_rf_b <- confusionMatrix(rf_pred_b, rf_test_b$predicted_class)
print(cm_rf_b)

# Macro F1
f1_per_class_b <- numeric(length(label_map))

for (i in seq_along(label_map)) {
  cls <- label_map[i]
  tp <- cm_rf_b$table[cls, cls]
  fp <- sum(cm_rf_b$table[, cls]) - tp
  fn <- sum(cm_rf_b$table[cls, ]) - tp
  precision <- ifelse(tp + fp == 0, 0, tp / (tp + fp))
  recall <- ifelse(tp + fn == 0, 0, tp / (tp + fn))
  f1 <- ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
  f1_per_class_b[i] <- f1
}
macro_f1_rf_b <- mean(f1_per_class_b)
cat("Random Forest - Pipeline B Macro F1:", round(macro_f1_rf_b, 4), "\n")

