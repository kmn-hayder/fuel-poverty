# model_evaluation_metrics.R

# =========================
# 📦 Load Packages
# =========================
packages <- c("MLmetrics", "caret", "pROC")
installed <- rownames(installed.packages())
for (pkg in packages) {
  if (!pkg %in% installed) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# =========================
# 📊 Evaluation Function
# =========================
evaluate_model <- function(pred, actual, model_name = "Model", pipeline = "A") {
  label_map <- levels(actual)
  
  # Confusion Matrix
  cm <- confusionMatrix(pred, actual)
  print(cm)
  
  # Accuracy
  acc <- Accuracy(y_pred = pred, y_true = actual)
  
  # Macro F1
  f1_per_class <- numeric(length(label_map))
  for (i in seq_along(label_map)) {
    cls <- label_map[i]
    tp <- cm$table[cls, cls]
    fp <- sum(cm$table[, cls]) - tp
    fn <- sum(cm$table[cls, ]) - tp
    precision <- ifelse(tp + fp == 0, 0, tp / (tp + fp))
    recall <- ifelse(tp + fn == 0, 0, tp / (tp + fn))
    f1 <- ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
    f1_per_class[i] <- f1
  }
  macro_f1 <- mean(f1_per_class)
  
  # Sensitivity & Specificity
  sensitivity <- mean(cm$byClass[, "Sensitivity"], na.rm = TRUE)
  specificity <- mean(cm$byClass[, "Specificity"], na.rm = TRUE)
  kappa <- cm$overall["Kappa"]
  
  # ROC AUC
  pred_num <- as.numeric(pred) - 1
  actual_num <- as.numeric(actual) - 1
  roc_auc <- tryCatch({
    multiclass.roc(actual_num, pred_num)$auc
  }, error = function(e) NA)
  
  # Final Summary Output
  cat("\n========================\n")
  cat("📋", model_name, "- Pipeline", pipeline, "\n")
  cat("========================\n")
  cat("Accuracy:", round(acc, 4), "\n")
  cat("Macro F1:", round(macro_f1, 4), "\n")
  cat("Sensitivity:", round(sensitivity, 4), "\n")
  cat("Specificity:", round(specificity, 4), "\n")
  cat("Kappa:", round(kappa, 4), "\n")
  cat("Multiclass ROC AUC:", round(roc_auc, 4), "\n\n")
}
source("evaluation.R")

#==============================
# 📦 Pipeline A: Evaluation Set-Up
#==============================
true_class_a <- test_data$predicted_class  # Ensure this is a factor
#==============================
# 📦 1. XGBoost - Pipeline A
#==============================
evaluate_model(
  pred_class_a,
  test_data$predicted_class,
  model_name = "XGBoost",
  pipeline = "A"
)

#==============================
# 🌲 2. Random Forest - Pipeline A
#==============================
evaluate_model(
  rf_pred_a,
  rf_test_a$predicted_class,
  model_name = "Random Forest",
  pipeline = "A"
)

#==============================
# 📉 3. Logistic Regression - Pipeline A
#==============================
evaluate_model(
  logit_pred_a,
  logit_test_a$predicted_class,
  model_name = "Logistic Regression",
  pipeline = "A"
)

#==============================
# 📦 1. XGBoost - Pipeline B
#==============================
evaluate_model(
  pred_class_b,
  test_data$predicted_class,
  model_name = "XGBoost",
  pipeline = "B"
)

#==============================
# 🌲 2. Random Forest - Pipeline B
#==============================
evaluate_model(
  rf_pred_b,
  rf_test_b$predicted_class,
  model_name = "Random Forest",
  pipeline = "B"
)

#==============================
# 📉 3. Logistic Regression - Pipeline B
#==============================
evaluate_model(
  logit_pred_b,
  logit_test_b$predicted_class,
  model_name = "Logistic Regression",
  pipeline = "B"
)
