library(xgboost)
library(Matrix)
library(caret)
library(MLmetrics)

# Convert class to numeric labels for XGBoost (0-indexed)
label_map <- levels(train_data$predicted_class)
train_label <- as.numeric(train_data$predicted_class) - 1
test_label  <- as.numeric(test_data$predicted_class) - 1

# Define feature sets
pipeline_a_vars <- setdiff(names(train_data), "predicted_class")  # includes efficiency
pipeline_b_vars <- setdiff(pipeline_a_vars, "current_energy_efficiency")

# Add high-correlation vars back into B
pipeline_b_vars <- unique(c(pipeline_b_vars,
                            "energy_consumption_current",
                            "co2_per_m2",
                            "energy_per_m2"))

# Ensure no missing
train_a <- train_data[, c(pipeline_a_vars, "predicted_class")]
train_b <- train_data[, c(pipeline_b_vars, "predicted_class")]

test_a  <- test_data[, c(pipeline_a_vars, "predicted_class")]
test_b  <- test_data[, c(pipeline_b_vars, "predicted_class")]


# Create DMatrix for Pipeline A
dtrain_a <- xgb.DMatrix(data = as.matrix(train_a[ , -ncol(train_a)]), label = train_label)
dtest_a  <- xgb.DMatrix(data = as.matrix(test_a[ , -ncol(test_a)]), label = test_label)

# Create DMatrix for Pipeline B
dtrain_b <- xgb.DMatrix(data = as.matrix(train_b[ , -ncol(train_b)]), label = train_label)
dtest_b  <- xgb.DMatrix(data = as.matrix(test_b[ , -ncol(test_b)]), label = test_label)


#======== Train XGBoost Model for Pipeline A ========
set.seed(42)
xgb_model_a <- xgb.train(
  params = list(
    objective = "multi:softprob",
    num_class = length(label_map),
    eval_metric = "mlogloss",
    max_depth = 6,
    eta = 0.1,
    subsample = 0.8,
    colsample_bytree = 0.8
  ),
  data = dtrain_a,
  nrounds = 150,
  watchlist = list(train = dtrain_a, test = dtest_a),
  early_stopping_rounds = 10,
  verbose = 1
)

#======== Train XGBoost Model for Pipeline B ========
set.seed(42)
xgb_model_b <- xgb.train(
  params = list(
    objective = "multi:softprob",
    num_class = length(label_map),
    eval_metric = "mlogloss",
    max_depth = 6,
    eta = 0.1,
    subsample = 0.8,
    colsample_bytree = 0.8
  ),
  data = dtrain_b,
  nrounds = 150,
  watchlist = list(train = dtrain_b, test = dtest_b),
  early_stopping_rounds = 10,
  verbose = 1
)
#======== Evaluate Pipeline A on Test Set ========

# Predict probabilities
pred_prob_a <- predict(xgb_model_a, dtest_a)

# Reshape to class probabilities
pred_prob_matrix_a <- matrix(pred_prob_a, ncol = length(label_map), byrow = TRUE)

# Get predicted class index (1-based)
pred_class_index_a <- max.col(pred_prob_matrix_a)

# Convert to factor with original class labels
pred_class_a <- factor(label_map[pred_class_index_a], levels = label_map)

# True labels
true_class <- test_data$predicted_class

# Confusion matrix
cm_a <- confusionMatrix(pred_class_a, true_class)
print(cm_a)

# Accuracy
accuracy_a <- Accuracy(y_pred = pred_class_a, y_true = true_class)
cat("\nAccuracy:", round(accuracy_a, 4), "\n")

