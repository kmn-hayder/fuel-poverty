# ======================
# 1. Load Libraries
# ======================
library(keras)
library(tensorflow)
library(dplyr)
library(caret)

# Use correct environment (adjust if needed)
# reticulate::use_condaenv("r-reticulate", required = TRUE)

# ======================
# 2. Prepare Data
# ======================

# Assume train_data and test_data are already available
# And target variable is `predicted_class`

# Convert factor to numeric labels (0-based)
train_label <- as.numeric(train_data$predicted_class) - 1
test_label  <- as.numeric(test_data$predicted_class) - 1

# One-hot encode labels
y_train_cat <- to_categorical(train_label)
y_test_cat  <- to_categorical(test_label)
num_classes <- ncol(y_train_cat)

# Select top 20 features (based on earlier SHAP/info gain results)
top_features <- c(
  "current_energy_efficiency", "insulation_score", "co2_emissions_current",
  "fuel_bill_individual_est", "fuel_cost_ratio", "fuel_bill_local_avg_est",
  "elec_price_kwh", "gas_avg_fixed_cost_annual", "energy_per_room",
  "electricity_avg_fixed_cost_annual", "log_energy_per_room", "income_bhc_2024",
  "area_km", "energy_per_m2", "population_density", "senior_population",
  "built_form_Mid-Terrace", "built_form_Semi-Detached"
)

# Filter features
X_train_a <- as.matrix(train_data[, top_features])
X_test_a  <- as.matrix(test_data[, top_features])

# Normalize inputs
X_train_a <- scale(X_train_a)
X_test_a  <- scale(X_test_a)

# ======================
# 3. Build DNN Model
# ======================
build_and_train_model <- function(X_train, y_train, X_test, y_test) {
  model <- keras_model_sequential() %>%
    layer_dense(units = 128, activation = "relu", input_shape = ncol(X_train)) %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = num_classes, activation = "softmax")
  
  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_adam(learning_rate = 0.001),
    metrics = "accuracy"
  )
  
  history <- model %>% fit(
    X_train, y_train,
    epochs = 100,
    batch_size = 64,
    validation_split = 0.2,
    verbose = 1
  )
  
  # Predict on test set
  probs <- model %>% predict(X_test)
  pred_classes <- apply(probs, 1, which.max) - 1  # zero-indexed to match label
  acc <- mean(pred_classes == test_label)
  
  return(list(model = model, history = history, accuracy = acc))
}

# ======================
# 4. Train and Evaluate
# ======================
nn_result_a <- build_and_train_model(X_train_a, y_train_cat, X_test_a, y_test_cat)

cat("✅ Neural Net - Pipeline A Accuracy:", round(nn_result_a$accuracy, 4), "\n")

#PIPELINE B
# ======================
# 1. Load Libraries
# ======================
library(keras)
library(tensorflow)
library(dplyr)
library(caret)

# ======================
# 2. Prepare Data for Pipeline B
# ======================

# Same target transformation as Pipeline A
test_label  <- as.numeric(test_data$predicted_class) - 1
train_label <- as.numeric(train_data$predicted_class) - 1

# One-hot encode target labels
y_train_cat <- to_categorical(train_label)
y_test_cat  <- to_categorical(test_label)
num_classes <- ncol(y_train_cat)

# Define Pipeline B features:
top_features_b <- c(
  "energy_consumption_current", "co2_per_m2", "energy_per_m2",
  "insulation_score", "fuel_bill_individual_est", "fuel_cost_ratio",
  "fuel_bill_local_avg_est", "elec_price_kwh", "gas_avg_fixed_cost_annual",
  "energy_per_room", "electricity_avg_fixed_cost_annual", "log_energy_per_room",
  "income_bhc_2024", "area_km", "population_density", "senior_population",
  "built_form_Mid-Terrace", "built_form_Semi-Detached"
)

# Subset and convert to matrix
X_train_b <- as.matrix(train_data[, top_features_b])
X_test_b  <- as.matrix(test_data[, top_features_b])

# Normalize
X_train_b <- scale(X_train_b)
X_test_b  <- scale(X_test_b)

# ======================
# 3. Build DNN Model
# ======================
build_and_train_model <- function(X_train, y_train, X_test, y_test) {
  model <- keras_model_sequential() %>%
    layer_dense(units = 128, activation = "relu", input_shape = ncol(X_train)) %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = num_classes, activation = "softmax")
  
  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_adam(learning_rate = 0.001),
    metrics = "accuracy"
  )
  
  history <- model %>% fit(
    X_train, y_train,
    epochs = 100,
    batch_size = 64,
    validation_split = 0.2,
    verbose = 1
  )
  
  # Evaluate
  probs <- model %>% predict(X_test)
  pred_classes <- apply(probs, 1, which.max) - 1
  acc <- mean(pred_classes == test_label)
  
  return(list(model = model, history = history, accuracy = acc))
}

# ======================
# 4. Train and Evaluate
# ======================
nn_result_b <- build_and_train_model(X_train_b, y_train_cat, X_test_b, y_test_cat)

cat("✅ Neural Net - Pipeline B Accuracy:", round(nn_result_b$accuracy, 4), "\n")
