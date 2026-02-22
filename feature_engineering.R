df_balanced <- df_balanced
library(caret)

# Install FSelector if needed
install.packages("rJava", type = "source")
library(rJava)

install.packages("FSelector")
library(FSelector)

# Make sure target is a factor
df_balanced$predicted_class <- as.factor(df_balanced$predicted_class)

# Compute information gain
ig <- information.gain(predicted_class ~ ., data = df_balanced)

# Sort descending
ig <- ig[order(-ig$attr_importance), , drop = FALSE]

# Show top 20 features
head(ig, 20)

# Optional: Plot top 15
library(ggplot2)

ig$Feature <- rownames(ig)
top_ig <- ig[1:15, ]

ggplot(top_ig, aes(x = reorder(Feature, attr_importance), y = attr_importance)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Information Gain (Top 15 Features)",
       x = "Feature", y = "Information Gain") +
  theme_minimal()

# Ensure target is factor
df_balanced$predicted_class <- as.factor(df_balanced$predicted_class)

# Create stratified split: 80% train, 20% test
set.seed(42)
train_index <- createDataPartition(df_balanced$predicted_class, p = 0.8, list = FALSE)

train_data <- df_balanced[train_index, ]
test_data  <- df_balanced[-train_index, ]

# Check class balance in each
prop.table(table(train_data$predicted_class))
prop.table(table(test_data$predicted_class))
# Make sure target is a factor
train_data$predicted_class <- as.factor(train_data$predicted_class)
test_data$predicted_class  <- as.factor(test_data$predicted_class)