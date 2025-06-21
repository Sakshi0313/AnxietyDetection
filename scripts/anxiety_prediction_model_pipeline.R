# Load necessary libraries
library(dplyr)
library(randomForest)
library(caret)
library(ggplot2)
library(reshape2)
library(FNN)
library(corrplot)

# ===========================
# 1. Load & Preprocess Data
# ===========================

# Read dataset
df <- read.csv("D:/VIT/SEM 4/Data Science/CP/Shiny/gaming_anxiety_prediction/data/cleaned_gaming_dataset_final.csv")

# Drop Anxiety_Type temporarily for correlation analysis
df_cleaned <- df %>% select(-Anxiety_Type)

# Convert categorical variables to numeric
df_cleaned$whyplay <- as.numeric(factor(df_cleaned$whyplay))
df_cleaned$Gender <- as.numeric(factor(df_cleaned$Gender))
df_cleaned$Work <- as.numeric(factor(df_cleaned$Work))
df_cleaned$Degree <- as.numeric(factor(df_cleaned$Degree))

# Compute correlation matrix
cor_matrix <- cor(df_cleaned, use = "complete.obs")
print(cor_matrix)

# Visualize correlation using a heatmap
melted_cor <- melt(cor_matrix)
ggplot(data = melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0) +
  labs(title = "Correlation Heatmap", x = "Features", y = "Features") +
  theme_minimal()

# Alternative: Use corrplot
corrplot(cor_matrix, method = "color", type = "lower", tl.col = "black", tl.srt = 45)

# Restore Anxiety_Type column
df_cleaned$Anxiety_Type <- ifelse(
  df$GAD_T >= 10 & df$SPIN_T >= 19, "Mixed_Anxiety",
  ifelse(df$GAD_T >= 10, "GAD",
         ifelse(df$SPIN_T >= 19, "SAD",
                "No_Anxiety")))

# Convert to factor
df_cleaned$Anxiety_Type <- factor(df_cleaned$Anxiety_Type)

# Save cleaned dataset
write.csv(df_cleaned, "D:/VIT/SEM 4/Data Science/CP/Shiny/gaming_anxiety_prediction/data/cleaned_gaming_dataset_final.csv", row.names = FALSE)

# ===========================
# 2. Apply Synthetic Sampling (SMOTE)
# ===========================

manual_SMOTE <- function(data, target_col, class_label, N = 100, k = 5) {
  minority_samples <- data[data[[target_col]] == class_label, ]
  features <- data[, !names(data) %in% target_col]
  minority_features <- minority_samples[, !names(minority_samples) %in% target_col]
  
  knn_result <- get.knnx(data = minority_features, query = minority_features, k = k + 1)
  synthetic_samples <- data.frame()
  
  for (i in 1:nrow(minority_features)) {
    for (j in 2:(k + 1)) {  
      neighbor_idx <- knn_result$nn.index[i, j]
      neighbor <- minority_features[neighbor_idx, ]
      
      gap <- runif(1)
      new_sample <- minority_features[i, ] + gap * (neighbor - minority_features[i, ])
      
      new_sample[target_col] <- class_label  
      synthetic_samples <- rbind(synthetic_samples, new_sample)
      
      if (nrow(synthetic_samples) >= (N / 100) * nrow(minority_samples)) break
    }
  }
  
  balanced_data <- rbind(data, synthetic_samples)
  return(balanced_data)
}

df_balanced <- df_cleaned

df_balanced$whyplay <- as.numeric(as.factor(df_balanced$whyplay))
df_balanced$Gender <- as.numeric(as.factor(df_balanced$Gender))
df_balanced$Work <- as.numeric(as.factor(df_balanced$Work))
df_balanced$Degree <- as.numeric(as.factor(df_balanced$Degree))
df_balanced$Anxiety_Type <- as.numeric(as.factor(df_balanced$Anxiety_Type))

set.seed(123)

df_balanced <- manual_SMOTE(df_balanced, target_col = "Anxiety_Type", class_label = 1, N = 200, k = 5)
df_balanced <- manual_SMOTE(df_balanced, target_col = "Anxiety_Type", class_label = 2, N = 100, k = 5)

table(df_balanced$Anxiety_Type) 



df$Anxiety_Type <- as.factor(df$Anxiety_Type)

# Remove highly correlated features (GAD_T and SPIN_T are usually correlated)
df <- df %>% select(-SWL_T)

# Split data into training (70%) and testing (30%)
set.seed(42)
train_index <- createDataPartition(df$Anxiety_Type, p = 0.7, list = FALSE)
train_data <- df[train_index, ]
test_data <- df[-train_index, ]

# Add slight noise to numerical features to avoid overfitting
num_cols <- sapply(train_data, is.numeric)
noise_level <- 0.01 
train_data[num_cols] <- train_data[num_cols] + 
  rnorm(n = sum(num_cols) * nrow(train_data), mean = 0, sd = noise_level)

# Train Random Forest Model with controlled complexity
rf_model <- randomForest(
  Anxiety_Type ~ ., 
  data = train_data, 
  ntree = 80,      # Limit number of trees
  mtry = 3,        # Limit features per split
  nodesize = 5,    # Increase node size to prevent overfitting
  importance = TRUE
)

# Model Evaluation
predictions <- predict(rf_model, newdata = test_data)
conf_matrix <- confusionMatrix(predictions, test_data$Anxiety_Type)

# Print results
print(conf_matrix)

# Calculate Accuracy
accuracy <- sum(diag(conf_matrix$table)) / sum(conf_matrix$table)
cat("\nFinal Model Accuracy:", round(accuracy * 100, 2), "%\n")


conf_matrix <- confusionMatrix(predictions, test_data$Anxiety_Type)

# Print Confusion Matrix
print(conf_matrix)

# Convert to a table for better visualization
conf_table <- as.table(conf_matrix$table)

# Display as a heatmap using ggplot2
library(ggplot2)

conf_df <- as.data.frame(conf_table)
colnames(conf_df) <- c("Actual", "Predicted", "Freq")

ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Freq), color = "black", size = 6) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Confusion Matrix Heatmap", x = "Actual", y = "Predicted") +
  theme_minimal()




train_predictions <- predict(rf_model, newdata = train_data)
train_conf_matrix <- confusionMatrix(train_predictions, train_data$Anxiety_Type)

# Training Accuracy
train_accuracy <- sum(diag(train_conf_matrix$table)) / sum(train_conf_matrix$table)

# Testing Accuracy (Already Computed)
test_accuracy <- sum(diag(conf_matrix$table)) / sum(conf_matrix$table)


accuracy_df <- data.frame(
  Dataset = c("Training", "Testing"),
  Accuracy = c(train_accuracy, test_accuracy)
)

library(ggplot2)

ggplot(accuracy_df, aes(x = Dataset, y = Accuracy, fill = Dataset)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(round(Accuracy * 100, 2), "%")), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(title = "Training vs Testing Accuracy", x = "Dataset", y = "Accuracy") +
  ylim(0, 1) +
  theme_minimal()

