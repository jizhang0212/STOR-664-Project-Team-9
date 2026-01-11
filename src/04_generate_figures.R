# Load required libraries
library(dplyr)
library(randomForest)

# Load processed data
ames_model_train <- readRDS("data/ames_train.rds")
ames_model_test <- readRDS("data/ames_test.rds")

# Load linear model results
linear_outputs <- readRDS("results/linear_model_outputs.rds")
model_clean <- readRDS("results/model3.rds")

# Create results directories
dir.create("results/figures", showWarnings = FALSE)
dir.create("results/tables", showWarnings = FALSE)

# ============================================================
# PREPARE DATA FOR RANDOM FOREST
# ============================================================


# Load cleaned training data (matching Model 3)
# Recreate outlier identification from Model 2
ames_model_train_log <- ames_model_train %>%
  mutate(
    log_sale_price = log(sale_price),
    log_gr_liv_area = log(gr_liv_area),
    log_total_bsmt_sf = log(total_bsmt_sf + 1)
  )

model2_temp <- lm(log_sale_price ~ log_gr_liv_area + overall_qual_encoded + year_built + 
                    log_total_bsmt_sf + full_bath + garage_cars + kitchen_qual_encoded +
                    downtown_dist + university_dist + airport_dist,
                  data = ames_model_train_log)

cooksd_temp <- cooks.distance(model2_temp)
threshold_temp <- 4/nrow(ames_model_train_log)
influential <- which(cooksd_temp > threshold_temp)

# Prepare Random Forest training data (same observations as Model 3)
rf_train_data <- ames_model_train[-influential, ] %>%
  select(sale_price, gr_liv_area, overall_qual_encoded, year_built,
         total_bsmt_sf, full_bath, garage_cars, kitchen_qual_encoded,
         downtown_dist, university_dist, airport_dist)

# Prepare Random Forest test data
rf_test_data <- ames_model_test %>%
  select(gr_liv_area, overall_qual_encoded, year_built,
         total_bsmt_sf, full_bath, garage_cars, kitchen_qual_encoded,
         downtown_dist, university_dist, airport_dist)

cat("Random forest training data size:", nrow(rf_train_data), "\n")
cat("Random forest test data size:", nrow(rf_test_data), "\n")

# ============================================================
# FIT RANDOM FOREST MODEL
# ============================================================

cat("\nTraining random forest model (may take a few minutes)...\n")

# Fit Random Forest with 500 trees and mtry=3
set.seed(123)
rf_model <- randomForest(
  sale_price ~ .,
  data = rf_train_data,
  ntree = 500,
  mtry = 3,  # sqrt(10) ≈ 3
  importance = TRUE,
  na.action = na.omit
)

print(rf_model)

# ============================================================
# VARIABLE IMPORTANCE
# ============================================================

cat("\n========== Variable Importance ==========\n")

# Extract and sort variable importance
importance_df <- as.data.frame(importance(rf_model))
importance_df$Variable <- rownames(importance_df)
importance_df <- importance_df[order(-importance_df$`%IncMSE`), ]
print(importance_df)

# Save variable importance table
write.csv(importance_df, "results/tables/rf_variable_importance.csv", row.names = FALSE)

# Plot variable importance
png("results/figures/rf_variable_importance.png", width = 800, height = 600)
varImpPlot(rf_model, main = "Variable Importance Plot")
dev.off()

# ============================================================
# TRAINING SET PERFORMANCE
# ============================================================

# Generate training set predictions
rf_predictions_train <- predict(rf_model, rf_train_data)

# Calculate training set metrics
rf_mse_train <- mean((rf_train_data$sale_price - rf_predictions_train)^2)
rf_rmse_train <- sqrt(rf_mse_train)
rf_mae_train <- mean(abs(rf_train_data$sale_price - rf_predictions_train))

# Calculate R-squared
rf_ss_res <- sum((rf_train_data$sale_price - rf_predictions_train)^2)
rf_ss_tot <- sum((rf_train_data$sale_price - mean(rf_train_data$sale_price))^2)
rf_r_squared_train <- 1 - (rf_ss_res / rf_ss_tot)

cat("\n========== Random Forest Training Set Performance ==========\n")
cat("R-squared =", round(rf_r_squared_train, 4), "\n")
cat("RMSE = $", format(round(rf_rmse_train, 0), big.mark = ","), "\n")
cat("MAE = $", format(round(rf_mae_train, 0), big.mark = ","), "\n")

# ============================================================
# TEST SET PREDICTIONS
# ============================================================

# Generate test set predictions
rf_predictions_test <- predict(rf_model, rf_test_data)

cat("\n========== Random Forest Test Set Predictions ==========\n")
cat("Number of predictions:", length(rf_predictions_test), "\n")
cat("Prediction range: $", format(round(min(rf_predictions_test), 0), big.mark = ","), 
    " - $", format(round(max(rf_predictions_test), 0), big.mark = ","), "\n")
cat("Mean prediction: $", format(round(mean(rf_predictions_test), 0), big.mark = ","), "\n")
cat("Median prediction: $", format(round(median(rf_predictions_test), 0), big.mark = ","), "\n")

# ============================================================
# MODEL 3 VS RANDOM FOREST COMPARISON
# ============================================================

cat("\n========== Model 3 (Linear Regression) vs Random Forest Comparison ==========\n\n")

# Extract Model 3 metrics
model3_train_metrics <- readRDS("results/linear_model_outputs.rds")

# Calculate Model 3 training metrics
ames_model_train_clean_original <- ames_model_train[-influential, ]
ames_train_clean_log <- ames_model_train_log[-influential, ]

predictions_clean_train_log <- predict(model_clean, ames_train_clean_log)
predictions_clean_train <- exp(predictions_clean_train_log)

mse_clean_train <- mean((ames_model_train_clean_original$sale_price - predictions_clean_train)^2)
rmse_clean_train <- sqrt(mse_clean_train)
mae_clean_train <- mean(abs(ames_model_train_clean_original$sale_price - predictions_clean_train))

predictions_clean_test <- linear_outputs$predictions_clean_test

# Create comparison table
comparison_final <- data.frame(
  Model = c("Model 3 (Linear-Log)", "Random Forest"),
  Training_Sample = c(nrow(rf_train_data), nrow(rf_train_data)),
  Train_R2 = c(summary(model_clean)$r.squared, rf_r_squared_train),
  Train_RMSE = c(rmse_clean_train, rf_rmse_train),
  Train_MAE = c(mae_clean_train, rf_mae_train),
  Test_Mean_Pred = c(mean(predictions_clean_test), mean(rf_predictions_test)),
  Test_Median_Pred = c(median(predictions_clean_test), median(rf_predictions_test)),
  Test_SD_Pred = c(sd(predictions_clean_test), sd(rf_predictions_test))
)

print(comparison_final)

# Save comparison table
write.csv(comparison_final, "results/tables/model3_vs_rf_comparison.csv", row.names = FALSE)

# Print formatted comparison
cat("\n[Training Set Performance Comparison]\n")
cat("Model 3 R-squared =", round(comparison_final$Train_R2[1], 4), "\n")
cat("Random Forest R-squared =", round(comparison_final$Train_R2[2], 4), "\n")

if(comparison_final$Train_R2[2] > comparison_final$Train_R2[1]) {
  cat("Winner: Random Forest (+", 
      round((comparison_final$Train_R2[2] - comparison_final$Train_R2[1]) * 100, 2), 
      "%)\n\n")
} else {
  cat("Winner: Model 3 (+", 
      round((comparison_final$Train_R2[1] - comparison_final$Train_R2[2]) * 100, 2), 
      "%)\n\n")
}

# Save Random Forest model and predictions
saveRDS(rf_model, "results/rf_model.rds")
saveRDS(list(
  rf_predictions_train = rf_predictions_train,
  rf_predictions_test = rf_predictions_test
), "results/rf_predictions.rds")

cat("\nRandom Forest results saved to results/ directory\n")
