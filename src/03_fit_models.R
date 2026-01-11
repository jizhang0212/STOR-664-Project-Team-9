# Load required libraries
library(dplyr)
library(car)
library(e1071)

# Load processed data
ames_model_train <- readRDS("data/ames_train.rds")
ames_model_test <- readRDS("data/ames_test.rds")

# Create results directories
dir.create("results", showWarnings = FALSE)
dir.create("results/figures", showWarnings = FALSE)
dir.create("results/tables", showWarnings = FALSE)

# ============================================================
# MODEL 1: Original Scale Linear Regression
# ============================================================

cat("\n========== Model 1: Original Scale ==========\n")

# Fit linear model on original scale
model1 <- lm(sale_price ~ gr_liv_area + overall_qual_encoded + year_built + 
               total_bsmt_sf + full_bath + garage_cars + kitchen_qual_encoded +
               downtown_dist + university_dist + airport_dist,
             data = ames_model_train)

# Print model summary
summary(model1)

# Extract and save model performance metrics
cat("\n========== Model 1 Results (Training Set) ==========\n")
cat("R-squared:", summary(model1)$r.squared, "\n")
cat("Adjusted R-squared:", summary(model1)$adj.r.squared, "\n")
cat("F-statistic:", summary(model1)$fstatistic[1], "\n")

# Test residual normality
residuals1 <- resid(model1)
shapiro_result <- shapiro.test(residuals1)

cat("\n========== Normality Test ==========\n")
print(shapiro_result)
cat("Skewness:", round(skewness(residuals1), 4), "\n")
cat("Kurtosis:", round(kurtosis(residuals1), 4), "\n")

# Check multicollinearity
vif_values <- vif(model1)
cat("\n========== VIF Values ==========\n")
print(vif_values)
cat("\nNote: VIF > 5 indicates potential multicollinearity\n")

# Generate diagnostic plots
png("results/figures/model1_diagnostics.png", width = 1200, height = 1200)
par(mfrow = c(2, 2))
plot(model1)
par(mfrow = c(1, 1))
dev.off()

# Plot residual distribution
png("results/figures/model1_residuals.png", width = 800, height = 600)
hist(residuals1, breaks = 50, 
     main = "Distribution of Residuals (Model 1)",
     xlab = "Residuals", col = "lightblue", freq = FALSE)
curve(dnorm(x, mean = mean(residuals1), sd = sd(residuals1)),
      add = TRUE, col = "red", lwd = 2)
dev.off()

# Test set predictions
predictions1_test <- predict(model1, newdata = ames_model_test)

cat("\n========== Model 1 Test Set Predictions ==========\n")
cat("Number of predictions:", length(predictions1_test), "\n")
cat("Prediction range: $", format(round(min(predictions1_test), 0), big.mark = ","), 
    " - $", format(round(max(predictions1_test), 0), big.mark = ","), "\n")
cat("Mean prediction: $", format(round(mean(predictions1_test), 0), big.mark = ","), "\n")

# ============================================================
# MODEL 2: Log-Transformed Linear Regression
# ============================================================

cat("\n========== Model 2: Log Transformation ==========\n")

# Create log-transformed variables for training set
ames_model_train_log <- ames_model_train %>%
  mutate(
    log_sale_price = log(sale_price),
    log_gr_liv_area = log(gr_liv_area),
    log_total_bsmt_sf = log(total_bsmt_sf + 1)  # Add 1 to handle zeros
  )

# Create log-transformed variables for test set
ames_model_test_log <- ames_model_test %>%
  mutate(
    log_gr_liv_area = log(gr_liv_area),
    log_total_bsmt_sf = log(total_bsmt_sf + 1)
  )

# Fit log-transformed model
model2 <- lm(log_sale_price ~ log_gr_liv_area + overall_qual_encoded + year_built + 
               log_total_bsmt_sf + full_bath + garage_cars + kitchen_qual_encoded +
               downtown_dist + university_dist + airport_dist,
             data = ames_model_train_log)

# Print model summary
summary(model2)

# Check VIF
vif2 <- vif(model2)
cat("\n========== VIF Values ==========\n")
print(round(vif2, 2))

# Test residual normality
residuals2 <- resid(model2)
shapiro2 <- shapiro.test(residuals2)

cat("\n========== Normality Test ==========\n")
print(shapiro2)
cat("Skewness:", round(skewness(residuals2), 4), "\n")
cat("Kurtosis:", round(kurtosis(residuals2), 4), "\n")

# Generate diagnostic plots
png("results/figures/model2_diagnostics.png", width = 1200, height = 1200)
par(mfrow = c(2, 2))
plot(model2)
par(mfrow = c(1, 1))
dev.off()

# Plot residual distribution
png("results/figures/model2_residuals.png", width = 800, height = 600)
hist(residuals2, breaks = 50, freq = FALSE,
     main = "Distribution of Residuals (Model 2 - Log)",
     xlab = "Residuals", col = "lightgreen")
curve(dnorm(x, mean = mean(residuals2), sd = sd(residuals2)),
      add = TRUE, col = "red", lwd = 2)
dev.off()

# Test set predictions (convert back to original scale)
predictions2_test_log <- predict(model2, newdata = ames_model_test_log)
predictions2_test <- exp(predictions2_test_log)

cat("\n========== Model 2 Test Set Predictions ==========\n")
cat("Number of predictions:", length(predictions2_test), "\n")
cat("Prediction range: $", format(round(min(predictions2_test), 0), big.mark = ","), 
    " - $", format(round(max(predictions2_test), 0), big.mark = ","), "\n")
cat("Mean prediction: $", format(round(mean(predictions2_test), 0), big.mark = ","), "\n")

# ============================================================
# OUTLIER DETECTION
# ============================================================

cat("\n========== Outlier Detection ==========\n")

# Calculate Cook's Distance
cooksd <- cooks.distance(model2)

# Plot Cook's Distance
png("results/figures/cooks_distance.png", width = 1000, height = 600)
plot(cooksd, pch = 20, main = "Cook's Distance",
     ylab = "Cook's Distance", xlab = "Observation Index")
abline(h = 4/nrow(ames_model_train_log), col = "red", lty = 2)
dev.off()

# Identify influential observations
threshold <- 4/nrow(ames_model_train_log)
influential <- which(cooksd > threshold)

cat("Cook's Distance threshold:", round(threshold, 5), "\n")
cat("Number of influential outliers:", length(influential), "\n")

if (length(influential) > 0) {
  cat("Observation indices:", head(influential, 20), "...\n")
  
  cat("\nOutlier price statistics:\n")
  print(summary(ames_model_train$sale_price[influential]))
  
  cat("\nNormal observation price statistics:\n")
  print(summary(ames_model_train$sale_price[-influential]))
}

# Additional outlier diagnostics
std_residuals <- rstandard(model2)
outlier_residuals <- which(abs(std_residuals) > 3)
cat("\nStandardized residuals |z| > 3:", length(outlier_residuals), "\n")

hat_values <- hatvalues(model2)
leverage_threshold <- 2 * (length(coef(model2))) / nrow(ames_model_train_log)
high_leverage <- which(hat_values > leverage_threshold)
cat("High leverage observations:", length(high_leverage), "\n")

all_outliers <- unique(c(influential, high_leverage))
cat("\nCombined outliers (Cook's D or high leverage):", length(all_outliers), "\n")

# ============================================================
# MODEL 3: Outlier-Removed Linear Regression
# ============================================================

cat("\n========== Model 3: Outliers Removed ==========\n")

# Remove influential outliers
ames_train_clean <- ames_model_train_log[-influential, ]

cat("Original training set size:", nrow(ames_model_train_log), "\n")
cat("After outlier removal:", nrow(ames_train_clean), "\n")
cat("Removal percentage:", round(length(influential)/nrow(ames_model_train_log)*100, 2), "%\n")

# Refit model on cleaned data
model_clean <- lm(log_sale_price ~ log_gr_liv_area + overall_qual_encoded + year_built + 
                    log_total_bsmt_sf + full_bath + garage_cars + kitchen_qual_encoded +
                    downtown_dist + university_dist + airport_dist,
                  data = ames_train_clean)

# Print model summary
summary(model_clean)

# Check VIF
vif_clean <- vif(model_clean)
cat("\n========== VIF Values ==========\n")
print(round(vif_clean, 2))

# Test residual normality
residuals_clean <- resid(model_clean)
shapiro_clean <- shapiro.test(residuals_clean)

cat("\n========== Normality Test ==========\n")
print(shapiro_clean)
cat("Skewness:", round(skewness(residuals_clean), 4), "\n")
cat("Kurtosis:", round(kurtosis(residuals_clean), 4), "\n")

# Generate diagnostic plots
png("results/figures/model3_diagnostics.png", width = 1200, height = 1200)
par(mfrow = c(2, 2))
plot(model_clean)
par(mfrow = c(1, 1))
dev.off()

# Plot residual distribution
png("results/figures/model3_residuals.png", width = 800, height = 600)
hist(residuals_clean, breaks = 50, freq = FALSE,
     main = "Distribution of Residuals (Model 3 - Clean)",
     xlab = "Residuals", col = "lightcyan")
curve(dnorm(x, mean = mean(residuals_clean), sd = sd(residuals_clean)),
      add = TRUE, col = "red", lwd = 2)
dev.off()

# Test set predictions
predictions_clean_test_log <- predict(model_clean, newdata = ames_model_test_log)
predictions_clean_test <- exp(predictions_clean_test_log)

cat("\n========== Model 3 Test Set Predictions ==========\n")
cat("Number of predictions:", length(predictions_clean_test), "\n")
cat("Prediction range: $", format(round(min(predictions_clean_test), 0), big.mark = ","), 
    " - $", format(round(max(predictions_clean_test), 0), big.mark = ","), "\n")
cat("Mean prediction: $", format(round(mean(predictions_clean_test), 0), big.mark = ","), "\n")

# ============================================================
# MODEL COMPARISON
# ============================================================

cat("\n========== Three-Model Comparison (Training Set Performance) ==========\n")

# Calculate training set predictions for all models
predictions1_train <- predict(model1, ames_model_train)
predictions2_train_log <- predict(model2, ames_model_train_log)
predictions2_train <- exp(predictions2_train_log)

ames_model_train_clean_original <- ames_model_train[-influential, ]
predictions_clean_train_log <- predict(model_clean, ames_train_clean)
predictions_clean_train <- exp(predictions_clean_train_log)

# Calculate performance metrics
mse1_train <- mean((ames_model_train$sale_price - predictions1_train)^2)
rmse1_train <- sqrt(mse1_train)
mae1_train <- mean(abs(ames_model_train$sale_price - predictions1_train))

mse2_train <- mean((ames_model_train$sale_price - predictions2_train)^2)
rmse2_train <- sqrt(mse2_train)
mae2_train <- mean(abs(ames_model_train$sale_price - predictions2_train))

mse_clean_train <- mean((ames_model_train_clean_original$sale_price - predictions_clean_train)^2)
rmse_clean_train <- sqrt(mse_clean_train)
mae_clean_train <- mean(abs(ames_model_train_clean_original$sale_price - predictions_clean_train))

# Create comparison table
comparison_train <- data.frame(
  Model = c("Model 1 (Original)", "Model 2 (Log)", "Model 3 (Clean)"),
  Sample_Size = c(nrow(ames_model_train), 
                  nrow(ames_model_train_log), 
                  nrow(ames_train_clean)),
  R_squared = c(summary(model1)$r.squared,
                summary(model2)$r.squared,
                summary(model_clean)$r.squared),
  Adj_R_squared = c(summary(model1)$adj.r.squared,
                    summary(model2)$adj.r.squared,
                    summary(model_clean)$adj.r.squared),
  MSE = c(mse1_train, mse2_train, mse_clean_train),
  RMSE = c(rmse1_train, rmse2_train, rmse_clean_train),
  MAE = c(mae1_train, mae2_train, mae_clean_train),
  Max_VIF = c(max(vif_values), max(vif2), max(vif_clean)),
  Shapiro_W = c(shapiro_result$statistic,
                shapiro2$statistic,
                shapiro_clean$statistic)
)

print(comparison_train)

# Save comparison table
write.csv(comparison_train, "results/tables/model_comparison.csv", row.names = FALSE)

# Save test set predictions
test_predictions <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3"),
  Min_Price = c(min(predictions1_test), 
                min(predictions2_test), 
                min(predictions_clean_test)),
  Mean_Price = c(mean(predictions1_test), 
                 mean(predictions2_test), 
                 mean(predictions_clean_test)),
  Median_Price = c(median(predictions1_test), 
                   median(predictions2_test), 
                   median(predictions_clean_test)),
  Max_Price = c(max(predictions1_test), 
                max(predictions2_test), 
                max(predictions_clean_test)),
  SD_Price = c(sd(predictions1_test), 
               sd(predictions2_test), 
               sd(predictions_clean_test))
)

write.csv(test_predictions, "results/tables/test_predictions.csv", row.names = FALSE)

# Save model objects
saveRDS(model1, "results/model1.rds")
saveRDS(model2, "results/model2.rds")
saveRDS(model_clean, "results/model3.rds")

# Save predictions for use in next script
saveRDS(list(
  predictions1_test = predictions1_test,
  predictions2_test = predictions2_test,
  predictions_clean_test = predictions_clean_test,
  residuals1 = residuals1,
  residuals2 = residuals2,
  residuals_clean = residuals_clean
), "results/linear_model_outputs.rds")

cat("\nResults saved to results/ directory\n")
