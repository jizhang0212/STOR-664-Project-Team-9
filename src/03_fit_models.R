library(car)
library(e1071)
library(randomForest)

source("src/01_load_data.R")


model1 <- lm(sale_price ~ gr_liv_area + overall_qual_encoded + year_built + 
               total_bsmt_sf + full_bath + garage_cars + kitchen_qual_encoded +
               downtown_dist + university_dist + airport_dist,
             data = ames_model_train)

summary(model1)

cat("R-squared:", summary(model1)$r.squared, "\n")
cat("Adjusted R-squared:", summary(model1)$adj.r.squared, "\n")
cat("F-statistic:", summary(model1)$fstatistic[1], "\n")

residuals1 <- resid(model1)

shapiro_result <- shapiro.test(residuals1)

print(shapiro_result)

cat("Skewness:", round(skewness(residuals1), 4), "\n")
cat("Kurtosis:", round(kurtosis(residuals1), 4), "\n")

vif_values <- vif(model1)
cat("\n========== VIF Values ==========\n")
print(vif_values)
cat("\nNote: VIF > 5 indicates potential multicollinearity\n")

predictions1_test <- predict(model1, newdata = ames_model_test)

cat("\n========== Model 1 Test Set Predictions ==========\n")
cat("Number of predictions:", length(predictions1_test), "\n")
cat("Prediction range: $", format(round(min(predictions1_test), 0), big.mark = ","), 
    " - $", format(round(max(predictions1_test), 0), big.mark = ","), "\n")
cat("Mean prediction: $", format(round(mean(predictions1_test), 0), big.mark = ","), "\n")

cat("\n========== Model 2: Log Transformation ==========\n")

ames_model_train_log <- ames_model_train %>%
  mutate(
    log_sale_price = log(sale_price),
    log_gr_liv_area = log(gr_liv_area),
    log_total_bsmt_sf = log(total_bsmt_sf + 1)
  )

ames_model_test_log <- ames_model_test %>%
  mutate(
    log_gr_liv_area = log(gr_liv_area),
    log_total_bsmt_sf = log(total_bsmt_sf + 1)
  )

model2 <- lm(log_sale_price ~ log_gr_liv_area + overall_qual_encoded + year_built + 
               log_total_bsmt_sf + full_bath + garage_cars + kitchen_qual_encoded +
               downtown_dist + university_dist + airport_dist,
             data = ames_model_train_log)

summary(model2)

vif2 <- vif(model2)
print(round(vif2, 2))

residuals2 <- resid(model2)
shapiro2 <- shapiro.test(residuals2)

print(shapiro2)

cat("Skewness:", round(skewness(residuals2), 4), "\n")
cat("Kurtosis:", round(kurtosis(residuals2), 4), "\n")

predictions2_test_log <- predict(model2, newdata = ames_model_test_log)
predictions2_test <- exp(predictions2_test_log)

cat("Number of predictions:", length(predictions2_test), "\n")
cat("Prediction range: $", format(round(min(predictions2_test), 0), big.mark = ","), 
    " - $", format(round(max(predictions2_test), 0), big.mark = ","), "\n")
cat("Mean prediction: $", format(round(mean(predictions2_test), 0), big.mark = ","), "\n")


cooksd <- cooks.distance(model2)

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

std_residuals <- rstandard(model2)
outlier_residuals <- which(abs(std_residuals) > 3)
cat("\nStandardized residuals |z| > 3:", length(outlier_residuals), "\n")

hat_values <- hatvalues(model2)
leverage_threshold <- 2 * (length(coef(model2))) / nrow(ames_model_train_log)
high_leverage <- which(hat_values > leverage_threshold)
cat("High leverage observations:", length(high_leverage), "\n")

all_outliers <- unique(c(influential, high_leverage))
cat("\nCombined outliers (Cook's D or high leverage):", length(all_outliers), "\n")


ames_train_clean <- ames_model_train_log[-influential, ]

cat("Original training set size:", nrow(ames_model_train_log), "\n")
cat("After outlier removal:", nrow(ames_train_clean), "\n")
cat("Removal percentage:", round(length(influential)/nrow(ames_model_train_log)*100, 2), "%\n")

model_clean <- lm(log_sale_price ~ log_gr_liv_area + overall_qual_encoded + year_built + 
                    log_total_bsmt_sf + full_bath + garage_cars + kitchen_qual_encoded +
                    downtown_dist + university_dist + airport_dist,
                  data = ames_train_clean)

summary(model_clean)

vif_clean <- vif(model_clean)
cat("\n========== VIF Values ==========\n")
print(round(vif_clean, 2))

residuals_clean <- resid(model_clean)
shapiro_clean <- shapiro.test(residuals_clean)

print(shapiro_clean)

cat("Skewness:", round(skewness(residuals_clean), 4), "\n")
cat("Kurtosis:", round(kurtosis(residuals_clean), 4), "\n")

predictions_clean_test_log <- predict(model_clean, newdata = ames_model_test_log)
predictions_clean_test <- exp(predictions_clean_test_log)

cat("Number of predictions:", length(predictions_clean_test), "\n")
cat("Prediction range: $", format(round(min(predictions_clean_test), 0), big.mark = ","), 
    " - $", format(round(max(predictions_clean_test), 0), big.mark = ","), "\n")
cat("Mean prediction: $", format(round(mean(predictions_clean_test), 0), big.mark = ","), "\n")


predictions1_train <- predict(model1, ames_model_train)
predictions2_train_log <- predict(model2, ames_model_train_log)
predictions2_train <- exp(predictions2_train_log)

ames_model_train_clean_original <- ames_model_train[-influential, ]
predictions_clean_train_log <- predict(model_clean, ames_train_clean)
predictions_clean_train <- exp(predictions_clean_train_log)

mse1_train <- mean((ames_model_train$sale_price - predictions1_train)^2)
rmse1_train <- sqrt(mse1_train)
mae1_train <- mean(abs(ames_model_train$sale_price - predictions1_train))

mse2_train <- mean((ames_model_train$sale_price - predictions2_train)^2)
rmse2_train <- sqrt(mse2_train)
mae2_train <- mean(abs(ames_model_train$sale_price - predictions2_train))

mse_clean_train <- mean((ames_model_train_clean_original$sale_price - predictions_clean_train)^2)
rmse_clean_train <- sqrt(mse_clean_train)
mae_clean_train <- mean(abs(ames_model_train_clean_original$sale_price - predictions_clean_train))

comparison_train <- data.frame(
  Model = c("Model 1 (Original)", "Model 2 (Log)", "Model 3 (Clean)"),
  Dataset = c("Training", "Training", "Training"),
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


cat("[Prediction Accuracy - R-squared]\n")
cat("Model 1 R-squared =", round(comparison_train$R_squared[1], 4), "\n")
cat("Model 2 R-squared =", round(comparison_train$R_squared[2], 4), "\n")
cat("Model 3 R-squared =", round(comparison_train$R_squared[3], 4), "\n\n")

cat("[Prediction Error - MSE]\n")
cat("Model 1 MSE = $", format(round(comparison_train$MSE[1], 0), big.mark = ","), "^2\n")
cat("Model 2 MSE = $", format(round(comparison_train$MSE[2], 0), big.mark = ","), "^2\n")
cat("Model 3 MSE = $", format(round(comparison_train$MSE[3], 0), big.mark = ","), "^2\n\n")

cat("[Prediction Error - RMSE]\n")
cat("Model 1 RMSE = $", format(round(comparison_train$RMSE[1], 0), big.mark = ","), "\n")
cat("Model 2 RMSE = $", format(round(comparison_train$RMSE[2], 0), big.mark = ","), "\n")
cat("Model 3 RMSE = $", format(round(comparison_train$RMSE[3], 0), big.mark = ","), "\n\n")

cat("[Prediction Error - MAE]\n")
cat("Model 1 MAE = $", format(round(comparison_train$MAE[1], 0), big.mark = ","), "\n")
cat("Model 2 MAE = $", format(round(comparison_train$MAE[2], 0), big.mark = ","), "\n")
cat("Model 3 MAE = $", format(round(comparison_train$MAE[3], 0), big.mark = ","), "\n")


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

print(test_predictions)

cat("\n========== Random Forest Model ==========\n")

rf_train_data <- ames_train_clean %>%
  select(sale_price, gr_liv_area, overall_qual_encoded, year_built,
         total_bsmt_sf, full_bath, garage_cars, kitchen_qual_encoded,
         downtown_dist, university_dist, airport_dist)

rf_test_data <- ames_model_test %>%
  select(gr_liv_area, overall_qual_encoded, year_built,
         total_bsmt_sf, full_bath, garage_cars, kitchen_qual_encoded,
         downtown_dist, university_dist, airport_dist)

cat("Random forest training data size:", nrow(rf_train_data), "\n")
cat("Random forest test data size:", nrow(rf_test_data), "\n")

cat("\nTraining random forest model (may take a few minutes)...\n")
rf_model <- randomForest(
  sale_price ~ .,
  data = rf_train_data,
  ntree = 500,
  mtry = 3,
  importance = TRUE,
  na.action = na.omit
)

print(rf_model)

cat("\n========== Variable Importance ==========\n")
importance_df <- as.data.frame(importance(rf_model))
importance_df$Variable <- rownames(importance_df)
importance_df <- importance_df[order(-importance_df$`%IncMSE`), ]
print(importance_df)

rf_predictions_train <- predict(rf_model, rf_train_data)

rf_mse_train <- mean((rf_train_data$sale_price - rf_predictions_train)^2)
rf_rmse_train <- sqrt(rf_mse_train)
rf_mae_train <- mean(abs(rf_train_data$sale_price - rf_predictions_train))

rf_ss_res <- sum((rf_train_data$sale_price - rf_predictions_train)^2)
rf_ss_tot <- sum((rf_train_data$sale_price - mean(rf_train_data$sale_price))^2)
rf_r_squared_train <- 1 - (rf_ss_res / rf_ss_tot)

cat("R-squared =", round(rf_r_squared_train, 4), "\n")
cat("RMSE = $", format(round(rf_rmse_train, 0), big.mark = ","), "\n")
cat("MAE = $", format(round(rf_mae_train, 0), big.mark = ","), "\n")

rf_predictions_test <- predict(rf_model, rf_test_data)

cat("Number of predictions:", length(rf_predictions_test), "\n")
cat("Prediction range: $", format(round(min(rf_predictions_test), 0), big.mark = ","), 
    " - $", format(round(max(rf_predictions_test), 0), big.mark = ","), "\n")
cat("Mean prediction: $", format(round(mean(rf_predictions_test), 0), big.mark = ","), "\n")
cat("Median prediction: $", format(round(median(rf_predictions_test), 0), big.mark = ","), "\n")

comparison_final <- data.frame(
  Model = c("Model 3 (Linear-Log)", "Random Forest"),
  Training_Sample = c(nrow(ames_train_clean), nrow(rf_train_data)),
  Train_R2 = c(summary(model_clean)$r.squared, rf_r_squared_train),
  Train_RMSE = c(rmse_clean_train, rf_rmse_train),
  Train_MAE = c(mae_clean_train, rf_mae_train),
  Test_Mean_Pred = c(mean(predictions_clean_test), mean(rf_predictions_test)),
  Test_Median_Pred = c(median(predictions_clean_test), median(rf_predictions_test)),
  Test_SD_Pred = c(sd(predictions_clean_test), sd(rf_predictions_test))
)

print(comparison_final)

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

cat("Model 3 RMSE = $", format(round(comparison_final$Train_RMSE[1], 0), big.mark = ","), "\n")
cat("Random Forest RMSE = $", format(round(comparison_final$Train_RMSE[2], 0), big.mark = ","), "\n")

if(comparison_final$Train_RMSE[2] < comparison_final$Train_RMSE[1]) {
  cat("Winner: Random Forest (reduction: $", 
      format(round(comparison_final$Train_RMSE[1] - comparison_final$Train_RMSE[2], 0), big.mark = ","), 
      ")\n\n")
} else {
  cat("Winner: Model 3 (reduction: $", 
      format(round(comparison_final$Train_RMSE[2] - comparison_final$Train_RMSE[1], 0), big.mark = ","), 
      ")\n\n")
}

cat("[Test Set Prediction Comparison]\n")
cat("Model 3 mean prediction = $", format(round(comparison_final$Test_Mean_Pred[1], 0), big.mark = ","), "\n")
cat("Random Forest mean prediction = $", format(round(comparison_final$Test_Mean_Pred[2], 0), big.mark = ","), "\n\n")

cat("Model 3 prediction SD = $", format(round(comparison_final$Test_SD_Pred[1], 0), big.mark = ","), "\n")
cat("Random Forest prediction SD = $", format(round(comparison_final$Test_SD_Pred[2], 0), big.mark = ","), "\n")



cat("[Training Set Performance Ranking]\n")
cat("1. Highest R-squared: ", 
    ifelse(rf_r_squared_train > summary(model_clean)$r.squared, 
           "Random Forest", "Model 3"), "\n")
cat("2. Lowest RMSE: ", 
    ifelse(rf_rmse_train < rmse_clean_train, 
           "Random Forest", "Model 3"), "\n")
cat("3. Lowest MAE: ", 
    ifelse(rf_mae_train < mae_clean_train, 
           "Random Forest", "Model 3"), "\n\n")

cat("[Model Selection Recommendation]\n")
cat("Model 3 (Linear Regression):\n")
cat("- Advantages: High interpretability, satisfies statistical assumptions, clear coefficient meanings\n")
cat("- Best for: Academic research, scenarios requiring variable interpretation\n")
cat("- Training R-squared =", round(summary(model_clean)$r.squared, 4), "\n")
cat("- Training RMSE = $", format(round(rmse_clean_train, 0), big.mark = ","), "\n\n")

cat("Random Forest:\n")
cat("- Advantages: High prediction accuracy, captures nonlinear relationships automatically\n")
cat("- Best for: Pure prediction tasks, Kaggle competitions\n")
cat("- Training R-squared =", round(rf_r_squared_train, 4), "\n")
cat("- Training RMSE = $", format(round(rf_rmse_train, 0), big.mark = ","), "\n\n")
