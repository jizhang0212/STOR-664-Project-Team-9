library(ggplot2)
library(tidyr)
library(scales)

source("src/03_fit_models.R")

par(mfrow = c(2, 2))
plot(model1)
par(mfrow = c(1, 1))

hist(residuals1, breaks = 50, 
     main = "Distribution of Residuals (Model 1)",
     xlab = "Residuals", col = "lightblue", freq = FALSE)
curve(dnorm(x, mean = mean(residuals1), sd = sd(residuals1)),
      add = TRUE, col = "red", lwd = 2)

qqnorm(residuals1, main = "Normal Q-Q Plot (Model 1)")
qqline(residuals1, col = "red", lwd = 2)

par(mfrow = c(2, 2))
plot(model2)
par(mfrow = c(1, 1))

qqnorm(residuals2, main = "Normal Q-Q Plot (Model 2 - Log)")
qqline(residuals2, col = "red", lwd = 2)

hist(residuals2, breaks = 50, freq = FALSE,
     main = "Distribution of Residuals (Model 2 - Log)",
     xlab = "Residuals", col = "lightgreen")
curve(dnorm(x, mean = mean(residuals2), sd = sd(residuals2)),
      add = TRUE, col = "red", lwd = 2)

plot(cooksd, pch = 20, main = "Cook's Distance",
     ylab = "Cook's Distance", xlab = "Observation Index")
abline(h = 4/nrow(ames_model_train_log), col = "red", lty = 2)

par(mfrow = c(2, 2))
plot(model_clean)
par(mfrow = c(1, 1))

qqnorm(residuals_clean, main = "Normal Q-Q Plot (Model 3 - Clean)")
qqline(residuals_clean, col = "red", lwd = 2)

hist(residuals_clean, breaks = 50, freq = FALSE,
     main = "Distribution of Residuals (Model 3 - Clean)",
     xlab = "Residuals", col = "lightcyan")
curve(dnorm(x, mean = mean(residuals_clean), sd = sd(residuals_clean)),
      add = TRUE, col = "red", lwd = 2)

all_predictions_final <- data.frame(
  Model1 = predictions1_test,
  Model2 = predictions2_test,
  Model3 = predictions_clean_test,
  RandomForest = rf_predictions_test
) %>%
  pivot_longer(cols = everything(), names_to = "Model", values_to = "Prediction")

all_predictions_final$Model <- factor(all_predictions_final$Model,
                                      levels = c("Model1", "Model2", "Model3", "RandomForest"),
                                      labels = c("Model 1", "Model 2", "Model 3", "Random Forest"))

ggplot(all_predictions_final, aes(x = Prediction, fill = Model)) +
  geom_histogram(alpha = 0.5, bins = 50, position = "identity") +
  scale_x_continuous(labels = scales::dollar) +
  labs(title = "Distribution of Test Set Predictions (All Models)",
       x = "Predicted Sale Price ($)",
       y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

ggplot(all_predictions_final, aes(x = Model, y = Prediction, fill = Model)) +
  geom_boxplot(alpha = 0.7) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Test Set Prediction Comparison (All Models)",
       x = "Model",
       y = "Predicted Sale Price ($)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        legend.position = "none")

comparison_scatter <- data.frame(
  Model3 = predictions_clean_test,
  RandomForest = rf_predictions_test
)

ggplot(comparison_scatter, aes(x = Model3, y = RandomForest)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Model 3 vs Random Forest: Test Set Predictions",
       x = "Model 3 Predictions ($)",
       y = "Random Forest Predictions ($)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

rf_residuals_train <- rf_train_data$sale_price - rf_predictions_train

par(mfrow = c(2, 2))

hist(residuals1, breaks = 50, freq = FALSE, 
     main = "Model 1: Original",
     xlab = "Residuals", col = "lightblue")
curve(dnorm(x, mean = mean(residuals1), sd = sd(residuals1)),
      add = TRUE, col = "red", lwd = 2)

hist(residuals2, breaks = 50, freq = FALSE,
     main = "Model 2: Log-transformed",
     xlab = "Residuals", col = "lightgreen")
curve(dnorm(x, mean = mean(residuals2), sd = sd(residuals2)),
      add = TRUE, col = "red", lwd = 2)

hist(residuals_clean, breaks = 50, freq = FALSE,
     main = "Model 3: Outliers Removed",
     xlab = "Residuals", col = "lightcyan")
curve(dnorm(x, mean = mean(residuals_clean), sd = sd(residuals_clean)),
      add = TRUE, col = "red", lwd = 2)

hist(rf_residuals_train, breaks = 50, freq = FALSE,
     main = "Random Forest",
     xlab = "Residuals", col = "lightpink")
curve(dnorm(x, mean = mean(rf_residuals_train), sd = sd(rf_residuals_train)),
      add = TRUE, col = "red", lwd = 2)

par(mfrow = c(1, 1))

varImpPlot(rf_model, main = "Variable Importance Plot")