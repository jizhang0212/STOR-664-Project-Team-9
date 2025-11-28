# Load required libraries
library(dplyr)
library(ggplot2)
library(corrplot)
library(scales)

# Load processed data
ames_model_train <- readRDS("data/ames_train.rds")
ames_model_test <- readRDS("data/ames_test.rds")

# Create results directory
dir.create("results", showWarnings = FALSE)
dir.create("results/figures", showWarnings = FALSE)

# Print summary statistics
summary(ames_model_train)

# Select numeric variables for correlation analysis
cor_data <- ames_model_train %>%
  select(sale_price, gr_liv_area, overall_qual_encoded, year_built,
         total_bsmt_sf, full_bath, garage_cars, kitchen_qual_encoded,
         downtown_dist, university_dist, airport_dist)

# Calculate correlation matrix
cor_matrix <- cor(cor_data)
cat("\nCorrelation Matrix:\n")
print(round(cor_matrix, 3))

# Save correlation matrix
write.csv(round(cor_matrix, 3), "results/correlation_matrix.csv")

# Visualize correlation matrix
png("results/figures/correlation_matrix.png", width = 800, height = 800)
corrplot(cor_matrix, method = "color", type = "upper",
         addCoef.col = "black", number.cex = 0.6,
         tl.col = "black", tl.srt = 45,
         title = "Correlation Matrix of Selected Variables",
         mar = c(0,0,2,0))
dev.off()

cat("\nCorrelation matrix saved to results/correlation_matrix.csv\n")
cat("Correlation plot saved to results/figures/correlation_matrix.png\n")
