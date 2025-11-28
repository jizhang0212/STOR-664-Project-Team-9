library(dplyr)
library(ggplot2)
library(corrplot)
library(scales)

source("src/01_load_data.R")

cat("\n========== Training Set Summary Statistics ==========\n")
summary(ames_model_train)

cor_data <- ames_model_train %>%
  select(sale_price, gr_liv_area, overall_qual_encoded, year_built,
         total_bsmt_sf, full_bath, garage_cars, kitchen_qual_encoded,
         downtown_dist, university_dist, airport_dist)

cor_matrix <- cor(cor_data)
cat("\nCorrelation Matrix:\n")
print(round(cor_matrix, 3))

corrplot(cor_matrix, method = "color", type = "upper",
         addCoef.col = "black", number.cex = 0.6,
         tl.col = "black", tl.srt = 45,
         title = "Correlation Matrix of Selected Variables",
         mar = c(0,0,2,0))