library(dplyr)
library(ggplot2)
library(corrplot)

# Remove ID column before correlation
train <- train %>% select(-Id)

# only numeric variables
num_vars <- train %>% select(where(is.numeric))

# correlation matrix, excluding rows with NA
cor_mat <- cor(num_vars, use = "pairwise.complete.obs")

# Simple correlation heatmap
corrplot(cor_mat, method = "color", type = "upper",
         tl.cex = 0.6, tl.col = "black")
target <- "SalePrice"
cors <- cor_mat[, target]
cors <- sort(cors, decreasing = TRUE)

# ggplot
cor_df <- data.frame(
  variable = names(cors),
  correlation = as.numeric(cors)
)

# Barplot of correlations with SalePrice
ggplot(cor_df %>% filter(variable != target),
       aes(x = reorder(variable, correlation), y = correlation)) +
  geom_col() +
  coord_flip() +
  labs(x = "Variable",
       y = paste("Correlation with", target),
       title = paste("Correlation of Numeric Predictors with", target))
