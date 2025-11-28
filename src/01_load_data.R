# Load required libraries
library(AmesHousing)
library(dplyr)

# Define key location coordinates
downtown <- c(42.025056, -93.614126)
university <- c(42.026546, -93.646443)
airport <- c(41.998346, -93.621765)

# Load Ames housing dataset
ames <- make_ames()

# Select relevant variables
ames_selected <- ames %>%
  select(Latitude, Longitude, Gr_Liv_Area, Overall_Qual, Year_Built,
         Total_Bsmt_SF, Full_Bath, Garage_Cars, Kitchen_Qual, Sale_Price)

# Calculate Euclidean distances to key locations
ames_selected <- ames_selected %>%
  mutate(
    Downtown_Dist = sqrt((Latitude - downtown[1])^2 + (Longitude - downtown[2])^2),
    University_Dist = sqrt((Latitude - university[1])^2 + (Longitude - university[2])^2),
    Airport_Dist = sqrt((Latitude - airport[1])^2 + (Longitude - airport[2])^2)
  )

# Define encoding mappings for categorical variables
kitchen_qual_mapping <- c("Poor" = 1, "Fair" = 2, "Typical" = 3, 
                          "Good" = 4, "Excellent" = 5)

overall_qual_mapping <- c("Very_Poor" = 1, "Poor" = 2, "Fair" = 3, 
                          "Below_Average" = 4, "Average" = 5,
                          "Above_Average" = 6, "Good" = 7, "Very_Good" = 8,
                          "Excellent" = 9, "Very_Excellent" = 10)

# Encode categorical variables and standardize column names
ames_model <- ames_selected %>%
  mutate(
    kitchen_qual_encoded = as.numeric(kitchen_qual_mapping[as.character(Kitchen_Qual)]),
    overall_qual_encoded = as.numeric(overall_qual_mapping[as.character(Overall_Qual)]),
    sale_price = Sale_Price,
    gr_liv_area = Gr_Liv_Area,
    year_built = Year_Built,
    total_bsmt_sf = Total_Bsmt_SF,
    full_bath = Full_Bath,
    garage_cars = Garage_Cars,
    downtown_dist = Downtown_Dist,
    university_dist = University_Dist,
    airport_dist = Airport_Dist
  ) %>%
  select(sale_price, gr_liv_area, overall_qual_encoded, year_built,
         total_bsmt_sf, full_bath, garage_cars, kitchen_qual_encoded,
         downtown_dist, university_dist, airport_dist)

# Remove missing values
ames_model <- na.omit(ames_model)

# Print dataset information
cat("Dataset size:", nrow(ames_model), "\n")
cat("Number of variables:", ncol(ames_model), "\n")

# Split data into training (80%) and test (20%) sets
set.seed(123)  # For reproducibility
train_index <- sample(1:nrow(ames_model), size = 0.8 * nrow(ames_model))

ames_model_train <- ames_model[train_index, ]
ames_model_test <- ames_model[-train_index, ]

cat("\nTraining set size:", nrow(ames_model_train), "\n")
cat("Test set size:", nrow(ames_model_test), "\n")

# Save processed datasets
dir.create("data", showWarnings = FALSE)
saveRDS(ames_model_train, "data/ames_train.rds")
saveRDS(ames_model_test, "data/ames_test.rds")

cat("\nProcessed data saved to data/ directory\n")
