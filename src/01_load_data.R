############################################
## 1. Load packages
############################################
library(tidyverse)
library(janitor)

############################################
## 2. Import and clean column names
############################################
ames <- read_csv("train.csv", na = c("", "NA")) %>%
  clean_names()    # convert to snake_case

############################################
## 3. Meaningful NA (does not exist → use "None")
############################################
meaningful_na_vars <- c(
  "alley", "bsmt_qual", "bsmt_cond", "bsmt_exposure",
  "bsmt_fin_type1", "bsmt_fin_type2", "fireplace_qu",
  "garage_type", "garage_finish", "garage_qual",
  "garage_cond", "pool_qc", "fence", "misc_feature",
  "mas_vnr_type"        # masonry veneer type NA = None
)

ames <- ames %>%
  mutate(across(all_of(meaningful_na_vars),
                ~ replace_na(.x, "None")))

############################################
## 4. Meaningful NA for numeric structural values
## (structure does not exist → set to 0)
############################################
zero_fill_vars <- c(
  "bsmt_fin_sf1", "bsmt_fin_sf2", "bsmt_unf_sf",
  "total_bsmt_sf",
  "garage_area", "garage_cars",
  "pool_area",
  "mas_vnr_area"        # veneer area NA = 0
)

ames <- ames %>%
  mutate(across(all_of(zero_fill_vars),
                ~ replace_na(.x, 0)))

############################################
## 5. GarageYrBlt meaningful NA (no garage)
############################################
ames$garage_yr_blt[is.na(ames$garage_yr_blt)] <- 0

############################################
## 6. Real Missing Values — imputation
############################################

## 6a. LotFrontage — impute with median
ames$lot_frontage[is.na(ames$lot_frontage)] <-
  median(ames$lot_frontage, na.rm = TRUE)

## 6b. Electrical — impute with mode
mode_val <- ames %>%
  filter(!is.na(electrical)) %>%
  count(electrical) %>%
  arrange(desc(n)) %>%
  slice(1) %>%
  pull(electrical)

ames$electrical <- replace_na(ames$electrical, mode_val)

############################################
## 7. Done — all meaningful NA handled,
## and real NA imputed appropriately
############################################

glimpse(ames)
sum(is.na(ames))   # Should be 0
