# STOR 664 Project – Ames Housing Analysis - Team 9

## Team Members
- **Joe Zhou** (@mzhouUNC)  
- **Ji Zhang** (@jizhang0212)  
- **Tiger (Jinghao) Teng** (@TigerTeng1024)

## Overview
This repository contains our complete project for **STOR 664 (Fall 2025)**. Our goal is to identify and evaluate the key determinants of **housing prices in Ames, Iowa** using the Ames Housing Dataset. In Part 2, we extend the exploratory analysis from Part 1 by incorporating spatial variables, fitting multiple regression models, conducting diagnostic assessments, and comparing model performance.

Our final analysis includes:
- Log-transformed linear regression models  
- Outlier-removed regression models  
- Multicollinearity evaluation using VIF  
- Manually collected longitude and latitude for each home  
- Computed distances to downtown Ames, Iowa State University, and the airport  
- Comparison of linear regression, lasso, and random forest  
- Full diagnostic checking and model selection  
- Discussion of limitations and future extensions  

## Repository Structure

```

src/
├── 01_load_data.R        # Data import and preprocessing
├── 02_eda.R              # Exploratory data analysis
├── 03_fit_models.R       # Model fitting and statistical analysis
├── 04_generate_figures.R # Visualization and table generation

````

| Folder | Purpose |
|--------|---------|
| `/data/raw` | Original datasets (`train.csv`, `test.csv`) |
| `/data/processed` | Cleaned datasets created during analysis |
| `/src` | R scripts used for data cleaning, EDA, and modeling |
| `/results/figures` | Plots and visualizations used in the report |
| `/reports` | Written deliverables: Part 1 and Part 2 reports |
| Root folder | Documentation including `README.md` |

## Getting Started

### 1. Clone the repository
```bash
git clone https://github.com/mzhouUNC/stor-664-project-team9
cd stor-664-project-team9/
````

### 2. Run analysis scripts

Execute the R scripts in order:

```bash
Rscript src/01_load_data.R
Rscript src/02_eda.R
Rscript src/03_fit_models.R
Rscript src/04_generate_figures.R
```

Figures used in the report are saved in:

```
/results/figures/
```

Processed datasets created during analysis are stored in:

```
/data/processed/
```

## Part 2 Summary

Part 2 builds upon the exploratory work from Part 1 by fitting and evaluating several models for predicting SalePrice and log(SalePrice). Major enhancements include the creation of spatial distance features (to downtown, the university, and the airport), improved model diagnostics, VIF-based multicollinearity checks, outlier removal using Cook’s distance, and the addition of nonlinear modeling via a random forest. These extensions produced more reliable inference and improved predictive performance.

## Reports

Final project reports are located in:

```
/reports/
```

* `Part1_Report.pdf`
* `Part2_Final_Report.pdf`

